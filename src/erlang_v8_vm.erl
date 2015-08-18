-module(erlang_v8_vm).

-behaviour(gen_server).

-export([start/0]).
-export([start_link/1]).
-export([stop/1]).
-export([reset/1]).
-export([restart/1]).
-export([set/1]).

-export([eval/2]).
-export([eval/3]).
-export([call/3]).
-export([call/4]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(EXECUTABLE, "erlang_v8").
-define(SPAWN_OPTS, [{packet, 2}, binary]).
-define(DEFAULT_TIMEOUT, 5000).
-define(MAX_SOURCE_SIZE, 65535).

-define(OP_EVAL, 0).
-define(OP_RESET, 2).
-define(OP_SET, 3).

-record(state, {
        initial_source = [],
        port,
        monitor_pid
    }).

%% External API

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

start() ->
    gen_server:start(?MODULE, [], []).

eval(Pid, Source) ->
    eval(Pid, Source, ?DEFAULT_TIMEOUT).

eval(Pid, Source, Timeout) ->
    gen_server:call(Pid, {eval, Source, Timeout}, Timeout + 1000).

call(Pid, FunctionName, Args) ->
    call(Pid, FunctionName, Args, ?DEFAULT_TIMEOUT).

call(Pid, FunctionName, Args, Timeout) ->
    gen_server:call(Pid, {call, FunctionName, Args, Timeout}, Timeout + 1000).

set(Pid) ->
    gen_server:call(Pid, set).

reset(Pid) ->
    gen_server:call(Pid, reset).

restart(Pid) ->
    gen_server:call(Pid, restart).

stop(Pid) ->
    closed = gen_server:call(Pid, stop),
    ok.

%% Callbacks

init([Opts]) ->
    State = start_port(parse_opts(Opts)),
    {ok, State}.

handle_call({call, FunctionName, Args, Timeout}, From, State) ->
    %% TODO: call should be a special op and decoding should be done in 
    %% the cc wrapper.
    SerializedArgs = jsx:encode(Args),
    Source = <<FunctionName/binary, ".apply(null, JSON.parse('",
               SerializedArgs/binary ,"'));">>,
    handle_call({eval, Source, Timeout}, From, State);

handle_call({eval, Source, _Timeout}, _From, State)
  when size(Source) > ?MAX_SOURCE_SIZE ->
    {reply, {error, invalid_source_size}, State};

handle_call({eval, Source, Timeout}, _From, #state{port = Port} = State) ->
    case eval_js(Port, Source, Timeout) of
        {ok, Response} ->
            {reply, {ok, Response}, State};
        {error, Reason} ->
            {reply, {error, Reason}, start_port(kill_port(State))}
    end;

handle_call(set, _From, #state{port = Port} = State) ->
    Port ! {self(), {command, <<?OP_SET:8>>}},
    {reply, ok, State};

handle_call(reset, _From, #state{port = Port} = State) ->
    Port ! {self(), {command, <<?OP_RESET:8>>}},
    {reply, ok, State};

handle_call(restart, _From, State) ->
    {reply, ok, start_port(close_port(State))};

handle_call(stop, _From, State) ->
    {stop, normal, closed, State};

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    close_port(State),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

%% @doc Close port gently.
close_port(#state{monitor_pid = Pid, port = Port} = State) ->
    catch port_close(Port),
    Pid ! demonitor,
    State#state{monitor_pid = undefined, port = undefined}.

%% @doc Close port and attempt to kill the OS process.
kill_port(#state{monitor_pid = Pid, port = Port} = State) ->
    catch port_close(Port),
    Pid ! kill,
    State#state{monitor_pid = undefined, port = undefined}.

%% @doc Start port and port monitor.
start_port(#state{initial_source = Source} = State) ->
    Executable = filename:join(priv_dir(), ?EXECUTABLE),
    Opts = [{args, Source}|?SPAWN_OPTS],
    Port = open_port({spawn_executable, Executable}, Opts),
    monitor_port(State#state{port = Port}).

%% @doc Kill active port monitor before starting a new process.
monitor_port(#state{monitor_pid = Pid} = State) when is_pid(Pid) ->
    Pid ! demonitor,
    monitor_port(State#state{monitor_pid = undefined});

%% @doc Start a process that monitors the port (and parent process) and kills
%% the actual OS process when things go south.
monitor_port(#state{port = Port} = State) ->
    Parent = self(),
    Pid = spawn(fun() ->
        {os_pid, OSPid} = erlang:port_info(Port, os_pid),
        MRef = erlang:monitor(process, Parent),
        receive 
            demonitor ->
                erlang:demonitor(MRef);
            kill ->
                os_kill(OSPid);
            {'DOWN', _Ref, process, Parent, _Reason} ->
                os_kill(OSPid)
        end
    end),
    State#state{monitor_pid = Pid}.

%% @doc Kill OS process.
os_kill(OSPid) ->
    os:cmd(io_lib:format("kill -9 ~p", [OSPid])).

%% @doc Evaluate source on port
eval_js(Port, Source, Timeout) ->
    Port ! {self(), {command, <<?OP_EVAL:8, Source/binary>>}},
    receive
        {Port, {data, <<_:8, "undefined">>}} ->
            {ok, undefined};
        {Port, {data, <<0:8, Response/binary>>}} ->
            {ok, jsx:decode(Response)};
        {Port, {data, <<1:8, Response/binary>>}} ->
            [{<<"error">>, Reason}] = jsx:decode(Response),
            {error, Reason};
        {Port, Error} ->
            %% TODO: we should probably special case here.
            {error, Error}
        after Timeout ->
            {error, timeout}
    end.

%% @doc Return the path to the application's priv dir (assuming directory
%% structure is intact).
priv_dir() ->
    filename:join(filename:dirname(filename:dirname(code:which(?MODULE))),
                  "priv").

%% @doc Parse proplists/opts and populate a state record.
parse_opts(Opts) ->
    lists:foldl(fun parse_opt/2, #state{initial_source = []}, Opts).

%% @doc Append source specified in source option.
parse_opt({source, S}, #state{initial_source = InitialSource} = State) ->
    State#state{initial_source = [S|InitialSource]};

%% @doc Read contents of file option and append to state.
parse_opt({file, F}, #state{initial_source = InitialSource} = State) ->
    %% Files should probably be read in the OS process instead to prevent
    %% keeping multiple copies of the JS source code in memory.
    {ok, S} = file:read_file(F),
    State#state{initial_source = [S|InitialSource]};

%% @doc Ignore unknown options.
parse_opt(_, State) -> State.
