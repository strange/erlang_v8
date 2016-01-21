-module(erlang_v8).

-export([start_vm/0]).
-export([start_vm/1]).
-export([stop_vm/1]).

-export([reset_vm/2]).
-export([restart_vm/1]).

-export([eval/3]).
-export([eval/4]).
-export([call/4]).
-export([call/5]).

-export([t/0]).

t() ->
    application:ensure_all_started(erlang_v8),
    {ok, VM} = start_vm(),
        [begin
        {ok, Context} = erlang_v8_vm:create_context(VM),
        R = erlang_v8:eval(VM, Context, <<"1 + 1;">>),
        ok = erlang_v8_vm:destroy_context(VM, Context),
        R
         end || _ <- lists:seq(1, 10000)].

start_vm() ->
    start_vm([]).

start_vm(Opts) ->
    erlang_v8_vm:start_link(Opts).

stop_vm(Pid) ->
    erlang_v8_vm:stop(Pid).

reset_vm(Pid, Context) ->
    erlang_v8_vm:reset(Pid, Context).

restart_vm(Pid) ->
    erlang_v8_vm:restart(Pid).

eval(Pid, Context, Source) ->
    erlang_v8_vm:eval(Pid, Context, Source).

eval(Pid, Context, Source, Timeout) ->
    erlang_v8_vm:eval(Pid, Context, Source, Timeout).

call(Pid, Context, FunctionName, Args) ->
    erlang_v8_vm:call(Pid, Context, FunctionName, Args).

call(Pid, Context, FunctionName, Args, Timeout) ->
    erlang_v8_vm:call(Pid, Context, FunctionName, Args, Timeout).
