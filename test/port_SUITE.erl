-module(port_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([eval/1]).
-export([call/1]).
-export([return_type/1]).
-export([nested_return_type/1]).
-export([errors/1]).
-export([timeout/1]).
-export([contexts/1]).
-export([init_from_source/1]).
-export([init_from_file/1]).
-export([multiple_vms/1]).
-export([big_input/1]).
-export([performance/1]).
-export([dead_proc/1]).
-export([escaped_control_characters/1]).

%% Callbacks

all() ->
    [
        eval,
        call,
        return_type,
        timeout,
        nested_return_type,
        errors,
        contexts,
        init_from_source,
        init_from_file,
        multiple_vms,
        performance,
        big_input,
        dead_proc,
        escaped_control_characters
    ].

init_per_suite(Config) ->
    application:start(jsx),
    application:start(erlang_v8),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

eval(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, Context} = erlang_v8:create_context(P),

    {ok, 2} = erlang_v8:eval(P, Context, <<"1 + 1">>),
    {ok, 5} = erlang_v8:eval(P, Context, <<"var a = 3; a + 2;">>),

    ok = erlang_v8_vm:destroy_context(P, Context),

    erlang_v8:stop_vm(P),
    ok.

call(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, Context} = erlang_v8:create_context(P),
    
    {ok, undefined} = erlang_v8:eval(P, Context, <<"function sum(a, b) { return a + b; }">>),
    {ok, 3} = erlang_v8:call(P, Context, <<"sum">>, [1, 2]),

    {ok, 4} = erlang_v8:call(P, Context, <<"sum">>, [2, 2]),
    {ok, <<"helloworld">>} =
        erlang_v8:call(P, Context, <<"sum">>, [<<"hello">>, <<"world">>]),

    %% a few arguments
    {ok, undefined} =
        erlang_v8:eval(P, Context, <<"function mul(a, b, c, d) { return a * b * c * d }">>),
    {ok, 1} = erlang_v8:call(P, Context, <<"mul">>, [1, 1, 1, 1]),

    %% object arguments
    {ok, undefined} =
        erlang_v8:eval(P, Context, <<"function get(o) { return o.a; }">>),
    {ok, undefined} = erlang_v8:call(P, Context, <<"get">>, [2, 2]),
    {ok, 1} = erlang_v8:call(P, Context, <<"get">>, [[{a, 1}]]),

    %% object fun
    {ok, undefined} =
        erlang_v8:eval(P, Context, <<"var x = { y: function z() { return 1; } }">>),
    {ok, 1} = erlang_v8:call(P, Context, <<"x.y">>, []),

    {ok, undefined} =
        erlang_v8:eval(P, Context, <<"var x = { y: function z() { return 1; } }">>),
    {ok, 1} = erlang_v8:call(P, Context, <<"x.y">>, []),

    ok = erlang_v8_vm:destroy_context(P, Context),

    erlang_v8:stop_vm(P),
    ok.

return_type(_Config) ->
    {ok, P} = erlang_v8:start_vm(),
    {ok, Context} = erlang_v8:create_context(P),

    {ok, 1} = erlang_v8:eval(P, Context, <<"1">>),
    {ok, #{ <<"a">> := 1 }} = erlang_v8:eval(P, Context, <<"x = { a: 1 }; x">>),
    {ok, [1]} = erlang_v8:eval(P, Context, <<"[1]">>),
    {ok, true} = erlang_v8:eval(P, Context, <<"true">>),
    {ok, null} = erlang_v8:eval(P, Context, <<"null">>),
    {ok, 1.1} = erlang_v8:eval(P, Context, <<"1.1">>),

    ok = erlang_v8_vm:destroy_context(P, Context),
    erlang_v8:stop_vm(P),
    ok.

timeout(_Config) ->
    {ok, VM} = erlang_v8:start_vm(),

    {ok, Context1} = erlang_v8:create_context(VM),
    {ok, Context2} = erlang_v8:create_context(VM),

    {ok, undefined} = erlang_v8:eval(VM, Context1, <<"var x = 1;">>),
    {ok, 1} = erlang_v8:eval(VM, Context1, <<"x">>),

    {error, timeout} = erlang_v8:eval(VM, Context2, <<"while (true) {}">>, 500),

    {ok, 1} = erlang_v8:eval(VM, Context1, <<"x">>),

    ok = erlang_v8_vm:destroy_context(VM, Context1),
    ok = erlang_v8_vm:destroy_context(VM, Context2),

    erlang_v8:stop_vm(VM),
    ok.


nested_return_type(_Config) ->
    {ok, VM} = erlang_v8:start_vm(),

    {ok, Context} = erlang_v8:create_context(VM),

    {ok, #{
           <<"val">> := 1,
           <<"list">> := [1, 2, 3],
           <<"obj">> := #{ <<"val">> := 1 }
    }} = erlang_v8:eval(VM, Context, <<"
    var x = {
        val: 1,
        list: [1, 2, 3],
        obj: {
            val: 1
        }
    };
    x
    ">>),

    ok = erlang_v8_vm:destroy_context(VM, Context),

    erlang_v8:stop_vm(VM),
    ok.
 
errors(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, Context} = erlang_v8:create_context(P),

    {error, <<"exception">>} = erlang_v8:eval(P, Context, <<"throw 'exception';">>),
    {error, <<"ReferenceError: i_do_not_exist is not defined", _/binary>>} =
        erlang_v8:eval(P, Context, <<"i_do_not_exist();">>),

    ok = erlang_v8_vm:destroy_context(P, Context),

    erlang_v8:stop_vm(P),
    ok.

contexts(_Config) ->
    {ok, P} = erlang_v8:start_vm([{source, <<"var erlang_v8 = 'yes';">>}]),

    {ok, Context1} = erlang_v8_vm:create_context(P),
    {ok, Context2} = erlang_v8_vm:create_context(P),

    {ok, <<"yes">>} = erlang_v8:eval(P, Context1, <<"erlang_v8">>),

    {ok, <<"no">>} = erlang_v8:eval(P, Context1, <<"erlang_v8 = 'no';">>),
    {ok, <<"no">>} = erlang_v8:eval(P, Context1, <<"erlang_v8">>),
    {ok, <<"yes">>} = erlang_v8:eval(P, Context2, <<"erlang_v8">>),

    ok = erlang_v8_vm:destroy_context(P, Context1),
    ok = erlang_v8_vm:destroy_context(P, Context2),

    {error, invalid_context} = erlang_v8:eval(P, Context1, <<"erlang_v8">>),

    erlang_v8:stop_vm(P),

    ok.

init_from_source(_Config) ->
    {ok, VM} = erlang_v8:start_vm([{source, <<"var erlang_v8 = 'yes';">>}]),
    {ok, Context} = erlang_v8_vm:create_context(VM),

    {ok, <<"yes">>} = erlang_v8:eval(VM, Context, <<"erlang_v8">>),
    {ok, <<"no">>} = erlang_v8:eval(VM, Context, <<"erlang_v8 = 'no'">>),
    {ok, <<"no">>} = erlang_v8:eval(VM, Context, <<"erlang_v8">>),

    {ok, Context2} = erlang_v8_vm:create_context(VM),
    {ok, <<"yes">>} = erlang_v8:eval(VM, Context2, <<"erlang_v8">>),

    erlang_v8_vm:destroy_context(VM, Context),
    erlang_v8_vm:destroy_context(VM, Context2),

    erlang_v8:stop_vm(VM),
    ok.

init_from_file(_Config) ->
    Directory = filename:dirname(code:which(?MODULE)),
    Path = filename:join(Directory, "js/variables.js"),
    {ok, VM} = erlang_v8:start_vm([{file, Path}]),
    {ok, Context} = erlang_v8_vm:create_context(VM),

    {ok, 3} = erlang_v8:eval(VM, Context, <<"z;">>),

    erlang_v8_vm:destroy_context(VM, Context),

    erlang_v8:stop_vm(VM),
    ok.

multiple_vms(_Config) ->
    {ok, VM1} = erlang_v8:start_vm(),
    {ok, VM2} = erlang_v8:start_vm(),

    {ok, Context1} = erlang_v8_vm:create_context(VM1),
    {ok, Context2} = erlang_v8_vm:create_context(VM2),

    {ok, undefined} = erlang_v8:eval(VM1, Context1, <<"var x = 1;">>),
    {ok, undefined} = erlang_v8:eval(VM2, Context2, <<"var x = 2;">>),

    {ok, 1} = erlang_v8:eval(VM1, Context1, <<"x;">>),
    {ok, 2} = erlang_v8:eval(VM2, Context2, <<"x;">>),

    erlang_v8_vm:destroy_context(VM1, Context1),
    erlang_v8_vm:destroy_context(VM2, Context2),

    ok = erlang_v8:stop_vm(VM1),
    ok = erlang_v8:stop_vm(VM2),

    ok.

big_input(_Config) ->
    {ok, VM} = erlang_v8:start_vm([{max_source_size, 5000}]),

    {ok, Context} = erlang_v8_vm:create_context(VM),

    {ok, undefined} = erlang_v8:eval(VM, Context, <<"function call(arg) {
        return arg;
    }">>),

    {error, invalid_source_size} = erlang_v8:call(VM, Context, <<"call">>,
                                                  [random_bytes(5000)]),

    {ok, _} = erlang_v8:call(VM, Context, <<"call">>, [random_bytes(500)]),

    ok = erlang_v8_vm:destroy_context(VM, Context),

    ok = erlang_v8:stop_vm(VM),
    ok.

escaped_control_characters(_Config) ->
    {ok, VM} = erlang_v8:start_vm(),
    {ok, Context} = erlang_v8_vm:create_context(VM),

    {ok, undefined} = erlang_v8:eval(VM, Context, <<"function call(arg) {
        return arg;
    }">>),
    Bytes = <<"\ntestar\nfestar\n">>,
    {ok, <<"\ntestar\nfestar\n">>} = erlang_v8:call(VM, Context, <<"call">>,
                                                    [Bytes]),

    ok = erlang_v8_vm:destroy_context(VM, Context),
    ok = erlang_v8:stop_vm(VM),
    ok.

performance(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, Context} = erlang_v8_vm:create_context(P),
    {ok, undefined} = erlang_v8:eval(P, Context,
                                     <<"function retval(i) { return i; }">>),

    [{ok, I} = erlang_v8:call(P, Context, <<"retval">>, [I]) ||
     I <- lists:seq(0, 999)],

    ok = erlang_v8_vm:destroy_context(P, Context),

    erlang_v8:stop_vm(P),
    ok.

dead_proc(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    Parent = self(),
    spawn(fun() ->
        {ok, Context} = erlang_v8:create_context(P),
        Parent ! Context
    end),
    receive
        Context ->
            {error, invalid_context} = erlang_v8_vm:destroy_context(P, Context)
    end,

    spawn(fun() ->
        {ok, Context2} = erlang_v8:create_context(P),
        ok = erlang_v8_vm:destroy_context(P, Context2),
        Parent ! Context2
    end),
    receive
        Context2 ->
            {error, invalid_context} = erlang_v8_vm:destroy_context(P, Context2)
    end,

    erlang_v8:stop_vm(P),
    ok.

%% Helpers

random_bytes(N) ->
    list_to_binary([rand:uniform(26) + 96 || _ <- lists:seq(0, N - 1)]).
