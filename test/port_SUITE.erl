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
-export([reset/1]).
-export([restart/1]).

%% Callbacks

all() ->
    [
        eval,
        call,
        return_type,
        nested_return_type,
        errors,
        timeout,
        reset,
        restart
    ].

init_per_suite(Config) ->
    application:start(jiffy),
    application:start(erlang_v8),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

eval(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, 2} = erlang_v8:eval(P, <<"1 + 1">>),
    {ok, 5} = erlang_v8:eval(P, <<"var a = 3; a + 2;">>),

    erlang_v8:stop_vm(P),
    ok.

call(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    %% sum fun
    {ok, undefined} =
        erlang_v8:eval(P, <<"function sum(a, b) { return a + b }">>),
    {ok, 2} = erlang_v8:call(P, <<"sum">>, [1, 1]),
    {ok, 4} = erlang_v8:call(P, <<"sum">>, [2, 2]),
    {ok, <<"helloworld">>} =
        erlang_v8:call(P, <<"sum">>, [<<"hello">>, <<"world">>]),

    %% object arguments
    {ok, undefined} =
        erlang_v8:eval(P, <<"function get(o) { return o.a; }">>),
    {ok, undefined} = erlang_v8:call(P, <<"get">>, [2, 2]),
    {ok, 1} = erlang_v8:call(P, <<"get">>, [{[{a, 1}]}]),

    erlang_v8:stop_vm(P),
    ok.

return_type(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, 1} = erlang_v8:eval(P, <<"1">>),
    {ok, {[{<<"a">>, 1}]}} = erlang_v8:eval(P, <<"var x = { a: 1 }; x">>),
    {ok, [1]} = erlang_v8:eval(P, <<"[1]">>),
    {ok, true} = erlang_v8:eval(P, <<"true">>),
    {ok, null} = erlang_v8:eval(P, <<"null">>),
    {ok, 1.1} = erlang_v8:eval(P, <<"1.1">>),

    erlang_v8:stop_vm(P),
    ok.

nested_return_type(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, {[
           {<<"val">>, 1},
           {<<"list">>, [1, 2, 3]},
           {<<"obj">>, {[{<<"val">>, 1}]}}
    ]}} = erlang_v8:eval(P, <<"
    var x = {
        val: 1,
        list: [1, 2, 3],
        obj: {
            val: 1
        }
    };
    x
    ">>),

    erlang_v8:stop_vm(P),
    ok.
 
errors(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {error, <<"exception">>} = erlang_v8:eval(P, <<"throw 'exception';">>),

    {error, <<"ReferenceError: i_do_not_exist", _/binary>>} =
        erlang_v8:call(P, <<"i_do_not_exist">>, []),

    erlang_v8:stop_vm(P),
    ok.

timeout(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {error, timeout} = erlang_v8:eval(P, <<"while (true) {}">>, 1),

    erlang_v8:stop_vm(P),
    ok.

reset(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, <<"yes">>} = erlang_v8:eval(P, <<"erlang_v8">>),
    erlang_v8:reset_vm(P),
    {ok, <<"yes">>} = erlang_v8:eval(P, <<"erlang_v8">>),

    {ok, <<"no">>} = erlang_v8:eval(P, <<"erlang_v8 = 'no';">>),
    erlang_v8:reset_vm(P),
    {ok, <<"yes">>} = erlang_v8:eval(P, <<"erlang_v8">>),

    {ok, <<"test">>} = erlang_v8:eval(P, <<"String.imposter = 'test';">>),
    erlang_v8:reset_vm(P),
    {ok, undefined} = erlang_v8:eval(P, <<"String.imposter">>),

    {ok, undefined} =
        erlang_v8:eval(P, <<"function sum(a, b) { return a + b }">>),
    {ok, 2} = erlang_v8:call(P, <<"sum">>, [1, 1]),

    erlang_v8:reset_vm(P),

    {error, <<"ReferenceError: sum", _/binary>>} =
        erlang_v8:call(P, <<"sum">>, [1, 1]),

    erlang_v8:stop_vm(P),
    ok.

restart(_Config) ->
    {ok, P} = erlang_v8:start_vm(),

    {ok, <<"yes">>} = erlang_v8:eval(P, <<"erlang_v8">>),
    erlang_v8:restart_vm(P),
    {ok, <<"yes">>} = erlang_v8:eval(P, <<"erlang_v8">>),

    {ok, <<"no">>} = erlang_v8:eval(P, <<"erlang_v8 = 'no';">>),
    erlang_v8:restart_vm(P),
    {ok, <<"yes">>} = erlang_v8:eval(P, <<"erlang_v8">>),

    {ok, <<"test">>} = erlang_v8:eval(P, <<"String.imposter = 'test';">>),
    erlang_v8:restart_vm(P),
    {ok, undefined} = erlang_v8:eval(P, <<"String.imposter">>),

    {ok, undefined} =
        erlang_v8:eval(P, <<"function sum(a, b) { return a + b }">>),
    {ok, 2} = erlang_v8:call(P, <<"sum">>, [1, 1]),

    erlang_v8:restart_vm(P),

    {error, <<"ReferenceError: sum", _/binary>>} =
        erlang_v8:call(P, <<"sum">>, [1, 1]),

    erlang_v8:stop_vm(P),
    ok.
