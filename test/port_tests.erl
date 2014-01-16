-module(port_tests).
                                                                                
-include_lib("eunit/include/eunit.hrl").
                                                                                
-compile([export_all]).

eval_test() ->
    {ok, P} = erlang_v8:start_vm(),

    ?assertEqual({ok, 2}, erlang_v8:eval(P, <<"1 + 1">>)),
    ?assertEqual({ok, 5}, erlang_v8:eval(P, <<"var a = 3; a + 2;">>)),

    erlang_v8:stop_vm(P),
    ok.

call_test() ->
    {ok, P} = erlang_v8:start_vm(),

    %% sum fun
    ?assertEqual({ok, undefined},
                 erlang_v8:eval(P, <<"function sum(a, b) { return a + b }">>)),
    ?assertEqual({ok, 2}, erlang_v8:call(P, <<"sum">>, [1, 1])),
    ?assertEqual({ok, 4}, erlang_v8:call(P, <<"sum">>, [2, 2])),
    ?assertEqual({ok, <<"helloworld">>},
                 erlang_v8:call(P, <<"sum">>, [<<"hello">>, <<"world">>])),

    %% object arguments
    ?assertEqual({ok, undefined},
                 erlang_v8:eval(P, <<"function get(o) { return o.a; }">>)),
    ?assertEqual({ok, undefined}, erlang_v8:call(P, <<"get">>, [2, 2])),
    ?assertEqual({ok, 1}, erlang_v8:call(P, <<"get">>, [{[{a, 1}]}])),

    erlang_v8:stop_vm(P),
    ok.

return_type_test() ->
    {ok, P} = erlang_v8:start_vm(),

    ?assertEqual({ok, 1}, erlang_v8:eval(P, <<"1">>)),
    ?assertEqual({ok, {[{<<"a">>, 1}]}},
                 erlang_v8:eval(P, <<"var x = { a: 1 }; x">>)),
    ?assertEqual({ok, [1]}, erlang_v8:eval(P, <<"[1]">>)),
    ?assertEqual({ok, true}, erlang_v8:eval(P, <<"true">>)),
    ?assertEqual({ok, null}, erlang_v8:eval(P, <<"null">>)),
    ?assertEqual({ok, 1.1}, erlang_v8:eval(P, <<"1.1">>)),

    erlang_v8:stop_vm(P),
    ok.

nested_return_type_test() ->
    {ok, P} = erlang_v8:start_vm(),

    ?assertEqual({ok, {[
                        {<<"val">>, 1},
                        {<<"list">>, [1, 2, 3]},
                        {<<"obj">>, {[{<<"val">>, 1}]}}
                       ]}},
    erlang_v8:eval(P, <<"var x = {
        val: 1,
        list: [1, 2, 3],
        obj: {
            val: 1
        }
    }; x">>)),

    erlang_v8:stop_vm(P),
    ok.
 
error_test() ->
    {ok, P} = erlang_v8:start_vm(),

    ?assertMatch({error, <<"exception">>},
                 erlang_v8:eval(P, <<"throw 'exception';">>)),

    ?assertMatch({error, <<"ReferenceError: i_do_not_exist", _/binary>>},
                 erlang_v8:call(P, <<"i_do_not_exist">>, [])),

    erlang_v8:stop_vm(P),
    ok.

timeout_test() ->
    {ok, P} = erlang_v8:start_vm(),

    ?assertEqual({error, timeout},
                 erlang_v8:eval(P, <<"while (true) {}">>, 1)),

    erlang_v8:stop_vm(P),
    ok.

reset_test() ->
    {ok, P} = erlang_v8:start_vm(),

    ?assertEqual({ok, undefined},
                 erlang_v8:eval(P, <<"function sum(a, b) { return a + b }">>)),
    ?assertEqual({ok, 2}, erlang_v8:call(P, <<"sum">>, [1, 1])),
    erlang_v8_vm:reset(P),
    ?assertMatch({error, <<"ReferenceError: sum", _/binary>>},
                 erlang_v8:call(P, <<"sum">>, [1, 1])),

    erlang_v8:stop_vm(P),
    ok.
