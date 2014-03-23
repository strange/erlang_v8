-module(erlang_v8).

-export([start_vm/0]).
-export([stop_vm/1]).

-export([reset_vm/1]).
-export([restart_vm/1]).

-export([eval/2]).
-export([eval/3]).
-export([call/3]).
-export([call/4]).

start_vm() ->
    erlang_v8_vm:start_link().

stop_vm(Pid) ->
    erlang_v8_vm:stop(Pid).

reset_vm(Pid) ->
    erlang_v8_vm:reset(Pid).

restart_vm(Pid) ->
    erlang_v8_vm:restart(Pid).

eval(Pid, Source) ->
    erlang_v8_vm:eval(Pid, Source).

eval(Pid, Source, Timeout) ->
    erlang_v8_vm:eval(Pid, Source, Timeout).

call(Pid, FunctionName, Args) ->
    erlang_v8_vm:call(Pid, FunctionName, Args).

call(Pid, FunctionName, Args, Timeout) ->
    erlang_v8_vm:call(Pid, FunctionName, Args, Timeout).
