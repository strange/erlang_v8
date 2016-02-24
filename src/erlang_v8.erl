-module(erlang_v8).

-export([start_vm/0]).
-export([start_vm/1]).
-export([stop_vm/1]).

-export([restart_vm/1]).

-export([create_context/1]).
-export([destroy_context/2]).

-export([eval/3]).
-export([eval/4]).
-export([call/4]).
-export([call/5]).

start_vm() ->
    start_vm([]).

start_vm(Opts) ->
    erlang_v8_vm:start_link(Opts).

stop_vm(Pid) ->
    erlang_v8_vm:stop(Pid).

restart_vm(Pid) ->
    erlang_v8_vm:restart(Pid).

create_context(VM) ->
    erlang_v8_vm:create_context(VM).

destroy_context(VM, Context) ->
    erlang_v8_vm:destroy_context(VM, Context).

eval(Pid, Context, Source) ->
    erlang_v8_vm:eval(Pid, Context, Source).

eval(Pid, Context, Source, Timeout) ->
    erlang_v8_vm:eval(Pid, Context, Source, Timeout).

call(Pid, Context, FunctionName, Args) ->
    erlang_v8_vm:call(Pid, Context, FunctionName, Args).

call(Pid, Context, FunctionName, Args, Timeout) ->
    erlang_v8_vm:call(Pid, Context, FunctionName, Args, Timeout).
