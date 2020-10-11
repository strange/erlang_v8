%% Copyright (c) 2016-2020, Gustaf Sjoberg <gsjoberg@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
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
-export([compile_module/4]).

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

compile_module(Pid, Context, Name, Source) ->
    erlang_v8_vm:compile_module(Pid, Context, Name, Source).
