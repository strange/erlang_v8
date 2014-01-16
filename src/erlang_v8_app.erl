-module(erlang_v8_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% Application callbacks

start(_StartType, _StartArgs) ->
    erlang_v8_sup:start_link().

stop(_State) ->
    ok.
