-module(erlang_v8_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% Supervisor callbacks

-export([init/1]).

-define(CHILD(I, Type, Args),
    {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ]}}.
