-module(proxylocal_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, F, Type, Args), {I, {I, F, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(client, start, worker, [proxylocal:get_var(client, [{port, 8282}])]),
                                 ?CHILD(proxy, start, worker, [proxylocal:get_var(http, [{port, 8080}])])]}}.
