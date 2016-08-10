%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%% sf_client top level supervisor.
%%% @end
%%% Created : 04. Jul 2016 17:52
%%%-------------------------------------------------------------------
-module(sf_client_sup).

-behaviour(supervisor).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, ARGS, Type), {I, {I, start_link, [ARGS]}, permanent, 5000, Type, [I]}).
-define(CHILD_WITH_FUNC(I, F, ARGS, Type), {I, {I, F, [ARGS]}, permanent, 5000, Type, [I]}).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [
        ?CHILD(sf_client_access_token_server, worker)
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
