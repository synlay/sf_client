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
         st_supervisor_lib:new_convenient_child_spec(sf_client_access_token_server,
                                                     sf_client_access_token_server, [], permanent, 5000, worker)
        ,st_supervisor_lib:new_convenient_child_spec(sf_client_sobjects_mapping_server,
                                                     sf_client_sobjects_mapping_server, [], permanent, 5000, worker)
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
