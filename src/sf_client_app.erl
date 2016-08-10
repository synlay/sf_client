%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%% sf_client public API
%%% @end
%%% Created : 04. Jul 2016 17:52
%%%-------------------------------------------------------------------
-module(sf_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    init(),
    ok.

start(_StartType, _StartArgs) ->
    sf_client_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    application:ensure_all_started(sf_client).
