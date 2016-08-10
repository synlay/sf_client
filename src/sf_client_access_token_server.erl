%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 10. Aug 2016 20:02
%%%-------------------------------------------------------------------
-module(sf_client_access_token_server).
-author("David Robakowski").

-behaviour(gen_fsm).

-define(ACCESS_TOKEN_ASSIGNED,   access_token_assigned).
-define(ACCESS_TOKEN_UNASSIGNED, access_token_unassigned).
-define(MAX_RECONNECT_ATTEMPTS,  3).

-type get_server_access_token_queue() :: queue:queue({pid(), term()}).
-type access_token() :: binary().

%% API
-export([
     start_link/1
    ,get_server_access_token/1
    ,reasign_server_access_token/1
]).

%% gen_fsm callbacks
-export([
    init/1,
    ?ACCESS_TOKEN_UNASSIGNED/3,
    ?ACCESS_TOKEN_UNASSIGNED/2,
    ?ACCESS_TOKEN_ASSIGNED/3,
    ?ACCESS_TOKEN_ASSIGNED/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-record(state, {
    access_token                  :: access_token(),
    access_token_expiry_tref      :: timer:tref(),
    get_server_access_token_queue :: get_server_access_token_queue(),
    server_app_env_name           :: atom(),
    max_reconnect_attempts        = ?MAX_RECONNECT_ATTEMPTS :: non_neg_integer(),
    attempt_error_msg             :: term()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API functions
%%%===================================================================


-spec get_server_access_token(ServerAppEnvName) -> {ok, Result} | {error, Reason} when
    ServerAppEnvName :: atom(),
    Result           :: access_token(),
    Reason           :: term().
get_server_access_token(ServerAppEnvName) ->
    case catch gen_fsm:sync_send_event(ServerAppEnvName, get_server_access_token,
             sf_client_config:get_access_token_server_request_retry_timeout() * (?MAX_RECONNECT_ATTEMPTS + 1) * 1000) of
        {'EXIT', {timeout, _}} ->
            {error, 'timeout_while_trying_to_communicate_with_auth_server'};
        Other ->
            Other
    end.


-spec reasign_server_access_token(ServerAppEnvName) -> {ok, Result} | {error, Reason} when
    ServerAppEnvName :: atom(),
    Result           :: access_token(),
    Reason           :: term().
reasign_server_access_token(ServerAppEnvName) ->
    case catch gen_fsm:sync_send_all_state_event(ServerAppEnvName, reasign_server_access_token,
             sf_client_config:get_access_token_server_request_retry_timeout() * (?MAX_RECONNECT_ATTEMPTS + 1) * 1000) of
        {'EXIT', {timeout, _}} ->
            {error, 'timeout_while_trying_to_communicate_with_auth_server'};
        Other ->
            Other
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ServerAppEnvName :: atom()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(ServerAppEnvName) ->
    gen_fsm:start_link({local, ServerAppEnvName}, ?MODULE, [ServerAppEnvName], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
init([ServerAppEnvName]) ->
    process_flag(trap_exit, true),
    {ok, access_token_unassigned, #state{
        get_server_access_token_queue = queue:new(),
        server_app_env_name = ServerAppEnvName}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
?ACCESS_TOKEN_UNASSIGNED(timeout, State) when State#state.max_reconnect_attempts > 0 ->
    case init_system() of
        {ok, AccessToken, AccessTokenExpiresTime} ->
            NewCallerQueue = notify_all_caller(State#state.get_server_access_token_queue, {ok, AccessToken}),
            NextStateHandlerFun = fun(TRef) ->
                {next_state, ?ACCESS_TOKEN_ASSIGNED, State#state{
                    access_token = AccessToken,
                    access_token_expiry_tref = TRef,
                    get_server_access_token_queue = NewCallerQueue,
                    max_reconnect_attempts = ?MAX_RECONNECT_ATTEMPTS
                }}
            end,
            TRef = if
                AccessTokenExpiresTime /= undefined ->
                    case timer:apply_after(AccessTokenExpiresTime, ?MODULE, reasign_server_access_token, []) of
                        {ok, TRefReturn} ->
                            TRefReturn;
                        {error, Reason} ->
                            lager:info("Could not create timer for reasigning access token after it expires; "
                                       "Reason: ~p", [Reason]),
                            undefined
                    end;
                true ->
                    undefined
            end,
            NextStateHandlerFun(TRef);
        {error, Reason} ->
            {next_state, ?ACCESS_TOKEN_UNASSIGNED, State#state{
                max_reconnect_attempts = State#state.max_reconnect_attempts - 1,
                attempt_error_msg = Reason
            }, sf_client_config:get_access_token_server_request_retry_timeout() * 1000}
    end;

?ACCESS_TOKEN_UNASSIGNED(timeout, State) ->
    NewCallerQueue = notify_all_caller(State#state.get_server_access_token_queue,
                                       {error, State#state.attempt_error_msg}),
    {next_state, ?ACCESS_TOKEN_UNASSIGNED, State#state{
        max_reconnect_attempts = ?MAX_RECONNECT_ATTEMPTS,
        attempt_error_msg = undefined,
        get_server_access_token_queue = NewCallerQueue
    }}.


?ACCESS_TOKEN_ASSIGNED(timeout, State) ->
    {next_state, ?ACCESS_TOKEN_UNASSIGNED, State#state{
        access_token = undefined,
        access_token_expiry_tref = cancel_timer(State),
        max_reconnect_attempts = ?MAX_RECONNECT_ATTEMPTS
    }, 0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
?ACCESS_TOKEN_UNASSIGNED(get_server_access_token, From, State) ->
    {next_state, ?ACCESS_TOKEN_UNASSIGNED, State#state{
        get_server_access_token_queue = queue:cons(From, State#state.get_server_access_token_queue),
        max_reconnect_attempts = ?MAX_RECONNECT_ATTEMPTS
    }, 0}.


?ACCESS_TOKEN_ASSIGNED(get_server_access_token, _From, State) ->
    Reply = {ok, State#state.access_token},
    {reply, Reply, ?ACCESS_TOKEN_ASSIGNED, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
handle_sync_event(reasign_server_access_token, From, _StateName, State) ->
    {next_state, ?ACCESS_TOKEN_UNASSIGNED, State#state{
        get_server_access_token_queue = queue:cons(From, State#state.get_server_access_token_queue),
        access_token = undefined,
        access_token_expiry_tref = cancel_timer(State),
        max_reconnect_attempts = ?MAX_RECONNECT_ATTEMPTS
    }, 0};


handle_sync_event(Event, From, StateName, State) ->
    lager:notice("handle_sync_event - got unkown event: ~p from: ~p...", [Event, From]),
    {next_state, StateName, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    lager:notice("handle_info - got unkown info: ~p...", [Info]),
    {next_state, StateName, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec cancel_timer(state()) -> undefined.
cancel_timer(#state{access_token_expiry_tref = TRef}) when TRef =/= undefined ->
    _ = timer:cancel(TRef),
    undefined;

cancel_timer(_State) ->
    undefined.


-spec notify_all_caller(get_server_access_token_queue(), term()) -> get_server_access_token_queue().
notify_all_caller(Queue, Reply) ->
    case queue:is_empty(Queue) of
        false ->
            _ = gen_fsm:reply(queue:head(Queue), Reply),
            notify_all_caller(queue:tail(Queue), Reply);
        true ->
            queue:new()
    end.


init_system() ->
    ExpiryDelta = 500,
    Url = restc:construct_url(sf_client_config:get_sf_rest_api_endpoint(), "/services/oauth2/token", [
        {"grant_type", "password"},
        {"client_id", sf_client_config:get_credentials_client_id()},
        {"client_secret", sf_client_config:get_credentials_client_secret()},
        {"username", sf_client_config:get_credentials_username()},
        {"password", sf_client_config:get_credentials_password()}
    ]),
    case restc:request(post, json, Url, [200]) of
        {ok, 200, _Header, Body} ->
            lager:debug("Got access token from SalesForce: ~p", [Body]),
            {ok, proplists:get_value(<<"access_token">>, Body),
                 (sf_client_config:get_access_token_expiry() - ExpiryDelta) * 1000};
        {error, _ErrorCode, _Header, Body} ->
            lager:error("Could not authorize client credentials; Reason: ~p", [Body]),
            {error, Body}
    end.
