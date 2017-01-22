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
-define(REQUEST_RETRY_TIMEOUT, 2000).

-type get_server_access_token_queue() :: queue:queue({pid(), term()}).
-type access_token() :: binary().

%% API
-export([
     start_link/0
    ,get_server_access_token/0
    ,reasign_server_access_token/0
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
    access_token                  :: access_token() | undefined,
    access_token_expiry_tref      :: timer:tref() | undefined,
    get_server_access_token_queue :: get_server_access_token_queue(),
    max_reconnect_attempts        = ?MAX_RECONNECT_ATTEMPTS :: non_neg_integer(),
    attempt_error_msg             :: term()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API functions
%%%===================================================================


-spec get_server_access_token() -> {ok, Result} | {error, Reason} when
    Result           :: access_token(),
    Reason           :: term().
get_server_access_token() ->
    case catch gen_fsm:sync_send_event(?MODULE, get_server_access_token,
                                       ?REQUEST_RETRY_TIMEOUT * (?MAX_RECONNECT_ATTEMPTS + 1)) of
        {'EXIT', {timeout, _}} ->
            {error, 'timeout_while_trying_to_communicate_with_auth_server'};
        Other ->
            Other
    end.


-spec reasign_server_access_token() -> ok.
reasign_server_access_token() ->
    gen_fsm:send_all_state_event(?MODULE, reasign_server_access_token).


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, access_token_unassigned, #state{
        get_server_access_token_queue = queue:new()
    }}.


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
                            _ = lager:info("Could not create timer for reasigning access token after it expires; "
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
            }, ?REQUEST_RETRY_TIMEOUT}
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
        get_server_access_token_queue = queue:cons(From, State#state.get_server_access_token_queue)
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
handle_event(reasign_server_access_token, StateName, State) ->
    {next_state, ?ACCESS_TOKEN_UNASSIGNED, State#state{
        access_token = undefined,
        access_token_expiry_tref = cancel_timer(State),
        max_reconnect_attempts = if
            StateName == ?ACCESS_TOKEN_ASSIGNED ->
                ?MAX_RECONNECT_ATTEMPTS;
            true ->
                State#state.max_reconnect_attempts
        end
    }, 0};

handle_event(Event, StateName, State) ->
    _ = lager:notice("handle_event - got unkown event: ~p in state: ~p from: ~p...", [Event, StateName]),
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
handle_sync_event(Event, From, StateName, State) ->
    _ = lager:notice("handle_sync_event - got unkown event: ~p in state: ~p from: ~p...", [Event, StateName, From]),
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
    _ = lager:notice("handle_info - got unkown info: ~p...", [Info]),
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
    Url = restc:construct_url(binary_to_list(sf_client_config:get_sf_rest_api_endpoint()), "/services/oauth2/token", [
        {"grant_type", "password"},
        {"client_id", sf_client_config:get_credentials_client_id()},
        {"client_secret", sf_client_config:get_credentials_client_secret()},
        {"username", sf_client_config:get_credentials_username()},
        {"password", sf_client_config:get_credentials_password()}
    ]),
    case sf_client_lib:request([], post, 200, Url, false, false) of
        {ok, Body} ->
            _ = lager:debug("Got new access token from SalesForce"),
            {ok, st_traverse_utils:traverse_by_path(<<"access_token">>, Body),
                 (sf_client_config:get_access_token_expiry() - ExpiryDelta) * 1000};
        {error, Reason}=Err ->
            _ = lager:error("Could not authorize client credentials; Reason: ~p", [Reason]),
            Err
    end.
