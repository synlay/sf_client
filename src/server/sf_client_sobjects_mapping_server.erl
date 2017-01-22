%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 10. Aug 2016 23:20
%%%-------------------------------------------------------------------
-module(sf_client_sobjects_mapping_server).
-author("David Robakowski").

-compile({parse_transform, do}).

-define(UNCONFIGURED, unconfigured).
-define(CONFIGURED, configured).
-define(MAX_RETRIES, 5).
-define(REQUEST_RETRY_TIMEOUT, 2000).

-behaviour(gen_fsm).

-include_lib("st_commons/include/st_commons.hrl").

%% API
-export([
     start_link/0
    ,get_sobjects_mapping/1
    ,reinitialize_sf_mapping/0
]).

-export([
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================


get_sobjects_mapping(MappingKey) ->
    case ets:lookup(?MODULE, MappingKey) of
        [] ->
            error_m:fail(mapping_not_found);
        [{MappingKey, Value}] ->
            error_m:return(Value)
    end.

%%-spec reinitialize_sf_mapping() -> {ok, Result} | {error, Reason} when
%%    Result           :: access_token(),
%%    Reason           :: term().
reinitialize_sf_mapping() ->
    case catch gen_fsm:sync_send_all_state_event(?MODULE, reinitialize, ?REQUEST_RETRY_TIMEOUT * (?MAX_RETRIES + 1)) of
        {'EXIT', {timeout, _}} ->
            {error, 'timeout_while_trying_to_communicate_with_sf_mapping_server'};
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
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
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
    _ = ets:new(?MODULE, [public, named_table, {read_concurrency, true}]),
    _ = spawn(fun() -> catch gen_fsm:sync_send_all_state_event(?MODULE, reinitialize) end),
    {ok, ?UNCONFIGURED, #state{}}.


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
handle_sync_event(reinitialize, _From, _StateName, State) ->
    case init_sf_mappings(?MAX_RETRIES) of
        ok ->
            {reply, ok, ?CONFIGURED, State};
        {error, _Reason}=Err ->
            {reply, Err, ?UNCONFIGURED, State}
    end;


handle_sync_event(Event, From, StateName, State) ->
    _ = lager:notice("handle_sync_event - got unkown event: ~p from: ~p...", [Event, From]),
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
    true = ets:delete(?MODULE),
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


init_sf_mappings(MaxRetries) when MaxRetries =< 0 ->
    error_m:fail(max_retries);

init_sf_mappings(MaxRetries) ->

    SObjectsMapping = maps:fold(fun(ModelIdentifier, Module, Acc) ->
        SObjectTableName = Module:sobject_table_name(),
        Acc#{SObjectTableName => [{Module, ModelIdentifier} | maps:get(SObjectTableName, Acc, [])]}
    end, #{}, sf_client_config:get_sobjects_mapping()),

    ApiEndpoint = sf_client_config:get_sf_rest_api_endpoint(),

    Return =
    do([error_m ||
        ApiVersionPath <- sf_client_config:get_sf_rest_api_version_path(),

        Url = restc:construct_url(binary_to_list(ApiEndpoint), binary_to_list(ApiVersionPath), []),

        Response <- sf_client_lib:request(get, 200, Url, true, false),
        SObjectsPath <- sf_client_lib:undefined_lift(st_traverse_utils:traverse_by_path(<<"sobjects">>, Response),
                                                     error_m, no_name_attribute_found),
        SObjectsUrl = restc:construct_url(binary_to_list(ApiEndpoint), binary_to_list(SObjectsPath), []),
        SObjectsResponse <- sf_client_lib:request(get, 200, SObjectsUrl, true, false),
        SObjects <- sf_client_lib:undefined_lift(st_traverse_utils:traverse_by_path(<<"sobjects">>, SObjectsResponse),
                                                 error_m, no_sobjects_attribute_found),
        ProcessedSObjectsMapping <- lists:foldl(fun(SObject, {ok, Acc}) ->
            ?maybe_get_default(do([error_m ||
                Name <- sf_client_lib:undefined_lift(st_traverse_utils:traverse_by_path(<<"name">>, SObject), error_m,
                                                     no_name_attribute_found),
                monad_plus:guard(error_m, maps:is_key(Name, Acc)),
                PathValue = st_traverse_utils:traverse_by_path(<<"urls.sobject">>, SObject),
                _ = lager:debug("Found mapping URL: '~ts' for sobject: '~ts'", [PathValue, Name]),

                lists:foreach(fun({Module, ModelIdentifier}) ->
                    ets:insert(?MODULE, {ModelIdentifier,
                                         sf_client_sobjects_mapping:new(Module,
                                                                        <<ApiEndpoint/binary, PathValue/binary>>)})
                end, maps:get(Name, Acc)),
                return(maps:remove(Name, Acc))
            ]), {error, _}, {ok, Acc})
        end, {ok, SObjectsMapping}, SObjects),

        UnprocessedSObjectsCount = maps:size(ProcessedSObjectsMapping),
        if
            UnprocessedSObjectsCount > 0 ->
                lager:warning("For some mappings were no corresponding sobjects found: ~p", [ProcessedSObjectsMapping]);
            true ->
                ok
        end
    ]),
    case Return of
        ok ->
            error_m:return(ok);
        {error, Reason} ->
            _ = lager:error("Unable to initialize sobjects mapping; Reason: ~p", [Reason]),
            timer:sleep(?REQUEST_RETRY_TIMEOUT),
            init_sf_mappings(MaxRetries - 1)
    end.
