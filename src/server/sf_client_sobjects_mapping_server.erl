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

-behaviour(gen_server).

-include_lib("st_commons/include/st_commons.hrl").

%% API
-export([
     start_link/0
    ,get_sobjects_mapping/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?MODULE, [public, named_table, {read_concurrency, true}]),
    _ = init_sf_mappings(5),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    true = ets:delete(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


undefined_lift(undefined, Monad, Message) -> Monad:fail(Message);

undefined_lift(Value, Monad, _Message) -> Monad:return(Value).


init_sf_mappings(Retries) when Retries > 0 ->

    SObjectsMapping = maps:fold(fun(Module, ModelIdentifier, Acc) ->
        SObjectTableName = Module:sobject_table_name(),
        Acc#{SObjectTableName => [{Module, ModelIdentifier} | maps:get(SObjectTableName, Acc, [])]}
    end, #{}, sf_client_config:get_sobjects_mapping()),

    ApiEndpoint = sf_client_config:get_sf_rest_api_endpoint(),
    ApiVersionPath = sf_client_config:get_sf_rest_api_version_path(),

    {ok, AccessToken} = sf_client_access_token_server:get_server_access_token(),
    Url = restc:construct_url(binary_to_list(<<ApiEndpoint/binary,  ApiVersionPath/binary>>), "/sobjects", []),

    case restc:request(get, json, Url, [200], [{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}]) of
        {ok, 200, _Header, Body} ->
            {ok, ProcessedSObjectsMapping} = lists:foldl(fun(SObject, {ok, Acc}) ->
                ?maybe_get_default(do([error_m ||
                    Name <- undefined_lift(st_traverse_utils:traverse_by_path(<<"name">>, SObject), error_m,
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
            end, {ok, SObjectsMapping}, st_traverse_utils:traverse_by_path(<<"sobjects">>, Body)),

            UnprocessedSObjectsCount = maps:size(ProcessedSObjectsMapping),
            if
                UnprocessedSObjectsCount > 0 ->
                    lager:warning("For some mappings were no corresponding sobjects found: ~p",
                                  [ProcessedSObjectsMapping]);
                true ->
                    ok
            end;
        {error, 401, _Header, _Body} ->
            Timeout = st_math_lib:ceiling(2000 * rand:uniform() + 1000),
            lager:debug("Token seems to be unauthorized or expired, retrying the request after: ~p ms; "
                        "Left retries: ~p", [Timeout, Retries]),
            timer:sleep(Timeout),
            init_sf_mappings(Retries - 1);
        {error, _ErrorCode, _Header, Body} ->
            lager:error("Could not authorize client credentials; Reason: ~p", [Body]),
            {error, Body}
    end;

init_sf_mappings(_) ->
    lager:error("SalesForce model mapping initialization process failed: Maximal retries reached"),
    {error, max_retries}.
