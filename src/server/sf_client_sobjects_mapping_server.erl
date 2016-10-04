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
    %% Spawn a separate process for the initialization so the library won't crash and calls to the
    %% API will return `mapping_not_found` until the initialization successfully completes
    _ = spawn(fun() -> init_sf_mappings() end),
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


init_sf_mappings() ->

    SObjectsMapping = maps:fold(fun(ModelIdentifier, Module, Acc) ->
        SObjectTableName = Module:sobject_table_name(),
        Acc#{SObjectTableName => [{Module, ModelIdentifier} | maps:get(SObjectTableName, Acc, [])]}
    end, #{}, sf_client_config:get_sobjects_mapping()),

    ApiEndpoint = sf_client_config:get_sf_rest_api_endpoint(),

    Return =
    do([error_m ||
        ApiVersionPath <- sf_client_config:get_sf_rest_api_version_path(),

        Url = restc:construct_url(binary_to_list(ApiEndpoint), binary_to_list(ApiVersionPath), []),

        Response <- sf_client_lib:request(get, 200, Url),
        SObjectsPath <- sf_client_lib:undefined_lift(st_traverse_utils:traverse_by_path(<<"sobjects">>, Response),
                                                     error_m, no_name_attribute_found),
        SObjectsUrl = restc:construct_url(binary_to_list(ApiEndpoint), binary_to_list(SObjectsPath), []),
        SObjectsResponse <- sf_client_lib:request(get, 200, SObjectsUrl),
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
            ok;
        {error, Reason} ->
            _ = lager:error("Unable to initialize sobjects mapping; Reason: ~p", [Reason]),
            timer:sleep(2000),
            init_sf_mappings()
    end.
