%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, David Robakowski & Synlay Technologies UG & Co. KG
%%% @doc
%%% Salesforce REST API client for Erlang
%%% @end
%%% Created : 11. Aug 2016 18:55
%%%-------------------------------------------------------------------
-module(sf_client).
-author("David Robakowski").

-compile({parse_transform, do}).

-type mapping_key() :: atom().
-type model() :: any().
-type sf_sobject_id() :: binary().

%% API
-export([
     reinitialize_client/0
    ,create/2
    ,update/3
    ,delete/2
    ,get_sobject_id_by_model/2
]).

-export_type([
     mapping_key/0
    ,model/0
    ,sf_sobject_id/0
]).

%% @doc Reinitialize the client.
%%
%% Reinitializes the client by reseting the configuration environment, get new credentials and reinitialize the intern
%% route mappings to the Salefoce endpoint. Returns `ok'.
-spec reinitialize_client() -> ok.
reinitialize_client() ->
    _ = sf_client_config:init(),
    _ = sf_client_access_token_server:reasign_server_access_token(),
    _ = sf_client_sobjects_mapping_server:reinitialize_sf_mapping(),
    ok.


%% @doc Creates a new `sObject'.
%%
%% Creates a new `sObject' for the data model `Model' using the concrete behaviour which is associated with
%% the model type `MappingKey'. Returns the created `sObject' ID if successful, otherwise an error.
-spec create(MappingKey :: mapping_key(), Model :: model()) -> {ok, Id :: sf_sobject_id()} | {error, Reason :: any()}.
create(MappingKey, Model) ->
    do([error_m ||
        {MappingModule, MappingUrl} <- get_model_mapping_config(MappingKey),
        Response <- sf_client_lib:request(MappingModule:new_sobject_from_model(Model), post, 201, MappingUrl),
        Id <- sf_client_lib:undefined_lift(st_traverse_utils:traverse_by_path(<<"id">>, Response), error_m,
                                           no_id_attribute_found),
        _ = lager:notice("Successfully created a sobject with ID: '~ts' for model: '~s'",
                         [Id, MappingModule:to_string(Model)]),
        return(Id)
    ]).


%% @doc Updates an existing `sObject'.
%%
%% Updates an existing `sObject' with the id `SObjectId' using the data model `Model', which is associated through
%% the model type `MappingKey'. Returns `ok' if successful, otherwise an error.
-spec update(MappingKey :: mapping_key(), SObjectId :: sf_sobject_id(), Model :: model()) -> ok | {error, Reason :: any()}.
update(MappingKey, SObjectId, Model) ->
    do([error_m ||
        {MappingModule, MappingUrl} <- get_model_mapping_config(MappingKey),
        SObjectUrl = restc:construct_url(binary_to_list(MappingUrl), binary_to_list(SObjectId), []),
        _ <- sf_client_lib:request(MappingModule:new_sobject_from_model(Model), patch, 204, SObjectUrl),
        lager:notice("Successfully updated sobject with ID: '~ts' for model: '~s'",
                     [SObjectId, MappingModule:to_string(Model)])
    ]).


%% @doc Deletes an existing `sObject'.
%%
%% Deletes an existing `sObject' associated with the Saleforce ID `SObjectId' and model type `MappingKey'. Returns `ok'
%% if successful, otherwise an error.
-spec delete(MappingKey :: mapping_key(), SObjectId :: sf_sobject_id()) -> ok | {error, Reason :: any()}.
delete(MappingKey, SObjectId) ->
    do([error_m ||
        {_MappingModule, MappingUrl} <- get_model_mapping_config(MappingKey),
        SObjectUrl = restc:construct_url(binary_to_list(MappingUrl), binary_to_list(SObjectId), []),
        _ <- sf_client_lib:request(delete, 204, SObjectUrl),
        lager:notice("Successfully deleted sobject with ID: '~ts'", [SObjectId])
    ]).


%% @doc Finds the `sObject' ID.
%%
%% Finds the `sObject' ID over the model `Model' which is associated with the model type `MappingKey'.
%% Returns the `sObject' ID if the `sObject' could be found, otherwise an error.
-spec get_sobject_id_by_model(MappingKey :: mapping_key(), Model :: model()) -> {ok, SObjectId :: sf_sobject_id()} | {error, Reason :: any()}.
get_sobject_id_by_model(MappingKey, Model) ->
    do([error_m ||
        {ModelId, SObject} <- find_sobject_by_model(MappingKey, Model, [{"fields", "Id"}]),
        Id <- sf_client_lib:undefined_lift(st_traverse_utils:traverse_by_path(<<"Id">>, SObject), error_m,
                                           no_id_attribute_found),
        lager:debug("Successfully found sobject with id: '~ts' for model id: '~ts'", [Id, ModelId]),
        return(Id)
    ]).

%%====================================================================
%% Internal functions
%%====================================================================


find_sobject_by_model(MappingKey, Model, Fields) ->
    do([error_m ||
        {MappingModule, MappingUrl} <- get_model_mapping_config(MappingKey),
        ExternalIdAttributeName = case catch MappingModule:sobject_external_id_attribute_name() of
            undefined ->
                <<"ExternalId">>;
            {'EXIT', _} ->
                <<"ExternalId">>;
            AttributeName ->
                AttributeName
        end,
        ModelId = MappingModule:model_id(Model),
        Url = restc:construct_url(binary_to_list(MappingUrl),
                                  binary_to_list(<<ExternalIdAttributeName/binary, "/", ModelId/binary>>), Fields),
        Response <- sf_client_lib:request(get, 200, Url),
        return({ModelId, Response})
    ]).


get_model_mapping_config(MappingKey) ->
    do([error_m ||
        SObjectMapping <- sf_client_sobjects_mapping_server:get_sobjects_mapping(MappingKey),
        MappingModule = sf_client_sobjects_mapping:'#get-model'(mapping_module, SObjectMapping),
        MappingUrl = sf_client_sobjects_mapping:'#get-model'(mapping_url, SObjectMapping),
        return({MappingModule, MappingUrl})
    ]).
