%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2016 18:55
%%%-------------------------------------------------------------------
-module(sf_client).
-author("David Robakowski").

-compile({parse_transform, do}).

%% API
-export([
     create/2
    ,update/3
    ,delete/2
]).


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


update(MappingKey, SObjectId, Model) ->
    do([error_m ||
        {MappingModule, MappingUrl} <- get_model_mapping_config(MappingKey),
        SObjectUrl = restc:construct_url(binary_to_list(MappingUrl), binary_to_list(SObjectId), []),
        _ <- sf_client_lib:request(MappingModule:new_sobject_from_model(Model), patch, 204, SObjectUrl),
        lager:notice("Successfully updated sobject with ID: '~ts' for model: '~s'",
                     [SObjectId, MappingModule:to_string(Model)])
    ]).


delete(MappingKey, SObjectId) ->
    do([error_m ||
        SObjectMapping <- sf_client_sobjects_mapping_server:get_sobjects_mapping(MappingKey),
        MappingUrl = sf_client_sobjects_mapping:'#get-model'(mapping_url, SObjectMapping),
        SObjectUrl = restc:construct_url(binary_to_list(MappingUrl), binary_to_list(SObjectId), []),
        _ <- sf_client_lib:request(delete, 204, SObjectUrl),
        lager:notice("Successfully deleted sobject with ID: '~ts'", [SObjectId])
    ]).

%%====================================================================
%% Internal functions
%%====================================================================

get_model_mapping_config(MappingKey) ->
    do([error_m ||
        SObjectMapping <- sf_client_sobjects_mapping_server:get_sobjects_mapping(MappingKey),
        MappingModule = sf_client_sobjects_mapping:'#get-model'(mapping_module, SObjectMapping),
        MappingUrl = sf_client_sobjects_mapping:'#get-model'(mapping_url, SObjectMapping),
        return({MappingModule, MappingUrl})
    ]).
