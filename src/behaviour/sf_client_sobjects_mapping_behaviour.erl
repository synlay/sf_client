%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2016 18:47
%%%-------------------------------------------------------------------
-module(sf_client_sobjects_mapping_behaviour).
-author("David Robakowski").

-type sobject_table_name() :: binary().
-type sobject_external_id_attribute_name() :: binary().
-type model_id() :: binary().
-type db_sobject() :: [{Key :: binary(), Value :: term()}].

-export_type([
     sobject_table_name/0
    ,sobject_external_id_attribute_name/0
    ,model_id/0
    ,db_sobject/0
]).


%% @doc Transform an internal data model `Model' to a `sObject' model `DbSObject'.
-callback new_sobject_from_model(Model :: sf_client:model()) -> DbSObject :: db_sobject().


%% @doc Define the particular `sObject' name for which this module should be associated.
-callback sobject_table_name() -> sobject_table_name().


%% @doc Define the attribute name where the internal model ID will be saved, in order to find `sObjects' with it.
-callback sobject_external_id_attribute_name() -> sobject_external_id_attribute_name().


%% @doc Transform the model ID included in the model `Model' into a Salesforce friendly way to be able find
%% the associated `sObjects' with it.
-callback model_id(Model :: sf_client:model()) -> model_id().


%% @doc Return a textual representation which will be used for logging.
-callback to_string(Model :: sf_client:model()) -> string().
