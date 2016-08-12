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
-type db_sobject() :: [{Key :: binary(), Value :: term()}].

-export_type([
     sobject_table_name/0
    ,db_sobject/0
]).

-callback new_sobject_from_model(Model :: any()) -> DbSObject :: db_sobject().

-callback sobject_table_name() -> sobject_table_name().

-callback to_string(Model :: any()) -> string().
