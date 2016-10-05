%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2016 19:52
%%%-------------------------------------------------------------------
-module(sf_client_sobjects_mapping).
-author("David Robakowski").

-compile({parse_transform, exprecs}).
-compile({parse_transform, do}).

-export_records([model]).

-type mapping_module() :: atom().
-type mapping_url() :: binary().

-record(model, {
     mapping_module :: mapping_module() | undefined
    ,mapping_url    :: mapping_url() | undefined
}).

-type model() :: #model{}.

-export_type([
     model/0
    ,mapping_module/0
    ,mapping_url/0
]).

%% API
-export([
    new/2
]).


-spec new(MappingModule :: mapping_module(), MappingUrl :: mapping_url()) -> model().
new(MappingModule, MappingUrl) ->
    #model{
         mapping_module = MappingModule
        ,mapping_url = MappingUrl
    }.
