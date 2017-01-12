%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2017, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 13. Jan 2017 00:00
%%%-------------------------------------------------------------------
-module(sf_client_config_eunit_lib).
-author("David Robakowski").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(sf_client_sobjects_mapping_behaviour).

-define(MAPPING_KEY, ?MODULE).

-define(RESTC_RESPONSE(ExpectedStatusCode, Header, Body), {ok, ExpectedStatusCode, Header, Body}).

%% API
-export([
    new_sobject_from_model/1,
    sobject_table_name/0,
    sobject_external_id_attribute_name/0,
    model_id/1,
    to_string/1
]).

%% API
-export([
     setup/0
    ,teardown/1
]).


setup() ->

    _ = ets:new(?MODULE, [public, named_table, set]),

    application:ensure_all_started(restc),

    ok = meck:new(sf_client_config, [passthrough]),
    ok = meck:new(restc, [passthrough]),

    meck:expect(sf_client_config, get_sobjects_mapping, fun() ->
        #{?MODULE => ?MODULE}
    end),

    meck:expect(restc, request, fun
        %% sf_client_sobjects_mapping_server:init_sf_mappings/0@L163 - Get a List of Resources
        (get, json, "https://localhost/services/data", [200], [], []) ->
            ?RESTC_RESPONSE(200, [], [#{
                 <<"version">> => <<"39.0">>
                ,<<"url">> => <<"/services/data/v39.0">>
            }]);

        %% sf_client_access_token_server:init_system/0@L326 - Get an access token
        (post, json, "https://localhost/services/oauth2/token?grant_type=" ++ _, [200], [], []) ->
            ?RESTC_RESPONSE(200, [], #{
                 <<"access_token">> => <<"ACCESS_TOKEN">>
                ,<<"instance_url">> => <<"https://cs83.salesforce.com">>
                ,<<"id">> => <<"https://test.salesforce.com/id/.../...">>
                ,<<"token_type">> => <<"Bearer">>
                ,<<"issued_at">> => <<"1484007526619">>
                ,<<"signature">> => <<"....">>
            });

        %% sf_client_sobjects_mapping_server:init_sf_mappings/0@L167 - Get the sobjects path
        (get, json, "https://localhost/services/data/v39.0", [200],[{<<"Authorization">>,<<"Bearer ACCESS_TOKEN">>}],
                                                                                                                  []) ->
            ?RESTC_RESPONSE(200, [], #{
                <<"sobjects">> => <<"/services/data/v39.0/sobjects">>
            });

        %% sf_client_sobjects_mapping_server:init_sf_mappings - Get the corresponding sobject urls
        (get, json, "https://localhost/services/data/v39.0/sobjects", [200],
                                                               [{<<"Authorization">>,<<"Bearer ACCESS_TOKEN">>}], []) ->
            ?RESTC_RESPONSE(200, [], #{
                <<"sobjects">> => [#{
                     <<"name">> => sobject_table_name()
                    ,<<"urls">> => #{
                        <<"sobject">> => <<"/services/data/v39.0/sobjects/MOCK_sobject_table_name">>
                    }
                }]
            });

        %% Create a new resource => sf_client:create...
        (post, json, <<"https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name">>, [201],
                                                          [{<<"Authorization">>,<<"Bearer ACCESS_TOKEN">>}], DbModel) ->

            SfInternalId = get_sf_id(DbModel),
            ExternalId = proplists:get_value(sobject_external_id_attribute_name(), DbModel),
            true = ets:insert(?MODULE, {SfInternalId, DbModel, ExternalId}),

            ?RESTC_RESPONSE(201, [], #{
                 <<"id">> => SfInternalId
                ,<<"success">> => true
                ,<<"errors">> => []
            });

        %% Find the SF resource by the internal model id
        (get, json,
                "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/External_ID__c/" ++ IdWithSufix,
                                                        [200], [{<<"Authorization">>,<<"Bearer ACCESS_TOKEN">>}], []) ->

            [ExternalId, _Suffix] = binary:split(list_to_binary(IdWithSufix), <<"?fields=Id">>),

            case ets:match(?MODULE, {'$1', '_', ExternalId}) of
                [] ->
                    {error, not_found};
                [[SfInternalId]] ->
                    ?RESTC_RESPONSE(200, [], #{<<"Id">> => SfInternalId})
            end;

        %% Update a specific SF resource
        (patch, json, "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/" ++ SObjectIdStr, [204],
                                                       [{<<"Authorization">>,<<"Bearer ACCESS_TOKEN">>}], NewDbModel) ->

            SObjectId = list_to_binary(SObjectIdStr),
            ExternalId = proplists:get_value(sobject_external_id_attribute_name(), NewDbModel),

            true = ets:insert(?MODULE, {SObjectId, NewDbModel, ExternalId}),

            ?RESTC_RESPONSE(204, [], <<>>);

        %% Delete a specific SF resource
        (delete, json, "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/" ++ SObjectIdStr, [204],
                                                               [{<<"Authorization">>,<<"Bearer ACCESS_TOKEN">>}], []) ->

            SObjectId = list_to_binary(SObjectIdStr),

            true = ets:delete(?MODULE, SObjectId),

            ?RESTC_RESPONSE(204, [], <<>>);

        (Action, json, Url, [Status], _Auth, DbModel) ->
            ?debugVal({Action, Url, Status, _Auth, DbModel}),
            throw(not_implemented)

    end),

    application:ensure_all_started(sf_client),

    ok.


teardown(_) ->
    true = ets:delete(?MODULE),
    catch meck:unload(sf_client_config),
    catch meck:unload(restc),
    ok.


%%--------------------------------------------------------------------
%% @doc sf_client_sobjects_mapping_behaviour implementation
%%--------------------------------------------------------------------


get_sf_id(Model) ->
    st_digest_lib:hexbinarystring(crypto:hash(md4,(term_to_binary(Model)))).


new_sobject_from_model(Model) ->
    [
         {sobject_external_id_attribute_name(), model_id(Model)}
        ,{<<"Data">>, term_to_binary(Model)}
    ].


sobject_external_id_attribute_name() ->
    <<"External_ID__c">>.


model_id({Id, _Model}) ->
    st_digest_lib:hexbinarystring(crypto:hash(md4,(term_to_binary(Id)))).


sobject_table_name() ->
    <<"MOCK_sobject_table_name">>.


to_string(Model) ->
    binary_to_list(model_id(Model)).
