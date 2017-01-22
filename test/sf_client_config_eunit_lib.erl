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
-define(RESTC_ERR_RESPONSE(ExpectedStatusCode, Header, Body), {error, ExpectedStatusCode, Header, Body}).

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
    ,setup/1
    ,teardown/1
    ,unset_config_env/0
    ,update_config_env/5
    ,mock_access_token_request_success/1
    ,mock_access_token_request_success/5
]).


unset_config_env() ->
    true = os:unsetenv("SF_CREDENTIALS_CLIENT_ID"),
    true = os:unsetenv("SF_CREDENTIALS_CLIENT_SECRET"),
    true = os:unsetenv("SF_CREDENTIALS_USERNAME"),
    true = os:unsetenv("SF_CREDENTIALS_PASSWORD"),
    true = os:unsetenv("SF_ACCESS_TOKEN_EXPIRY").


update_config_env(ClientId, ClientSecret, Username, Password, AccessTokenExpiry) ->
    true = os:putenv("SF_CREDENTIALS_CLIENT_ID", ClientId),
    true = os:putenv("SF_CREDENTIALS_CLIENT_SECRET", ClientSecret),
    true = os:putenv("SF_CREDENTIALS_USERNAME", Username),
    true = os:putenv("SF_CREDENTIALS_PASSWORD", Password),
    true = os:putenv("SF_ACCESS_TOKEN_EXPIRY", integer_to_list(AccessTokenExpiry)).


mock_access_token_request_success(AccessToken) ->
    mock_access_token_request_success(AccessToken, "client_id", "client_secret", "username", "password").


mock_access_token_request_success(AccessToken, ClientId, ClientSecret, Username, Password) ->
    timer:sleep(100),

    Url = restc:construct_url("https://localhost", "/services/oauth2/token", [
        {"grant_type", "password"},
        {"client_id", ClientId},
        {"client_secret", ClientSecret},
        {"username", Username},
        {"password", Password}
    ]),

    AccessTokenHeader = <<"Bearer ", AccessToken/binary>>,

    meck:expect(restc, request, fun
        %% sf_client_sobjects_mapping_server:init_sf_mappings/0@L163 - Get a List of Resources
        (get, json, "https://localhost/services/data", [200], [], []) ->
            ?RESTC_RESPONSE(200, [], [#{
                 <<"version">> => <<"39.0">>
                ,<<"url">> => <<"/services/data/v39.0">>
            }]);

        %% sf_client_access_token_server:init_system/0@L326 - Get an access token
        (post, json, UrlToMatch, [200], [], []) when UrlToMatch == Url ->
            ?RESTC_RESPONSE(200, [], #{
                 <<"access_token">> => AccessToken
                ,<<"instance_url">> => <<"https://cs83.salesforce.com">>
                ,<<"id">> => <<"https://test.salesforce.com/id/.../...">>
                ,<<"token_type">> => <<"Bearer">>
                ,<<"issued_at">> => <<"1484007526619">>
                ,<<"signature">> => <<"....">>
            });

        (post, json, "https://localhost/services/oauth2/token?grant_type=" ++ _, [200], [], []) ->
            invalid_grant();

        %% sf_client_sobjects_mapping_server:init_sf_mappings/0@L167 - Get the sobjects path
        (get, json, "https://localhost/services/data/v39.0", [200],[{<<"Authorization">>, Token}], []) when
                                                                                           Token == AccessTokenHeader ->
            ?RESTC_RESPONSE(200, [], #{
                <<"sobjects">> => <<"/services/data/v39.0/sobjects">>
            });

        (get, json, "https://localhost/services/data/v39.0", [200],[{<<"Authorization">>, _InvalidGrant}], []) ->
            invalid_grant();

        %% sf_client_sobjects_mapping_server:init_sf_mappings - Get the corresponding sobject urls
        (get, json, "https://localhost/services/data/v39.0/sobjects", [200], [{<<"Authorization">>, Token}], []) when
                                                                                           Token == AccessTokenHeader ->
            ?RESTC_RESPONSE(200, [], #{
                <<"sobjects">> => [#{
                     <<"name">> => sobject_table_name()
                    ,<<"urls">> => #{
                        <<"sobject">> => <<"/services/data/v39.0/sobjects/MOCK_sobject_table_name">>
                    }
                }]
            });

        (get, json, "https://localhost/services/data/v39.0/sobjects", [200],
                                                                          [{<<"Authorization">>, _InvalidGrant}], []) ->
            invalid_grant();

        %% Create a new resource => sf_client:create...
        (post, json, <<"https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name">>, [201],
                                                                           [{<<"Authorization">>, Token}], DbModel) when
                                                                                           Token == AccessTokenHeader ->

            SfInternalId = get_sf_id(DbModel),
            ExternalId = proplists:get_value(sobject_external_id_attribute_name(), DbModel),
            true = ets:insert(?MODULE, {SfInternalId, DbModel, ExternalId}),

            ?RESTC_RESPONSE(201, [], #{
                 <<"id">> => SfInternalId
                ,<<"success">> => true
                ,<<"errors">> => []
            });

        (post, json, <<"https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name">>, [201],
                                                                    [{<<"Authorization">>, _InvalidGrant}], _DbModel) ->
            invalid_grant();

        %% Find the SF resource by the internal model id
        (get, json,
                "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/External_ID__c/" ++ IdWithSufix,
                                                                         [200], [{<<"Authorization">>, Token}], []) when
                                                                                           Token == AccessTokenHeader ->

            [ExternalId, _Suffix] = binary:split(list_to_binary(IdWithSufix), <<"?fields=Id">>),

            case ets:match(?MODULE, {'$1', '_', ExternalId}) of
                [] ->
                    {error, not_found};
                [[SfInternalId]] ->
                    ?RESTC_RESPONSE(200, [], #{<<"Id">> => SfInternalId})
            end;

        (get, json, "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/External_ID__c/" ++ _,
                                                                   [200], [{<<"Authorization">>, _InvalidGrant}], []) ->
            invalid_grant();

        %% Update a specific SF resource
        (patch, json, "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/" ++ SObjectIdStr, [204],
                                                                        [{<<"Authorization">>, Token}], NewDbModel) when
                                                                                           Token == AccessTokenHeader ->

            SObjectId = list_to_binary(SObjectIdStr),
            ExternalId = proplists:get_value(sobject_external_id_attribute_name(), NewDbModel),

            true = ets:insert(?MODULE, {SObjectId, NewDbModel, ExternalId}),

            ?RESTC_RESPONSE(204, [], <<>>);

        (patch, json, "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/" ++ _, [204],
                                                                 [{<<"Authorization">>, _InvalidGrant}], _NewDbModel) ->
            invalid_grant();

        %% Delete a specific SF resource
        (delete, json, "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/" ++ SObjectIdStr, [204],
                                                                                [{<<"Authorization">>, Token}], []) when
                                                                                           Token == AccessTokenHeader ->

            SObjectId = list_to_binary(SObjectIdStr),

            true = ets:delete(?MODULE, SObjectId),

            ?RESTC_RESPONSE(204, [], <<>>);

            (delete, json, "https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name/" ++ _, [204],
                                                                          [{<<"Authorization">>, _InvalidGrant}], []) ->
                invalid_grant();

        (Action, json, UrlToMatch, [Status], _Auth, DbModel) ->
            ?debugVal({Action, UrlToMatch, Status, _Auth, DbModel}),
            throw(not_implemented)

    end).


setup() ->
    setup(<<"ACCESS_TOKEN">>).


setup(AccessToken) ->

    teardown(ok),

    _ = ets:new(?MODULE, [public, named_table, set]),

    application:ensure_all_started(restc),

    ok = meck:new(sf_client_config, [passthrough]),
    ok = meck:new(restc, [passthrough]),

    meck:expect(sf_client_config, get_sobjects_mapping, fun() ->
        #{?MODULE => ?MODULE}
    end),

    mock_access_token_request_success(AccessToken),

    application:ensure_all_started(sf_client),

    ok.


teardown(_) ->
    catch ets:delete(?MODULE),
    catch meck:unload(sf_client_config),
    catch meck:unload(restc),
    catch application:stop(sf_client),
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


%%====================================================================
%% Internal functions
%%====================================================================


invalid_grant() ->
    ?RESTC_ERR_RESPONSE(401, [], #{
        <<"error">> => <<"invalid_grant">>
    }).
