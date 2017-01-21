%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2017, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2017 16:32
%%%-------------------------------------------------------------------
-module(sf_client_config_tests).
-author("David Robakowski").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(RESTC_RESPONSE(ExpectedStatusCode, Header, Body), {ok, ExpectedStatusCode, Header, Body}).


%%%============================================================================
%%% API
%%%============================================================================


fixture_test_() ->
    {foreach, fun setup/0, fun teardown/1,
        [
             fun sf_client_config_sf_rest_api_test/1
            ,fun sf_client_config_unset_test/1
        ]}.


sf_client_config_sf_rest_api_test(_) ->

    UrlType = ?LET(UrlParts, {oneof([return(""), return("https://")]), non_empty(list(range($ , $~)))}, begin
        {Protocol, Domain} = UrlParts,
        string:concat(Protocol, Domain)
    end),
    PrintableString = non_empty(list(range(1,255))),

    Property = ?FORALL(String, PrintableString,
    begin
        true = os:putenv("SF_REST_API_VERSION", String),
        sf_client_config:init(),
        list_to_binary(String) == sf_client_config:get_sf_rest_api_version()
    end),

    EndpointUrlProp = ?FORALL(URL, UrlType,
    begin
        true = os:putenv("SF_REST_API_ENDPOINT", URL),
        HttpsUrl =
        case URL of
            "https://" ++ _ ->
                URL;
            URL ->
                "https://" ++ URL
        end,
        MeckURL = restc:construct_url(HttpsUrl, "/services/data", []),
        meck:expect(restc, request, fun(get, json, Path, [200], [], []) when Path == MeckURL ->
            ?RESTC_RESPONSE(200, [], [#{
                <<"version">> => <<"39.0">>
                ,<<"url">> => <<"/services/data/v39.0">>
            }])
        end),
        sf_client_config:init(),
        {ok, <<"/services/data/v39.0">>} == sf_client_config:get_sf_rest_api_version_path() andalso
        list_to_binary(HttpsUrl) == sf_client_config:get_sf_rest_api_endpoint()

    end),
    CredentialProperty = ?FORALL({ClientId, ClientSecret, Username, Password, AccessTokenExpiry},
                                 {PrintableString, PrintableString, PrintableString, PrintableString, pos_integer()},
        begin
            true = os:putenv("SF_CREDENTIALS_CLIENT_ID", ClientId),
            true = os:putenv("SF_CREDENTIALS_CLIENT_SECRET", ClientSecret),
            true = os:putenv("SF_CREDENTIALS_USERNAME", Username),
            true = os:putenv("SF_CREDENTIALS_PASSWORD", Password),
            true = os:putenv("SF_ACCESS_TOKEN_EXPIRY", integer_to_list(AccessTokenExpiry)),
            sf_client_config:init(),
            ClientId == sf_client_config:get_credentials_client_id() andalso
            ClientSecret == sf_client_config:get_credentials_client_secret() andalso
            Username == sf_client_config:get_credentials_username() andalso
            Password == sf_client_config:get_credentials_password() andalso
            AccessTokenExpiry == sf_client_config:get_access_token_expiry()
        end),

    {inorder, [
        {"Configuration for sf_rest_api_version from app.config should match",
            ?_assertEqual(<<"39.0">>, sf_client_config:get_sf_rest_api_version())}
        ,?_assert(proper:quickcheck(Property, [{to_file, user}]))
        ,{"Configuration for sf_rest_api_endpoint from app.config should match",
            ?_assertEqual(<<"https://localhost">>, begin
                true = os:unsetenv("SF_REST_API_ENDPOINT"),
                sf_client_config:get_sf_rest_api_endpoint()
            end)}
        ,?_assert(proper:quickcheck(EndpointUrlProp, [{to_file, user}]))
        ,?_assertEqual({error, dont_set_sf_rest_api_version_path}, begin
             true = os:putenv("_INTERNAL_SF_REST_API_VERSION_PATH", "test"),
             sf_client_config:init(),
             sf_client_config:get_sf_rest_api_version_path()
         end)
        ,?_assertEqual({error, no_available_rest_api_version_path}, begin
             true = os:unsetenv("SF_REST_API_ENDPOINT"),
             true = os:unsetenv("_INTERNAL_SF_REST_API_VERSION_PATH"),
             meck:expect(restc, request, fun(_, _, _, _, _, _) ->
                error_m:fail(noconnection)
             end),
             sf_client_config:init(),
             sf_client_config:get_sf_rest_api_version_path()
         end)
        ,{"Configuration for client_id from app.config should match",
            ?_assertEqual("client_id", sf_client_config:get_credentials_client_id())}
        ,{"Configuration for client_secret from app.config should match",
            ?_assertEqual("client_secret", sf_client_config:get_credentials_client_secret())}
        ,{"Configuration for username from app.config should match",
            ?_assertEqual("username", sf_client_config:get_credentials_username())}
        ,{"Configuration for password from app.config should match",
            ?_assertEqual("password", sf_client_config:get_credentials_password())}
        ,{"Configuration for access_token_expiry from app.config should match",
            ?_assertEqual(3600, sf_client_config:get_access_token_expiry())}
        ,{"Configuration for access_token_server_request_retry_timeout from app.config should match",
            ?_assertEqual(6300, sf_client_config:get_access_token_server_request_retry_timeout())}
        ,?_assert(proper:quickcheck(CredentialProperty, [{to_file, user}]))
        ,{"Configuration for sobjects_mapping from app.config should match",
            ?_assertEqual(#{}, begin ok = meck:unload(sf_client_config), sf_client_config:get_sobjects_mapping() end )}
    ]}.


sf_client_config_unset_test(_) ->
    application:unset_env(sf_client_config:application(), sf_rest_api_version),
    application:unset_env(sf_client_config:application(), sf_rest_api_endpoint),
    application:unset_env(sf_client_config:application(), sf_credentials_client_id),
    application:unset_env(sf_client_config:application(), sf_credentials_client_secret),
    application:unset_env(sf_client_config:application(), sf_credentials_username),
    application:unset_env(sf_client_config:application(), sf_credentials_password),
    application:unset_env(sf_client_config:application(), sf_access_token_expiry),
    application:unset_env(sf_client_config:application(), access_token_server_request_retry_timeout),
    application:unset_env(sf_client_config:application(), sobjects_mapping),
    {inorder, [
         {"Missing configuration for sf_rest_api_version should throw an error",
            ?_assertError({missing_config, sf_rest_api_version}, sf_client_config:get_sf_rest_api_version())}
        ,{"Missing configuration for sf_rest_api_endpoint should throw an error",
            ?_assertError({missing_config, sf_rest_api_endpoint}, sf_client_config:get_sf_rest_api_endpoint())}
        ,{"Missing configuration for sf_credentials_client_id should throw an error",
            ?_assertError({missing_config, sf_credentials_client_id}, sf_client_config:get_credentials_client_id())}
        ,{"Missing configuration for sf_credentials_client_secret should throw an error",
            ?_assertError({missing_config, sf_credentials_client_secret}, sf_client_config:get_credentials_client_secret())}
        ,{"Missing configuration for sf_credentials_username should throw an error",
            ?_assertError({missing_config, sf_credentials_username}, sf_client_config:get_credentials_username())}
        ,{"Missing configuration for sf_credentials_password should throw an error",
            ?_assertError({missing_config, sf_credentials_password}, sf_client_config:get_credentials_password())}
        ,{"Missing configuration for sf_access_token_expiry should throw an error",
            ?_assertError({missing_config, sf_access_token_expiry}, sf_client_config:get_access_token_expiry())}
        ,{"Missing configuration for access_token_server_request_retry_timeout should throw an error",
            ?_assertError({missing_config, access_token_server_request_retry_timeout}, sf_client_config:get_access_token_server_request_retry_timeout())}
        ,{"Missing configuration for sobjects_mapping should throw an error",
            ?_assertError({missing_config, sobjects_mapping}, begin ok = meck:unload(sf_client_config), sf_client_config:get_sobjects_mapping() end)}
    ]}.

%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------


setup() ->
    sf_client_config_eunit_lib:setup().


teardown(Context) ->
    true = os:unsetenv("SF_REST_API_VERSION"),
    true = os:unsetenv("SF_REST_API_ENDPOINT"),
    true = os:unsetenv("_INTERNAL_SF_REST_API_VERSION_PATH"),
    true = os:unsetenv("SF_CREDENTIALS_CLIENT_ID"),
    true = os:unsetenv("SF_CREDENTIALS_CLIENT_SECRET"),
    true = os:unsetenv("SF_CREDENTIALS_USERNAME"),
    true = os:unsetenv("SF_CREDENTIALS_PASSWORD"),
    true = os:unsetenv("SF_ACCESS_TOKEN_EXPIRY"),

    sf_client_config_eunit_lib:teardown(Context).
