%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2017, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2017 14:23
%%%-------------------------------------------------------------------
-module(sf_client_access_token_server_SUITE).
-author("David Robakowski").

-define(RESTC_ERR_RESPONSE(ExpectedStatusCode, Header, Body), {error, ExpectedStatusCode, Header, Body}).

-export([
     all/0
    ,init_per_testcase/2
    ,end_per_testcase/2
%%    ,init_per_suite/1
%%    ,end_per_suite/1
    ,integration_test/1
    ,token_expired/1
    ,invalid_credentials/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%%all() -> [integration_test, token_expired, invalid_credentials].
all() -> [integration_test, token_expired].
%%all() -> [integration_test].
%%all() -> [token_expired_test].


init_per_testcase(_TestCase, Config) ->
    AccessToken = term_to_binary(make_ref()),
    ok = sf_client_config_eunit_lib:setup(AccessToken),
    [{access_token, AccessToken} | Config].


end_per_testcase(_TestCase, Config) ->
    sf_client_config_eunit_lib:teardown(Config).


integration_test(Config) ->
    AccessToken = ?config(access_token, Config),
    {ok, AccessToken} = sf_client_access_token_server:get_server_access_token(),
    NewAccessToken = term_to_binary(make_ref()),
    sf_client_config_eunit_lib:mock_access_token_request_success(NewAccessToken),
    {ok, NewAccessToken} = sf_client_access_token_server:reasign_server_access_token(),
    {ok, NewAccessToken} = sf_client_access_token_server:get_server_access_token().


token_expired(_Config) ->
    AccessToken = term_to_binary(make_ref()),
    sf_client_config_eunit_lib:mock_access_token_request_success(AccessToken),
    meck:expect(sf_client_config, get_access_token_expiry, fun() ->
        501
    end),
    {ok, AccessToken} = sf_client_access_token_server:reasign_server_access_token(),
    NewAccessToken = term_to_binary(make_ref()),
    sf_client_config_eunit_lib:mock_access_token_request_success(NewAccessToken),
    catch timer:sleep(1000),
    {ok, NewAccessToken} = sf_client_access_token_server:get_server_access_token().


invalid_credentials(_Config) ->
    meck:expect(restc, request, fun
        (post, json, "https://localhost/services/oauth2/token?grant_type=" ++ _, [200], [], []) ->
            ?RESTC_ERR_RESPONSE(401, [], #{
                <<"error">> => <<"invalid_grant">>
            })
    end),

    {error, bla} = sf_client_access_token_server:reasign_server_access_token().
