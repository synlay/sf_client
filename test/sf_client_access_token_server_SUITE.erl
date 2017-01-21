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
    ,white_box_testing/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


all() -> [integration_test, token_expired, invalid_credentials, white_box_testing].


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
    ok = sf_client_access_token_server:reasign_server_access_token(),
    {ok, NewAccessToken} = sf_client_access_token_server:get_server_access_token().


token_expired(_Config) ->
    AccessToken = term_to_binary(make_ref()),
    sf_client_config_eunit_lib:mock_access_token_request_success(AccessToken),
    meck:expect(sf_client_config, get_access_token_expiry, fun() -> 501 end),
    ok = sf_client_access_token_server:reasign_server_access_token(),
    {ok, AccessToken} = sf_client_access_token_server:get_server_access_token(),
    NewAccessToken = term_to_binary(make_ref()),
    sf_client_config_eunit_lib:mock_access_token_request_success(NewAccessToken),
    catch timer:sleep(1000),
    {ok, NewAccessToken} = sf_client_access_token_server:get_server_access_token().


invalid_credentials(Config) ->
    AccessToken = ?config(access_token, Config),
    ok = sf_client_access_token_server:reasign_server_access_token(),
    {ok, AccessToken} = sf_client_access_token_server:get_server_access_token(),

    meck:expect(restc, request, fun
        (post, json, "https://localhost/services/oauth2/token?grant_type=" ++ _, [200], [], []) ->
            ?RESTC_ERR_RESPONSE(401, [], #{
                <<"error">> => <<"invalid_grant">>
            })
    end),
    ok = sf_client_access_token_server:reasign_server_access_token(),
    {error, max_retries} = sf_client_access_token_server:get_server_access_token(),
    meck:expect(sf_client_config, get_access_token_server_request_retry_timeout, fun() -> 0 end),
    {error, timeout_while_trying_to_communicate_with_auth_server} =
                                                                sf_client_access_token_server:get_server_access_token().

white_box_testing(_Config) ->
    Event = make_ref(),
    From = make_ref(),
    StateName = make_ref(),
    State = make_ref(),
    OldVsn = make_ref(),
    Extra = make_ref(),
    {next_state, StateName, State} = sf_client_access_token_server:handle_event(Event, StateName, State),
    {next_state, StateName, State} = sf_client_access_token_server:handle_sync_event(Event, From, StateName, State),
    {next_state, StateName, State} = sf_client_access_token_server:handle_info(Event, StateName, State),
    {ok, StateName, State} = sf_client_access_token_server:code_change(OldVsn, StateName, State, Extra).
