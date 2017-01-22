%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2017, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 09. Jan 2017 21:08
%%%-------------------------------------------------------------------
-module(sf_client_tests).
-author("David Robakowski").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAPPING_KEY, sf_client_config_eunit_lib).

-define(RESTC_ERR_RESPONSE(ExpectedStatusCode, Header, Body), {error, ExpectedStatusCode, Header, Body}).

%%%============================================================================
%%% API
%%%============================================================================


fixture_test_() ->
    {foreach, fun setup/0, fun teardown/1,
        [
%%            fun sf_client_credentials_changed_test/1
        ]}.


%%sf_client_access_token_exired_test(_) ->
%%    ok.


%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------


setup() ->
    sf_client_config_eunit_lib:setup().


teardown(Context) ->
    sf_client_config_eunit_lib:teardown(Context).
