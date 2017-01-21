%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2017, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2017 12:26
%%%-------------------------------------------------------------------
-module(sf_client_SUITE).
-author("David Robakowski").

-define(MAPPING_KEY, sf_client_config_eunit_lib).
-define(RESTC_ERR_RESPONSE(ExpectedStatusCode, Header, Body), {error, ExpectedStatusCode, Header, Body}).


-export([
     all/0
    ,init_per_testcase/2
    ,end_per_testcase/2
    ,proper_integration_test/1
    ,credentials_changed/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%%all() -> [].
%%all() -> [proper_integration_test, credentials_changed].
all() -> [proper_integration_test].
%%all() -> [credentials_changed].


init_per_testcase(_TestCase, Config) ->
    ok = sf_client_config_eunit_lib:setup(),
    Config.


end_per_testcase(_TestCase, Config) ->
    sf_client_config_eunit_lib:teardown(Config).


proper_integration_test(_Config) ->
    NumTests = list_to_integer(os:getenv("NUMTESTS", "100")),
    ?assert(proper:quickcheck(integration_test(), [{numtests, NumTests}])).


integration_test() ->
    ?FORALL({Model1, Model2}, ?SUCHTHAT({Model1, Model2}, {sf_client:model(), sf_client:model()}, Model1 /= Model2),

        ?WHENFAIL(ct:pal("intgration test failes with the following models:~n~p~n~n~p~n", [Model1, Model2]),
        begin
            InternalModelId = make_ref(),
            {ok, ID} = sf_client:create(?MAPPING_KEY, {InternalModelId, Model1}),
            {ok, ID} = sf_client:get_sobject_id_by_model(?MAPPING_KEY, {InternalModelId, Model1}),
            ok = sf_client:update(?MAPPING_KEY, ID, {InternalModelId, Model2}),
            {ok, ID} = sf_client:get_sobject_id_by_model(?MAPPING_KEY, {InternalModelId, Model2}),
            ok = sf_client:delete(?MAPPING_KEY, ID),
            {error, not_found} = sf_client:get_sobject_id_by_model(?MAPPING_KEY, {InternalModelId, Model1}),
            {error, not_found} = sf_client:get_sobject_id_by_model(?MAPPING_KEY, {InternalModelId, Model2}),
            true
        end)).


credentials_changed(_Config) ->
    Model1 = <<>>,
    Model2 = <<>>,

    InternalModelId = make_ref(),
    {ok, _ID} = sf_client:create(?MAPPING_KEY, {InternalModelId, Model1}),

    meck:expect(restc, request, fun
        (post, json, "https://localhost/services/oauth2/token?grant_type=" ++ _, [200], [], []) ->
            ?RESTC_ERR_RESPONSE(401, [], #{
                <<"error">> => <<"invalid_grant">>
            });

        %% Create a new resource => sf_client:create...
        (post, json, <<"https://localhost/services/data/v39.0/sobjects/MOCK_sobject_table_name">>, [201],
            [{<<"Authorization">>,<<"Bearer ACCESS_TOKEN">>}], _DbModel) ->
            ?RESTC_ERR_RESPONSE(401, [], #{
                <<"error">> => <<"invalid_grant">>
            })
    end),

    InternalModel2Id = make_ref(),
    {error, max_retries} = sf_client:create(?MAPPING_KEY, {InternalModel2Id, Model2}).
