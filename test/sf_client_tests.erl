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


%%%============================================================================
%%% API
%%%============================================================================


fixture_test_() ->
    {foreach, fun setup/0, fun teardown/1,
        [
            fun sf_client_integration_test/1
        ]}.


sf_client_integration_test(_) ->

    Property = ?FORALL({Model1, Model2}, ?SUCHTHAT({Model1, Model2},
                       {sf_client:model(), sf_client:model()},
                       Model1 /= Model2),
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
        end),

    ?_assert(proper:quickcheck(Property, [{to_file, user}])).


%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------


setup() ->
    sf_client_config_eunit_lib:setup().


teardown(Context) ->
    sf_client_config_eunit_lib:teardown(Context).
