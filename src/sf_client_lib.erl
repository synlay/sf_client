%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 12. Aug 2016 18:53
%%%-------------------------------------------------------------------
-module(sf_client_lib).
-author("David Robakowski").

%% API
-export([
     request/3
    ,request/4
    ,request/5
    ,undefined_lift/3
]).


request(Action, ExpectedStatusCode, Url) ->
    request([], Action, ExpectedStatusCode, Url).


request(DbModel, Action, ExpectedStatusCode, Url) ->
    request(DbModel, Action, ExpectedStatusCode, Url, true).


request(DbModel, Action, ExpectedStatusCode, Url, UseAuth) ->
    request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, 5).


undefined_lift(undefined, Monad, Message) -> Monad:fail(Message);

undefined_lift(Value, Monad, _Message) -> Monad:return(Value).

%%%===================================================================
%%% Internal functions
%%%===================================================================


request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, Retries) when Retries > 0 ->
    Auth = if
        UseAuth ->
            {ok, AccessToken} = sf_client_access_token_server:get_server_access_token(),
            [{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}];
        true ->
            []
    end,

    case restc:request(Action, json, Url, [ExpectedStatusCode], Auth, DbModel) of
        {ok, ExpectedStatusCode, _Header, Body} ->
            error_m:return(Body);
        {error, 401, _Header, _Body} ->
            Timeout = st_math_lib:ceiling(2000 * rand:uniform() + 1000),
            lager:debug("Token seems to be unauthorized or expired, retrying the request after: ~p ms; "
                        "Left retries: ~p", [Timeout, Retries]),
%%            sf_client_access_token_server:reasign_server_access_token(),
            timer:sleep(Timeout),
            request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, Retries - 1);
        {error, _Code, _Header, Body} ->
            try
                error_m:fail(hd(Body))
            catch
                _:_ ->
                    error_m:fail(Body)
            end
    end;

request_helper(_DbModel, Action, _ExpectedStatusCode, Url, _UseAuth, _Retries) ->
    lager:error("Maximal retries reached for '~p' request to URL: '~p'", [Action, Url]),
    error_m:fail(max_retries).
