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

-compile({parse_transform, do}).

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

random_timeout(LeftRetries, LogMessage, LogArgs) ->
    Timeout = st_math_lib:ceiling(2000 * rand:uniform() + 1000),
    _ = lager:debug(LogMessage ++ "  retrying the request after: ~p ms; Left retries: ~p",
                    LogArgs ++ [Timeout, LeftRetries]),
    timer:sleep(Timeout).


get_access_token_helper(Retries) when Retries > 0 ->
    case sf_client_access_token_server:get_server_access_token() of
        {ok, AccessToken} ->
            error_m:return([{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}]);
        {error, Reason} ->
            ok = random_timeout(Retries, "Can't get access token; Reason: ~p;", [Reason]),
            get_access_token_helper(Retries - 1)
    end;

get_access_token_helper(_Retries) ->
    _ = lager:error("Maximal retries reached for trying to get access token"),
    error_m:fail(get_access_token_max_retries).


request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, Retries) when Retries > 0 ->
    do([error_m ||
        Auth <- if
            UseAuth ->
                get_access_token_helper(5);
            true ->
                error_m:return([])
        end,
        case restc:request(Action, json, Url, [ExpectedStatusCode], Auth, DbModel) of
            {ok, ExpectedStatusCode, _Header, Body} ->
                error_m:return(Body);
            {error, 401, _Header, _Body} ->
                _ = sf_client_access_token_server:reasign_server_access_token(),
                ok = random_timeout(Retries, "Token seems to be unauthorized or expired", []),
                request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, Retries - 1);
            {error, _Reason}=Err ->
                Err;
            {error, _Code, _Header, Body} ->
                try
                    error_m:fail(hd(Body))
                catch
                    _:_ ->
                        error_m:fail(Body)
                end
        end
    ]);

request_helper(_DbModel, Action, _ExpectedStatusCode, Url, _UseAuth, _Retries) ->
    _ = lager:error("Maximal retries reached for '~p' request to URL: '~p'", [Action, Url]),
    error_m:fail(max_retries).
