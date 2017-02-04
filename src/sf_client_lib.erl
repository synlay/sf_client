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
    ,request/6
    ,undefined_lift/3
]).


-spec request(Action :: restc:method(), ExpectedStatusCode :: restc:status_code(), Url :: restc:url()) -> {ok, Body :: restc:body()} | {error, Reason :: any()}.
request(Action, ExpectedStatusCode, Url) ->
    request([], Action, ExpectedStatusCode, Url).

request(Action, ExpectedStatusCode, Url, UseAuth, RetryRequest) ->
    request([], Action, ExpectedStatusCode, Url, UseAuth, RetryRequest).

request(DbModel, Action, ExpectedStatusCode, Url) ->
    request(DbModel, Action, ExpectedStatusCode, Url, true, true).

request(DbModel, Action, ExpectedStatusCode, Url, UseAuth, RetryRequest) ->
    request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, RetryRequest, 3, undefined).


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


get_access_token_helper() ->
    case sf_client_access_token_server:get_server_access_token() of
        {ok, AccessToken} ->
            error_m:return([{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}]);
        {error, _Reason}=Err ->
            Err
    end.


request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, RetryRequest, Retries, _ErrAcc) when not RetryRequest
                                                                                                   orelse Retries > 0 ->
    Return =
    do([error_m ||
        Auth <- if
            UseAuth ->
                get_access_token_helper();
            true ->
                error_m:return([])
        end,
        case restc:request(Action, json, Url, [ExpectedStatusCode], Auth, DbModel) of
            {ok, ExpectedStatusCode, _Header, Body} ->
                error_m:return(Body);
            {error, 401, _Header, _Body} ->
                error_m:fail(not_authorized);
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
    ]),
    case Return of
        {error, Reason}=Err ->
            if
                RetryRequest andalso Reason == not_authorized ->
                    _ = sf_client_access_token_server:reasign_server_access_token(),
                    ok = random_timeout(Retries, "Token seems to be unauthorized or expired", []),
                    request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, RetryRequest, Retries - 1, Reason);

                RetryRequest ->
                    request_helper(DbModel, Action, ExpectedStatusCode, Url, UseAuth, RetryRequest, Retries - 1, Reason);

                true ->
                    Err
            end;

        OkTuple ->
            OkTuple
    end;

request_helper(_DbModel, _Action, _ExpectedStatusCode, _Url, _UseAuth, _RetryRequest, _Retries, ErrAcc) ->
    _ = lager:error("Maximal retries reached for trying to execute request"),
    error_m:fail(ErrAcc).
