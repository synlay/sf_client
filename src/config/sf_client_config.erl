%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2016, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2016 17:52
%%%-------------------------------------------------------------------
-module(sf_client_config).
-author("David Robakowski").

-define (APP_ENV, sf_client).

-type sobjects_mapping() :: st_common_types:map_(sf_client:mapping_key(), module()).

%% API
-export([
     init/0
    ,application/0
    ,get_sf_rest_api_version/0
    ,get_sf_rest_api_endpoint/0
    ,get_sf_rest_api_version_path/0
    ,get_credentials_client_id/0
    ,get_credentials_client_secret/0
    ,get_credentials_username/0
    ,get_credentials_password/0
    ,get_access_token_expiry/0
    ,get_sobjects_mapping/0
]).

-export_type([
    sobjects_mapping/0
]).


init() ->
    stillir:set_config(?APP_ENV, [
         {sf_rest_api_version, "SF_REST_API_VERSION", [{transform, binary}, {default, fun() ->
              maybe_to_binary(get_config(sf_rest_api_version))
          end}]}
        ,{sf_rest_api_endpoint, "SF_REST_API_ENDPOINT", [{transform, fun(Endpoint) ->
              list_to_binary(maybe_prefix_with_https(Endpoint))
          end}, {default, fun() ->
              maybe_to_binary(maybe_prefix_with_https(get_config(sf_rest_api_endpoint)))
          end}]}
        ,{sf_rest_api_version_path, "_INTERNAL_SF_REST_API_VERSION_PATH", [{transform, fun(_) ->
              %% sf_rest_api_version_path will be generated at runtime and can't be configured through the config
              error_m:fail(dont_set_sf_rest_api_version_path)
          end}, {default, fun() ->
              RestApiEndpoint = get_sf_rest_api_endpoint(),
              ServiceDataUrl = restc:construct_url(binary_to_list(RestApiEndpoint), "/services/data/", []),
              case sf_client_lib:request([], get, 200, ServiceDataUrl, false, false) of
                  {ok, Body} ->
                      VersionPath = find_iterator(get_sf_rest_api_version(), Body),
                      _ = lager:debug("Found a SalesForce REST API version path: ~s", [VersionPath]),
                      error_m:return(VersionPath);
                  {error, Reason} ->
                      _ = lager:error("Error while trying to list available REST API versions; Reason: ~p", [Reason]),
                      error_m:fail(no_available_rest_api_version_path)
              end
          end}]}
        ,{sf_credentials_client_id, "SF_CREDENTIALS_CLIENT_ID", [{default, fun() ->
              get_credentials_config(client_id)
          end}]}
        ,{sf_credentials_client_secret, "SF_CREDENTIALS_CLIENT_SECRET", [{default, fun() ->
              get_credentials_config(client_secret)
          end}]}
        ,{sf_credentials_username, "SF_CREDENTIALS_USERNAME", [{default, fun() ->
              get_credentials_config(username)
          end}]}
        ,{sf_credentials_password, "SF_CREDENTIALS_PASSWORD", [{default, fun() ->
              get_credentials_config(password)
          end}]}
        ,{sf_access_token_expiry, "SF_ACCESS_TOKEN_EXPIRY", [{transform, integer}, {default, fun() ->
              get_config(access_token_expiry)
          end}]}
    ]).


-spec application() -> ?APP_ENV.
application() ->
    ?APP_ENV.


-spec get_sf_rest_api_version() -> binary().
get_sf_rest_api_version() ->
    stillir:get_config(?APP_ENV, sf_rest_api_version).


-spec get_sf_rest_api_endpoint() -> binary().
get_sf_rest_api_endpoint() ->
    stillir:get_config(?APP_ENV, sf_rest_api_endpoint).


-spec get_sf_rest_api_version_path() -> {ok, binary()} | {error, no_available_rest_api_version_path | dont_set_sf_rest_api_version_path}.
get_sf_rest_api_version_path() ->
    stillir:get_config(?APP_ENV, sf_rest_api_version_path).


-spec get_credentials_client_id() -> string().
get_credentials_client_id() ->
    stillir:get_config(?APP_ENV, sf_credentials_client_id).


-spec get_credentials_client_secret() -> string().
get_credentials_client_secret() ->
    stillir:get_config(?APP_ENV, sf_credentials_client_secret).


-spec get_credentials_username() -> string().
get_credentials_username() ->
    stillir:get_config(?APP_ENV, sf_credentials_username).


-spec get_credentials_password() -> string().
get_credentials_password() ->
    stillir:get_config(?APP_ENV, sf_credentials_password).


-spec get_access_token_expiry() -> pos_integer().
get_access_token_expiry() ->
    stillir:get_config(?APP_ENV, sf_access_token_expiry).


-spec get_sobjects_mapping() -> sobjects_mapping().
get_sobjects_mapping() ->
    get_config(sobjects_mapping).

%%====================================================================
%% Internal functions
%%====================================================================

get_credentials_config(Key) ->
    proplists:get_value(Key, get_config(credentials)).

get_config(Config) ->
    get_required(?APP_ENV, Config).

get_required(AppEnv, Key) ->
    case application:get_env(AppEnv, Key) of
        undefined ->
            erlang:error({missing_config, Key});
        {ok, Value} ->
            Value
    end.


find_iterator(_SearchedVersion, []) ->
    <<"/services/data/v39.0">>;
find_iterator(SearchedVersion, [VersionTuple | Tail]) ->
    case version_compare(SearchedVersion, st_traverse_utils:traverse_by_path(<<"version">>, VersionTuple)) of
        true ->
            st_traverse_utils:traverse_by_path(<<"url">>, VersionTuple);
        false ->
            find_iterator(SearchedVersion, Tail)
    end.


version_compare(SearchedVersion, SearchedVersion) ->
    true;
version_compare(_, _) ->
    false.


-spec maybe_to_binary(Value :: list() | binary()) -> Result :: binary().
maybe_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);

maybe_to_binary(Value) ->
    Value.


-spec maybe_prefix_with_https(MaybeURL :: binary() | list()) -> URL :: binary() | list().
maybe_prefix_with_https(<<"https://", _Domain/binary>>=URL) ->
    URL;

maybe_prefix_with_https("https://" ++ _Domain=URL) ->
    URL;

maybe_prefix_with_https(Domain) ->
    "https://" ++ Domain.
