%%% @copyright (C) 2014, AdRoll
%%% @doc
%%%
%%%     Helper module for obtaining information from an instance metadata server.
%%%
%%% @end
%%% Created : 17 Oct 2014 by Mike Watters <mike.watters@adroll.com>

-module(imds).

-export([role_name/0,
         zone/0,
         instance_id/0,
         public_hostname/0,
         get_session_token/0,
         imds_response/3,
         imds_response/4]).

-include("erliam.hrl").

-define(IMDS_HOST, erliam_config:g(imds_host, "169.254.169.254")).
-define(IMDS_URL, "http://" ++ ?IMDS_HOST ++ "/latest/meta-data/").
-define(INSTANCE_ID_URL, ?IMDS_URL ++ "instance-id").
-define(INSTANCE_HOSTNAME_URL, ?IMDS_URL ++ "public-hostname").
-define(AZ_URL, ?IMDS_URL ++ "placement/availability-zone").
-define(IAM_URL, ?IMDS_URL ++ "iam/").
-define(IAM_ROLES_URL, ?IAM_URL ++ "security-credentials/").
-define(IMDS_TIMEOUT, 30000).
-define(IMDS_RETRIES, 3).


%%%% API

-spec role_name() -> {error, term()} | {ok, string()}.
role_name() ->
    imds_text_response(?IAM_ROLES_URL).

-spec zone() -> {error, term()} | {ok, string()}.
zone() ->
    imds_text_response(?AZ_URL).

-spec instance_id() -> {error, term()} | {ok, string()}.
instance_id() ->
    imds_text_response(?INSTANCE_ID_URL).

-spec public_hostname() -> {error, term()} | {ok, string()}.
public_hostname() ->
    imds_text_response(?INSTANCE_HOSTNAME_URL).


%% Obtain a session token from the instance metadata server, returning an
%% awsv4:credentials().
-spec get_session_token() -> {error, term()} | list(proplists:property()).
get_session_token() ->
    case role_name() of
        {ok, RoleName} ->
            %% fixme; urlencode the role name.
            TokenUrl = ?IAM_ROLES_URL ++ RoleName,
            Result = imds_token_response(TokenUrl),
            lists:foldl(fun ({Name, N}, Acc) ->
                                setelement(N, Acc, proplists:get_value(Name, Result))
                        end,
                        #credentials{},
                        [{expiration, #credentials.expiration},
                         {token, #credentials.security_token},
                         {access_key_id, #credentials.access_key_id},
                         {secret_access_key, #credentials.secret_access_key}]);
        Error ->
            Error
    end.


%% Make a GET request to the given URL, expecting (accepting) the given mime types, and
%% with the given request timeout in milliseconds.
-spec imds_response(string(), list(string()), pos_integer()) ->
    {ok, term()} | {error, term()}.
imds_response(Url, MimeTypes, Timeout) ->
    RequestHeaders = [{"Accept", string:join(MimeTypes, ", ")}],
    case httpc:request(get, {Url, RequestHeaders},
                       [{timeout, Timeout}], [{body_format, binary}],
                       erliam:httpc_profile()) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            case lists:member(mime_type(Headers), MimeTypes) of
                true ->
                    {ok, Body};
                false ->
                    %% the server ignored our accept header:
                    {error, unacceptable_response}
            end;
        {ok, {{_, 406, _}, _, _}} ->
            %% the server respected our accept header and could not produce a response
            %% with any of the requested mime types:
            {error, unacceptable_response};
        {ok, {{_, Code, Status}, _, _}} ->
            {error, {bad_response, {Code, Status}}};
        {error, Reason} ->
            {error, Reason}
    end.


%% As in imds_response/3, but retrying on failure.
-spec imds_response(string(), list(string()), pos_integer(), pos_integer()) ->
    {ok, term()} | {error, term()}.
imds_response(Url, MimeTypes, Timeout, Retries) ->
    call_with_retry(?MODULE, imds_response, [Url, MimeTypes, Timeout],
                    "Could not obtain IMDS response: ~p~n",
                    Retries).


%%%% INTERNAL FUNCTIONS

%% Call the given arity-1 Transform function with the result of a successful call to
%% imds_response/4, or return the error which resulted from that call.
-spec imds_transform_response(string(), list(string()), function()) ->
    {error, term()} | term().
imds_transform_response(Url, MimeTypes, Transform) ->
    case imds_response(Url, MimeTypes, ?IMDS_TIMEOUT, ?IMDS_RETRIES) of
        {ok, Result} ->
            Transform(Result);
        Error ->
            Error
    end.


%% Fetch the given Url and return the response as text (a string) if successful.
-spec imds_text_response(string()) ->
    {ok, string()} | {error, term()}.
imds_text_response(Url) ->
    imds_transform_response(Url, ["text/plain"],
                            %% fixme; assumes utf-8 encoding.
                            fun (Result) ->
                                    case unicode:characters_to_list(Result) of
                                        {error, _, _} ->
                                            {error, invalid_unicode};
                                        {incomplete, _, _} ->
                                            {error, invalid_unicode};
                                        String ->
                                            {ok, String}
                                    end
                            end).

%% Fetch the given Url and return the response as a proplist of tokens.
-spec imds_token_response(string()) ->
    list(proplists:property()) | {error, term()}.
imds_token_response(Url) ->
    MimeTypes = ["text/plain", "application/json"],
    imds_transform_response(Url, MimeTypes,
                            fun metadata_response_to_token_proplist/1).


%% Obtain relevant values from the JSON response body, returning a proplist with atom keys
%% and the appropriate values.
metadata_response_to_token_proplist(Body) ->
    Targets = [{<<"Expiration">>, expiration},
               {<<"AccessKeyId">>, access_key_id},
               {<<"SecretAccessKey">>, secret_access_key},
               {<<"Token">>, token}],
    case jiffy:decode(Body) of
        {Plist} ->
            case proplists:get_value(<<"Code">>, Plist) of
                <<"Success">> ->
                    lists:foldl(fun ({Element, Value}, Acc) ->
                                        case lists:keyfind(Element, 1, Targets) of
                                            {_, AtomName} ->
                                                [{AtomName, binary_to_list(Value)} | Acc];
                                            _ ->
                                                Acc
                                        end
                                end,
                                [],
                                Plist);
                _ ->
                    {error, failed_token_response}
            end;
        _ ->
            {error, invalid_token_json}
    end.


%% Return the base mime type based on the given response headers, assuming text/plain if
%% that header is missing.
-spec mime_type(list()) -> string().
mime_type(Headers) ->
    case find_header("content-type", Headers) of
        {ok, MimeType} ->
            [BaseType|_] = string:tokens(MimeType, ";"),
            string:strip(BaseType);
        _ ->
            "text/plain"
    end.


%% Return the named HTTP response header from the given proplist of headers
%% (case-insensitive).
-spec find_header(string(), list(proplists:property())) -> undefined | {ok, string()}.
find_header(Name, Headers) ->
    case lists:keyfind(string:to_lower(Name), 1, [{string:to_lower(HeaderName), HeaderValue}
                                                  || {HeaderName, HeaderValue} <- Headers]) of
        {_, Value} ->
            {ok, Value};
        _ ->
            undefined
    end.



%% Call the given M:F with Args, emitting an error with the given ErrorFormat (with the
%% error as the single format argument) and retrying otherwise.  Does not catch exits.
-spec call_with_retry(module(), function(), list(), string(), integer()) ->
    {ok, term()} | {error, term()}.
call_with_retry(Module, Fun, Args, ErrorFormat, Retries) when Retries > 0 ->
    case apply(Module, Fun, Args) of
        {ok, Result} ->
            {ok, Result};
        Error ->
            error_logger:error_msg(ErrorFormat, [Error]),
            call_with_retry(Module, Fun, Args, ErrorFormat, Retries - 1)
    end;
call_with_retry(_, _, _, _, _) ->
    {error, retries_exceeded}.



%%%% UNIT TESTS
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

find_header_test() ->
    Headers = [{"Content-Type","text/plain; charset=utf-8"},
               {"Content-Length","15"},
               {"Date","Fri, 17 Oct 2014 21:41:13 GMT"}],
    ?assertMatch({ok, "15"}, find_header("content-length", Headers)),
    ?assertMatch({ok, "text/plain; charset=utf-8"}, find_header("Content-Type", Headers)),
    ?assertEqual(undefined, find_header("X-YZZY", Headers)),
    ok.

mime_type_test() ->
    Headers = [{"Content-Type","text/plain; charset=utf-8"},
               {"Content-Length","15"},
               {"Date","Fri, 17 Oct 2014 21:41:13 GMT"}],
    ?assertEqual("text/plain", mime_type(Headers)),
    ?assertEqual("text/plain", mime_type([])),
    ?assertEqual("text/html", mime_type([{"content-type", "text/html; foo=bar"}])),
    ok.

metadata_response_to_proplist_test() ->
    Body = <<"{\"Code\":\"Success\",\"LastUpdated\":\"2014-10-17T15:17:07-07:00\",\"Type\":\"AWS-HMAC\",\"AccessKeyId\":\"XYZZY\",\"SecretAccessKey\":\"FLOOBLE\",\"Token\":\"BAZZLE\",\"Expiration\":\"2014-10-18T09:00:30Z\"}">>,
    Result = metadata_response_to_token_proplist(Body),
    Expected = [{expiration, "2014-10-18T09:00:30Z"},
                {access_key_id, "XYZZY"},
                {secret_access_key, "FLOOBLE"},
                {token, "BAZZLE"}],
    [?assertEqual(proplists:get_value(Key, Expected),
                  proplists:get_value(Key, Result))
     || Key <- [expiration, access_key_id, secret_access_key, token]],
    ok.

-endif.
