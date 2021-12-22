%%% @copyright (C) 2014, AdRoll
%%% @doc
%%%
%%%     Helper module for obtaining information from an instance metadata server.
%%%
%%% @end
-module(imds).

-export([role_name/0, zone/0, instance_id/0, public_hostname/0, get_session_token/0,
         imds_response/3, imds_response/4]).

-define(IMDS_HOST, erliam_config:g(imds_host, "169.254.169.254")).
-define(IMDS_VERSION, erliam_config:g(imds_version, "latest")).
-define(IMDS_TIMEOUT, 30000).
-define(IMDS_RETRIES, 3).

%%%% API

-spec role_name() -> {error, term()} | {ok, string()}.
role_name() ->
    imds_text("iam/security-credentials/").

-spec zone() -> {error, term()} | {ok, string()}.
zone() ->
    imds_text("placement/availability-zone").

-spec instance_id() -> {error, term()} | {ok, string()}.
instance_id() ->
    imds_text("instance-id").

-spec public_hostname() -> {error, term()} | {ok, string()}.
public_hostname() ->
    imds_text("public-hostname").

%% Obtain a session token from the instance metadata server, returning an
%% awsv4:credentials().
-spec get_session_token() -> {error, term()} | awsv4:credentials().
get_session_token() ->
    case role_name() of
        {ok, RoleName} ->
            Result = imds_tokens(["iam/security-credentials/", RoleName]),
            awsv4:credentials_from_plist(Result);
        Error ->
            Error
    end.

%% Make a GET request to the given URL, expecting (accepting) the given mime types, and
%% with the given request timeout in milliseconds.
-spec imds_response(string(), [string()], pos_integer()) ->
                       {ok, term()} | {error, term()}.
imds_response(Url, MimeTypes, Timeout) ->
    RequestHeaders = [{"Accept", string:join(MimeTypes, ", ")}],
    case httpc:request(get,
                       {Url, RequestHeaders},
                       [{timeout, Timeout}],
                       [{body_format, binary}],
                       erliam:httpc_profile())
    of
        {ok, {{_, 200, _}, Headers, Body}} ->
            case lists:member(
                     erliam_util:mime_type(Headers), MimeTypes)
            of
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
-spec imds_response(string(), [string()], pos_integer(), pos_integer()) ->
                       {ok, term()} | {error, term()}.
imds_response(Url, MimeTypes, Timeout, Retries) ->
    call_with_retry(?MODULE,
                    imds_response,
                    [Url, MimeTypes, Timeout],
                    "Could not obtain IMDS response: ~p~n",
                    Retries).

%%%% INTERNAL FUNCTIONS

%% Call the given Transform function with the result of a successful call to
%% imds_response/4, or return the error which resulted from that call.
-spec imds_transform_response(string(), [string()], function()) ->
                                 {error, term()} | term().
imds_transform_response(Url, MimeTypes, Transform) ->
    case imds_response(Url, MimeTypes, ?IMDS_TIMEOUT, ?IMDS_RETRIES) of
        {ok, Result} ->
            Transform(Result);
        Error ->
            Error
    end.

%% Fetch the given Url and return the response as text (a string) if successful.
-spec imds_text_response(string()) -> {ok, string()} | {error, term()}.
imds_text_response(Url) ->
    imds_transform_response(Url,
                            ["text/plain"],
                            %% fixme; assumes utf-8 encoding.
                            fun(Result) ->
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
-spec imds_token_response(string()) -> [{atom(), string()}] | {error, term()}.
imds_token_response(Url) ->
    MimeTypes = ["text/plain", "application/json"],
    imds_transform_response(Url, MimeTypes, fun metadata_response_to_token_proplist/1).

%% Obtain relevant values from the JSON response body, returning a proplist with atom keys
%% and the appropriate values.
metadata_response_to_token_proplist(Body) ->
    Targets =
        [{<<"Expiration">>, expiration},
         {<<"AccessKeyId">>, access_key_id},
         {<<"SecretAccessKey">>, secret_access_key},
         {<<"Token">>, token}],
    case get_code(Body) of
        {success, Plist} ->
            lists:foldl(fun({Element, Value}, Acc) ->
                           case erliam_util:getkey(Element, Targets) of
                               undefined ->
                                   Acc;
                               AtomName ->
                                   [{AtomName, binary_to_list(Value)} | Acc]
                           end
                        end,
                        [],
                        Plist);
        Error ->
            Error
    end.

get_code(Body) ->
    case jiffy:decode(Body) of
        {Plist} ->
            case erliam_util:getkey(<<"Code">>, Plist) of
                <<"Success">> ->
                    {success, Plist};
                _ ->
                    {error, failed_token_response}
            end;
        _ ->
            {error, invalid_token_json}
    end.

%% Call the given M:F with Args, emitting an error with the given ErrorFormat (with the
%% error as the single format argument) and retrying otherwise.  Does not catch exits.
-spec call_with_retry(atom(), atom(), list(), string(), integer()) ->
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

imds_url(Suffix) ->
    uri_string:normalize(["http://", ?IMDS_HOST, "/", ?IMDS_VERSION, "/meta-data/", Suffix]).

imds_text(Suffix) ->
    imds_text_response(imds_url(Suffix)).

imds_tokens(Suffix) ->
    imds_token_response(imds_url(Suffix)).

%%%% UNIT TESTS
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

metadata_response_to_proplist_test() ->
    Body =
        <<"{\"Code\":\"Success\",\"LastUpdated\":\"2014-10-17T15:17:07-07:00\",\""
          "Type\":\"AWS-HMAC\",\"AccessKeyId\":\"XYZZY\",\"SecretAccessKey\":\""
          "FLOOBLE\",\"Token\":\"BAZZLE\",\"Expiration\":\"2014-10-18T09:00:30Z\""
          "}">>,
    Result = metadata_response_to_token_proplist(Body),
    Expected =
        [{expiration, "2014-10-18T09:00:30Z"},
         {access_key_id, "XYZZY"},
         {secret_access_key, "FLOOBLE"},
         {token, "BAZZLE"}],
    [?assertEqual(erliam_util:getkey(Key, Expected), erliam_util:getkey(Key, Result))
     || Key <- [expiration, access_key_id, secret_access_key, token]],
    ok.

-endif.
