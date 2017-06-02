%%% @copyright (C) 2017, AdRoll
%%% @doc
%%%
%%%     Support obtaining session tokens from STS using long-term credentials.
%%%
%%% @end
%%% Created :  2 Jun 2017 by Mike Watters <mike.watters@adroll.com>

-module(erliam_sts).

-export([get_session_token/1]).

-define(STS_HOST, erliam_config:g(sts_host, "sts.amazonaws.com")).
-define(STS_REGION, erliam_config:g(sts_region, "us-east-1")).
-define(STS_TIMEOUT, 30000).


%%%% API

get_session_token(Credentials) ->
    Query = #{"Action" => "GetSessionToken",
              "Version" => "2011-06-15"},
    Host = ?STS_HOST,
    Headers = [{"Accept", "text/xml"}]
        ++ awsv4:headers(Credentials, #{service => "sts",
                                        region => ?STS_REGION,
                                        query_params => Query,
                                        host => Host}),
    Url = "https://" ++ Host ++ "?" ++ awsv4:canonical_query(Query),
    decode_response(httpc:request(get, {Url, Headers},
                                  [{timeout, ?STS_TIMEOUT}],
                                  [{body_format, binary}],
                                  erliam:httpc_profile())).


%%%% INTERNAL FUNCTIONS

decode_response({ok, {{_, 200, _}, Headers, Body}}) ->
    case erliam_util:mime_type(Headers) of
        "text/xml" ->
            decode_credentials([erliam_xml:parse(Body)]);
        _ ->
            {error, unacceptable_response}
    end;

decode_response({ok, {{_, 406, _}, _, _}}) ->
    %% the server respected our accept header and could not produce a response with any of
    %% the requested mime types:
    {error, unacceptable_response};

decode_response({ok, {{_, Code, Status}, _, _}}) ->
    {error, {bad_response, {Code, Status}}};

decode_response({error, _} = Error) ->
    Error.


decode_credentials(Plist) ->
    KeyPath = ['GetSessionTokenResponse', 'GetSessionTokenResult', 'Credentials'],
    case lists:foldl(fun (E, A) when is_list(A) ->
                             erliam_util:getkey(E, A);
                         (_, _) ->
                             undefined
                     end, Plist, KeyPath) of
        CredentialPlist when is_list(CredentialPlist) ->
            awsv4:credentials_from_plist(convert_credential_plist(CredentialPlist));
        _ ->
            {error, {bad_result, Plist}}
    end.


convert_credential_plist(Plist) ->
    KeyMap = [{'AccessKeyId', access_key_id},
              {'SecretAccessKey', secret_access_key},
              {'Expiration', expiration},
              {'SessionToken', token}],
    [{erliam_util:getkey(K, KeyMap), binary_to_list(V)}
     || {K, V} <- Plist].
