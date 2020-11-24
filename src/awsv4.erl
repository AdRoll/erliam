%%% @copyright (C) 2017, AdRoll
%%% @doc
%%%
%%%     AWSv4 signature support adapted from Kinetic.
%%%
%%% @end
%%% Created :  1 Feb 2017 by Mike Watters <mike.watters@adroll.com>
-module(awsv4).

-export([headers/2, headers/3, headers/11, canonical_query/1, long_term_credentials/2,
         credentials_from_plist/1, isonow/0, isonow/1]).

-include("erliam.hrl").

-type credentials() :: #credentials{}.

-export_type([credentials/0]).

-type pairs() :: #{string() => iodata()} | [{string(), iodata()}].

%%%% API

headers(Credentials, Parameters) ->
    headers(Credentials, Parameters, <<>>).

headers(Credentials, Parameters, undefined) ->
    headers(Credentials, Parameters, <<>>);
headers(Credentials,
        #{service := Service, region := Region} = Parameters,
        RequestPayload) ->
    Defaults =
        #{target_api => undefined,
          method => "GET",
          path => "/",
          query_params => #{},
          signed_headers => #{},
          aws_date => undefined,
          host => join($., [Service, Region, "amazonaws.com"])},
    headers_(Credentials, maps:merge(Defaults, Parameters), RequestPayload).

-spec headers(Credentials :: credentials(),
              Service :: string(),
              Region :: string(),
              Host :: string(),
              AwsDate :: undefined | aws_datetime(),
              TargetAPI :: string() | undefined,
              Method :: string(),
              Path :: string(),
              QueryParams :: pairs(),
              ExtraSignedHeaders :: pairs(),
              RequestPayload :: binary()) ->
                 [{HeaderName :: string(), HeaderValue :: iodata()}].
headers(#credentials{secret_access_key = SecretAccessKey,
                     access_key_id = AccessKeyId,
                     security_token = SecurityToken},
        Service,
        Region,
        Host,
        AwsDate,
        TargetAPI,
        Method,
        Path,
        QueryParams,
        ExtraSignedHeaders,
        RequestPayload) ->
    ActualAwsDate =
        case AwsDate of
            undefined ->
                isonow();
            _ ->
                AwsDate
        end,
    Date = lists:sublist(ActualAwsDate, 8), % yyyymmdd
    Scope = [Date, Region, Service, "aws4_request"],

    Hash = sha256,
    Algorithm = "AWS4-HMAC-SHA256",
    SigningKey =
        lists:foldl(fun(E, A) -> crypto:hmac(Hash, A, E) end, "AWS4" ++ SecretAccessKey, Scope),

    Headers =
        lists:keysort(1,
                      [{string:to_lower(K), V}
                       || {K, V}
                              <- [{"host", Host},
                                  {"x-amz-date", ActualAwsDate},
                                  {"x-amz-security-token", SecurityToken},
                                  {"x-amz-target", TargetAPI}]
                                 ++
                                     if is_map(ExtraSignedHeaders) ->
                                            maps:to_list(ExtraSignedHeaders);
                                        true ->
                                            ExtraSignedHeaders
                                     end,
                          V /= undefined]),

    SignedHeaders = string:join([Name || {Name, _} <- Headers], ";"),

    CanonicalHeaders = [[K, $:, V, $\n] || {K, V} <- Headers],

    PayloadHash = hexlify(crypto:hash(Hash, RequestPayload)),
    CanonicalRequest =
        join($\n,
             [Method,
              canonical_path(Service, Path),
              canonical_query(QueryParams),
              CanonicalHeaders,
              SignedHeaders,
              PayloadHash]),

    CredentialScope = join($/, Scope),

    StringToSign =
        join($\n,
             [Algorithm,
              ActualAwsDate,
              CredentialScope,
              hexlify(crypto:hash(Hash, CanonicalRequest))]),

    Signature = hexlify(crypto:hmac(Hash, SigningKey, StringToSign)),

    [{"authorization",
      [Algorithm,
       " Credential=",
       AccessKeyId,
       $/,
       CredentialScope,
       ",SignedHeaders=",
       SignedHeaders,
       ",Signature=",
       Signature]},
     {"x-amz-content-sha256", PayloadHash}
     | Headers].

-spec isonow(calendar:datetime()) -> aws_datetime().
isonow({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    lists:flatten(
        io_lib:format("~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                      [Year, Month, Day, Hour, Min, Sec])).

isonow() ->
    isonow(calendar:universal_time()).

%% fixme; handle repeated params.
-spec canonical_query(pairs()) -> iolist().
canonical_query(QueryParams) when is_list(QueryParams) ->
    join($&,
         [[quote(K), $=, quote(V)] || {K, V} <- lists:keysort(1, QueryParams), V /= undefined]);
canonical_query(QueryParams) when is_map(QueryParams) ->
    canonical_query(maps:to_list(QueryParams)).

-spec long_term_credentials(iodata(), iodata()) -> credentials().
long_term_credentials(AccessKeyId, SecretAccessKey) ->
    #credentials{access_key_id = AccessKeyId, secret_access_key = SecretAccessKey}.

-spec credentials_from_plist([{expiration | token | access_key_id | secret_access_key,
                               iodata() | undefined}]) ->
                                credentials().
credentials_from_plist(Plist) ->
    #credentials{expiration = erliam_util:getkey(expiration, Plist),
                 security_token = erliam_util:getkey(token, Plist),
                 access_key_id = erliam_util:getkey(access_key_id, Plist),
                 secret_access_key = erliam_util:getkey(secret_access_key, Plist)}.

%%%% INTERNAL FUNCTIONS

headers_(Credentials,
         #{service := Service,
           region := Region,
           host := Host,
           target_api := TargetAPI,
           aws_date := AwsDate,
           method := Method,
           path := Path,
           query_params := QueryParams,
           signed_headers := ExtraSignedHeaders},
         RequestPayload) ->
    headers(Credentials,
            Service,
            Region,
            Host,
            AwsDate,
            TargetAPI,
            Method,
            Path,
            QueryParams,
            ExtraSignedHeaders,
            RequestPayload).

canonical_path(_Service, Path) ->
    %% note: should remove redundant and relative path components, except leave empty path
    %% components for s3.
    quote(Path, path).

quote(X) ->
    quote(X, all).

quote(undefined, _) ->
    <<>>;
quote(X, Kind) when is_list(X) ->
    quote(unicode:characters_to_binary(X, utf8), Kind);
quote(X, Kind) when is_binary(X) ->
    << <<case should_encode(C, Kind) of
             true ->
                 [A, B] = string:to_upper(int_to_hex(C)),
                 <<"%", A, B>>;
             false ->
                 <<C>>
         end/binary>>
       || <<C:8>> <= X >>.

should_encode(X, Kind) ->
    ExtraChars =
        case Kind of
            all ->
                ":/?#[]@";
            path ->
                []
        end,
    %% note: does not encode gen-delims ()
    X > 127 orelse X < 33 orelse lists:member(X, "!$&'()*+,;=" ++ ExtraChars).

hexlify(Bin) ->
    [int_to_hex(X) || X <- binary_to_list(Bin)].

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).

join(Sep, List) ->
    lists:foldr(fun (E, []) ->
                        [E];
                    (E, A) ->
                        [E, Sep | A]
                end,
                [],
                List).

%%%% TESTS

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

join_test() ->
    ?assertEqual([], join(y, [])),
    ?assertEqual([x], join(y, [x])),
    ?assertEqual([w, y, x, y, z], join(y, [w, x, z])).

flattened(KVs) ->
    [{K, lists:flatten(V)} || {K, V} <- KVs].

flattened_headers(Args) ->
    flattened(apply(?MODULE, headers, Args)).

basic_headers_test() ->
    Actual =
        flattened_headers([#credentials{secret_access_key = "secretkey",
                                        access_key_id = "accesskey",
                                        security_token = "securitytoken"},
                           "kinesis",
                           "us-east-1",
                           "kinesis.us-east-1.amazonaws.com",
                           "20140629T022822Z",
                           "Kinesis_20131202.ListStreams",
                           "POST",
                           "/",
                           [],
                           #{},
                           "something"]),
    Expected =
        flattened([{"authorization",
                    ["AWS4-HMAC-SHA256 Credential=accesskey/20140629/us-east-1/kinesis/aws"
                     "4_request",
                     ",SignedHeaders=host;x-amz-date;x-amz-security-token;x-amz-target",
                     ",Signature=847fee48568298911772356fe332443bf2679c48fd42695a84aaa0d0e"
                     "7f28c66"]},
                   {"x-amz-content-sha256",
                    "3fc9b689459d738f8c88a3a48aa9e33542016b7a4052e001aaa536fca74813cb"},
                   {"host", "kinesis.us-east-1.amazonaws.com"},
                   {"x-amz-date", "20140629T022822Z"},
                   {"x-amz-security-token", "securitytoken"},
                   {"x-amz-target", "Kinesis_20131202.ListStreams"}]),
    ?assertEqual(Expected, Actual).

aws4_example1_test() ->
    %% get-vanilla-query-order-key-case from aws4 test suite
    Actual =
        flattened_headers([#credentials{secret_access_key =
                                            "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY",
                                        access_key_id = "AKIDEXAMPLE"},
                           #{aws_date => "20150830T123600Z",
                             service => "service",
                             region => "us-east-1",
                             host => "example.amazonaws.com",
                             query_params => #{"Param2" => "value2", "Param1" => "value1"}}]),
    Expected =
        flattened([{"authorization",
                    ["AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/service/a"
                     "ws4_request",
                     ",SignedHeaders=host;x-amz-date",
                     ",Signature=b97d918cfa904a5beff61c982a1b6f458b799221646efd99d3219ec94"
                     "cdf2500"]},
                   {"x-amz-content-sha256",
                    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
                   {"host", "example.amazonaws.com"},
                   {"x-amz-date", "20150830T123600Z"}]),
    ?assertEqual(Expected, Actual).

aws4_example2_test() ->
    %% post-vanilla-query from aws4 test suite
    Actual =
        flattened_headers([#credentials{secret_access_key =
                                            "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY",
                                        access_key_id = "AKIDEXAMPLE"},
                           #{aws_date => "20150830T123600Z",
                             service => "service",
                             region => "us-east-1",
                             host => "example.amazonaws.com",
                             method => "POST",
                             query_params => #{"Param1" => "value1"}}]),
    Expected =
        flattened([{"authorization",
                    ["AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/service/a"
                     "ws4_request",
                     ",SignedHeaders=host;x-amz-date",
                     ",Signature=28038455d6de14eafc1f9222cf5aa6f1a96197d7deb8263271d420d13"
                     "8af7f11"]},
                   {"x-amz-content-sha256",
                    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
                   {"host", "example.amazonaws.com"},
                   {"x-amz-date", "20150830T123600Z"}]),
    ?assertEqual(Expected, Actual).

aws4_example3_test() ->
    %% get-unreserved from aws4 test suite
    Actual =
        flattened_headers([#credentials{secret_access_key =
                                            "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY",
                                        access_key_id = "AKIDEXAMPLE"},
                           #{aws_date => "20150830T123600Z",
                             service => "service",
                             region => "us-east-1",
                             host => "example.amazonaws.com",
                             path =>
                                 "/-._~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"}]),
    Expected =
        flattened([{"authorization",
                    ["AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/service/a"
                     "ws4_request",
                     ",SignedHeaders=host;x-amz-date",
                     ",Signature=07ef7494c76fa4850883e2b006601f940f8a34d404d0cfa977f52a65b"
                     "bf5f24f"]},
                   {"x-amz-content-sha256",
                    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
                   {"host", "example.amazonaws.com"},
                   {"x-amz-date", "20150830T123600Z"}]),
    ?assertEqual(Expected, Actual).

-endif.
