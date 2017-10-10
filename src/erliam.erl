%%% @copyright (C) 2017, AdRoll
%%% @doc
%%%
%%%    Supports interaction with AWS IAM:
%%%
%%%      - awsv4 signature computation (awsv4.erl)
%%%      - instance metadata (imds.erl)
%%%      - credential caching and storing in ets (erliam_srv.erl)
%%%
%%% @end
%%% Created :  7 Apr 2017 by Mike Watters <mike.watters@adroll.com>
-module(erliam).


-export([httpc_profile/0,
         get_session_token/0,
         credentials/0,
         invalidate/0]).


%% Return the current cached credentials (crash if none are cached or credential refresher
%% server isn't running).
credentials() ->
    erliam_srv:current().


%% Fetch a new session token from STS (if aws_access_key and aws_secret_key app env values
%% are set), or from instance metadata if not.  Normally external users don't need to call
%% this; call credentials/0 instead.
get_session_token() ->
    case {erliam_config:g(aws_access_key), erliam_config:g(aws_secret_key)} of
        {AccessKeyId, SecretAccessKey} when AccessKeyId /= undefined;
                                            SecretAccessKey /= undefined ->
            erliam_sts:get_session_token(awsv4:long_term_credentials(AccessKeyId,
                                                                     SecretAccessKey));
        _ ->
            imds:get_session_token()
    end.


httpc_profile() ->
    erliam_config:g(httpc_profile, erliam).


%% force cached credentials to be invalidated and refreshed.
invalidate() ->
    erliam_srv:invalidate().
