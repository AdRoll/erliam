%%% @copyright (C) 2017, AdRoll
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2017 by Mike Watters <mike.watters@adroll.com>

-module(erliam).


-export([httpc_profile/0,
         get_session_token/0]).


httpc_profile() ->
    erliam_config:g(httpc_profile, erliam).


get_session_token() ->
    case {erliam_config:g(aws_access_key), erliam_config:g(aws_secret_key)} of
        {AccessKeyId, SecretAccessKey} when AccessKeyId /= undefined;
                                            SecretAccessKey /= undefined ->
            erliam_sts:get_session_token(awsv4:long_term_credentials(AccessKeyId,
                                                                     SecretAccessKey));
        _ ->
            imds:get_session_token()
    end.
