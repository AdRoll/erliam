%%% @copyright (C) 2017, AdRoll
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2017 by Mike Watters <mike.watters@adroll.com>

-module(erliam).


-export([httpc_profile/0]).


httpc_profile() ->
    erliam_config:g(httpc_profile, erliam).
