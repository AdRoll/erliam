-module(erliam_config).

-export([g/1, g/2]).

g(Key) ->
    g(Key, undefined).

g(Key, Default) ->
    case application:get_env(erliam, Key) of
      undefined ->
          Default;
      {ok, undefined} ->
          Default;
      {ok, Value} ->
          Value
    end.

