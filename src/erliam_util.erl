%%% @copyright (C) 2017, AdRoll
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2017 by Mike Watters <mike.watters@adroll.com>

-module(erliam_util).

-export([getkey/2, find_header/2, mime_type/1]).

%%%% API

%% Return the base mime type based on the given response headers, assuming text/plain if
%% that header is missing.
-spec mime_type(list()) -> string().
mime_type(Headers) ->
    case find_header("content-type", Headers) of
      undefined -> "text/plain";
      MimeType ->
          [BaseType | _] = string:tokens(MimeType, ";"),
          string:strip(BaseType)
    end.

%% Return the named HTTP response header from the given proplist of headers
%% (case-insensitive).
-spec find_header(string(), [{string(), string()}]) -> undefined |
                                                       string().
find_header(Name, Headers) ->
    getkey(string:to_lower(Name),
           [{string:to_lower(HeaderName), HeaderValue}
            || {HeaderName, HeaderValue} <- Headers]).

getkey(Key, Plist) ->
    case lists:keyfind(Key, 1, Plist) of
      false -> undefined;
      {Key, Value} -> Value
    end.

%%%% TESTS
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

mime_type_test() ->
    Headers = [{"Content-Type", "text/plain; charset=utf-8"},
               {"Content-Length", "15"},
               {"Date", "Fri, 17 Oct 2014 21:41:13 GMT"}],
    ?assertEqual("text/plain", mime_type(Headers)),
    ?assertEqual("text/plain", mime_type([])),
    ?assertEqual("text/html",
                 mime_type([{"content-type", "text/html; foo=bar"}])),
    ok.

find_header_test() ->
    Headers = [{"Content-Type", "text/plain; charset=utf-8"},
               {"Content-Length", "15"},
               {"Date", "Fri, 17 Oct 2014 21:41:13 GMT"}],
    ?assertEqual("15", find_header("content-length", Headers)),
    ?assertEqual("text/plain; charset=utf-8",
                 find_header("Content-Type", Headers)),
    ?assertEqual(undefined, find_header("X-YZZY", Headers)),
    ok.

-endif.

