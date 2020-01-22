%%% @copyright (C) 2017, AdRoll
%%% @doc
%%%
%%%    Helper for parsing XML returned by certain AWS APIs.
%%%
%%% @end
%%% Created :  2 Jun 2017 by Mike Watters <mike.watters@adroll.com>

-module(erliam_xml).

-export([parse/1]).

-include_lib("xmerl/include/xmerl.hrl").

%%%% API

parse(Xml) ->
    {E, _} = xmerl_scan:string(unicode:characters_to_list(Xml, utf8)),
    parse_element(E).

%%%% INTERNAL FUNCTIONS

parse_element(#xmlElement{name = Name, content = Content}) ->
    {Name,
     case Content of
       [#xmlElement{} | _] ->
           [{ChildName, parse_element(ChildContent)}
            || #xmlElement{name = ChildName, content = ChildContent}
                   <- Content];
       _ -> parse_element(Content)
     end};
parse_element([#xmlText{} | _] = L) ->
    case lists:all(fun (E) -> is_record(E, xmlText) end, L) of
      true -> unescape_xml_text(xmerl_xs:value_of(L));
      %% if a list of elements is not solely text, discard the text elements and parse
      %% the non-text elements:
      false -> parse_element([X || X <- L, not is_record(X, xmlText)])
    end;
parse_element(L) when is_list(L) -> [parse_element(X) || X <- L];
parse_element(X) -> X.

unescape_xml_text(X) ->
    unescape_xml_text(unicode:characters_to_binary(X, utf8),
                      [{<<"&lt;">>, <<"<">>}, {<<"&gt;">>, <<">">>},
                       {<<"&quot;">>, <<"\"">>}, {<<"&apos;">>, <<"'">>},
                       {<<"&amp;">>, <<"&">>}]).

unescape_xml_text(X, []) -> X;
unescape_xml_text(X, [{C, R} | T]) ->
    unescape_xml_text(binary:replace(X, C, R, [global]), T).

%%%% TESTS
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basic_decode_test() ->
    Data = <<"<GetSessionTokenResponse xmlns=\"https://sts.amazonaws"
             ".com/doc/2011-06-15/\">\n  <GetSessionTokenResult>\n "
             "  <Credentials>\n    <AccessKeyId>ACCESS_KEY_ID</Acces"
             "sKeyId>\n    <SecretAccessKey>SECRET_ACCESS_KEY</Secre"
             "tAccessKey>\n    <SessionToken>SESSION_TOKEN</SessionT"
             "oken>\n    <Expiration>EXPIRATION</Expiration>\n "
             "  </Credentials>\n  </GetSessionTokenResult>\n "
             " <ResponseMetadata>\n   <RequestId>REQUEST_ID</Request"
             "Id>\n  </ResponseMetadata>\n </GetSessionTokenResponse"
             ">\n">>,
    ?assertEqual({'GetSessionTokenResponse',
                  [{'GetSessionTokenResult',
                    [{'Credentials',
                      [{'AccessKeyId', <<"ACCESS_KEY_ID">>},
                       {'SecretAccessKey', <<"SECRET_ACCESS_KEY">>},
                       {'SessionToken', <<"SESSION_TOKEN">>},
                       {'Expiration', <<"EXPIRATION">>}]}]},
                   {'ResponseMetadata', [{'RequestId', <<"REQUEST_ID">>}]}]},
                 parse(Data)).

-endif.

