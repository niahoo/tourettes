-module(torr_parser).
-author("Tobias & David").

-import(string,[tokens/2]).
-import(lists,[map/2,all/2]).
-import(dict,[from_list/1,find/2]).

-export([parse/2]).
-compile(export_all).

parse(<<"GET /announce?",Request/binary>>,Pid) -> ok.
%    PairList = split(Request,<<>>,<<>>).
%    Pid ! {parse_ok,PairList}.

% Empty request strings are NOT accepted
split_fsm(_State,<<>>,<<>>,_Val) -> exit(early_end);
split_fsm(_State,<<>>,_Key,<<>>) -> exit(early_end);
split_fsm(_State,<<>>,Key,Val) -> 
      [{url_decode(Key,<<>>),url_decode(Val,<<>>)}];

% When parsing the key
split_fsm(key,<<$&,_Rest/binary>>,_Key,_Val) -> exit(early_amp);
split_fsm(key,<<$=,_Rest/binary>>,<<>>,_Val) -> exit(early_eq);
split_fsm(key,<<$=,Rest/binary>>,Key,<<>>)   -> split_fsm(val,Rest,url_decode(Key,<<>>),<<>>);
split_fsm(key,<<$=,_Rest/binary>>,_Key,_Val)  -> exit(parse_fail); %should never happen
split_fsm(key,<<Char:8,Rest/binary>>,Key,_Val) -> split_fsm(key,Rest,<<Key/binary,Char>>,<<>>);
% When parsing the value
split_fsm(val,<<$=,_Rest/binary>>,_Key,_Val) -> exit(early_eq);
split_fsm(val,<<$&,_Rest/binary>>,_Key,<<>>) -> exit(early_amp);
split_fsm(val,<<$&,_Rest/binary>>,<<>>,_Val) -> exit(parse_fail); % should never happen
split_fsm(val,<<$&,Rest/binary>>,Key,Val) -> 
   [ {Key, url_decode(Val,<<>>)} | split_fsm(key,Rest,<<>>,<<>>) ];
split_fsm(val,<<Char:8,Rest/binary>>,Key,Val) -> split_fsm(val,Rest,Key,<<Val/binary,Char>>).

% Fails in case of a malformed string
url_decode(<<>>,Acc) -> Acc;
url_decode(<<$%,H:8,L:8,Rest/binary>>,Acc) ->
   Value = list_to_integer([H,L]),
   url_decode(Rest,<<Acc/binary,Value>>);
url_decode(<<Byte:8,Rest/binary>>,Acc) -> url_decode(Rest,<<Acc/binary,Byte>>).
