-module(torr_parser).
-author("Tobias & David").

-import(string,[tokens/2]).
-import(lists,[map/2,all/2]).
-import(dict,[is_key/2,store/3,new/0,from_list/1]).

-export([parse/2]).
-compile(export_all).

parse(<<"GET /announce?",Request/binary>>,Pid) -> 
   ReqDict = split_fsm(dict,key,Request,<<>>,<<>>),
   valid(ReqDict),
   Pid ! {{parse_ok,announce},ReqDict};
parse(<<"GET /scrape?",Request/binary>>,Pid) -> 
   ReqList = split_fsm(list,key,Request,<<>>,<<>>),
   case all(fun({K,_}) -> K =:= <<"info_hash">> end,ReqList) of
      true -> Pid ! {{parse_ok,scrape},ReqList};
      false -> exit(bad_scrape)
   end;
parse(_,_) -> exit(bad_request).

% Empty request strings are NOT accepted
split_fsm(_T,_State,<<>>,<<>>,_Val) -> exit(early_end);
split_fsm(_T,_State,<<>>,_Key,<<>>) -> exit(early_end);
split_fsm(list,_State,<<>>,Key,Val) -> 
   [{url_decode(Key,<<>>),url_decode(Val,<<>>)}];
split_fsm(dict,_State,<<>>,Key,Val) -> 
   store(url_decode(Key,<<>>),url_decode(Val,<<>>),new());
% When parsing the key
split_fsm(_T,key,<<$&,_Rest/binary>>,_Key,_Val) -> exit(early_amp);
split_fsm(_T,key,<<$=,_Rest/binary>>,<<>>,_Val) -> exit(early_eq);
split_fsm(Type,key,<<$=,Rest/binary>>,Key,<<>>) -> 
   split_fsm(Type,val,Rest,url_decode(Key,<<>>),<<>>);
split_fsm(Type,key,<<Char:8,Rest/binary>>,Key,_Val) -> 
   split_fsm(Type,key,Rest,<<Key/binary,Char>>,<<>>);
% When parsing the value
split_fsm(_T,val,<<$=,_Rest/binary>>,_Key,_Val) -> exit(early_eq);
split_fsm(_T,val,<<$&,_Rest/binary>>,_Key,<<>>) -> exit(early_amp);
split_fsm(list,val,<<$&,Rest/binary>>,Key,Val) -> 
   [{Key,url_decode(Val,<<>>)} | split_fsm(list,key,Rest,<<>>,<<>>)];
split_fsm(dict,val,<<$&,Rest/binary>>,Key,Val) -> 
   store(Key,url_decode(Val,<<>>),split_fsm(dict,key,Rest,<<>>,<<>>));
split_fsm(T,val,<<Char:8,Rest/binary>>,Key,Val) -> 
   split_fsm(T,val,Rest,Key,<<Val/binary,Char>>).

% Fails in case of a malformed string
url_decode(<<>>,Acc) -> Acc;
url_decode(<<$%,H:8,L:8,Rest/binary>>,Acc) ->
   Value = list_to_integer([H,L]),
   url_decode(Rest,<<Acc/binary,Value>>);
url_decode(<<Byte:8,Rest/binary>>,Acc) -> url_decode(Rest,<<Acc/binary,Byte>>).

valid(Dict) -> case
   is_key(<<"info_hash">>,Dict) and
   is_key(<<"peer_id">>,Dict) and
   is_key(<<"port">>,Dict) of
   % is_key(<<"uploaded">>,Dict) and
   %is_key(<<"downloaded">>,Dict) and
   %is_key(<<"left">>,Dict) of
      true -> Dict;
      false -> exit(missingkeys)
end.
