-module(torr_parser).
-author("Tobias & David").

-import(string,[tokens/2]).
-import(lists,[map/2,all/2]).
-import(dict,[from_list/1,find/2]).
-import(erlang,[list_to_integer/2]).

-export([parse_req/2]).

parse_req(String,Pid) ->
   RawPairs = tokens(String,"&"),
   KeyValues = map(fun(E) -> list_to_tuple(tokens(E,"=")) end, RawPairs),
   case all(fun(Tup) -> tuple_size(Tup) =:= 2 end, KeyValues) of
      false -> Pid ! parse_fail;
      true ->
         UrlFun = fun({Key,Val}) -> {Key,url_decode(Val)} end,
         Dict = from_list(map(UrlFun,KeyValues)),
         Pid ! {parse_ok,Dict}
      end.

%% Take care of this later.
%% Should fail in case of malformed encoded string
url_decode([]) -> [];
url_decode([$%,H,L|Tail]) ->
   Hex = list_to_integer([H,L],16),
   [Hex | url_decode(Tail)];
url_decode([H|Tail]) -> [H | url_decode(Tail)].
   
