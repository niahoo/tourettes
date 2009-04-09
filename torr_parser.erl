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
   case all(fun(Tup) -> tuple_size(Tup) == 2 end, KeyValues) of
      false -> Pid ! parse_fail;
      true ->
        Dict = from_list(map(fun(A) -> convert(A) end,KeyValues)),
        Pid ! {parse_ok,Dict}
      end.

convert({Key,Val}) -> 
    KBin = list_to_binary(Key),
    case KBin of
        <<"port">> -> {KBin,list_to_integer(Val)};
        <<"uploaded">> -> {KBin,list_to_integer(Val)};
        <<"downloaded">> -> {KBin,list_to_integer(Val)};
        <<"left">> -> {KBin,list_to_integer(Val)};
        _   -> {KBin,list_to_binary(url_decode(Val))}
    end.



%% Take care of this later.
%% Should fail in case of malformed encoded string
url_decode([]) -> [];
url_decode([$%,H,L|Tail]) ->
   Hex = list_to_integer([H,L],16),
   [Hex | url_decode(Tail)];
url_decode([H|Tail]) -> [H | url_decode(Tail)].
