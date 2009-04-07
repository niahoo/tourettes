-module(bval).
-export([bencode/1,bdecode/1]).


bencode(List) when is_list(List) ->
	[$l, [bencode(X) || X <- List], $e];

bencode(Int) when is_integer(Int) ->
	[$i,integer_to_list(Int),$e];

bencode(Str) when is_binary(Str) ->
	[integer_to_list(size(Str)),$:,Str];

bencode(Dict) when element(1,Dict) == dict ->
	[$d, [[bencode(Key), bencode(Val)] || {Key, Val} <- dict:to_list(Dict)], $e].

bdecode([$i | Rest]) ->
 	End = string:chr(Rest, $e),
	(lists:sublist(Rest,1,End-1));

bdecode([Int | Rest]) when Int >= $0, Int =< $9 ->
	lists:sublist(Rest,2,Int).

%% bdecode([$l | Rest]) ->
%% 	decodeList(Rest);

%% bdecode([$d | Rest]) ->
%% 	decodeDict(Rest).

%% decodeList(List) ->
%% 	List;

%% decodeDict(Dict) ->
%% 	Dict.

%% bdecode([Char | Rest]) -> 
%% 	case Char of
%% 		{$l} ->  

%%%is_string(Str) -> [and is_char(Char) || Char <- Str].

%% is_string([_Char | _Rest] = Str) when is_list(Str) ->
%% 	lists:all(fun(C) -> is_char(C) end, Str);
%% is_string(_) -> false.

%% is_char(Char) -> Char >= 32 andalso Char =< 255.
