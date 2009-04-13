-module(bval). 
-export([bencode/1,bencode_bin/1]).
-import(dict,[fold/3]).

bencode(List) when is_list(List) ->
	[$l, [bencode(X) || X <- List], $e];

bencode(Int) when is_integer(Int) ->
	[$i,integer_to_list(Int),$e];

bencode(Str) when is_binary(Str) ->
	[integer_to_list(size(Str)),$:,binary_to_list(Str)];

bencode({Key,Val}) -> [bencode(Key),bencode(Val)];

% Somewhat inefficient
bencode(Dict) when element(1,Dict) == dict ->
	[$d,[[bencode(Key), bencode(Val)] || {Key, Val} <- dict:to_list(Dict)], $e].

% This bencodes peers using the binary model representation
bencode_bin(Dict) when element(1,Dict) == dict -> 
   Bin = fold(fun(_Key,Val,Acc) -> bencode_bin_elem(Val,Acc) end, <<>>, Dict),
   Size = dict:size(Dict) * 6,   
   {integer_to_list(Size),Bin}.
bencode_bin_elem({{_,IP},{_,Port}},Acc) -> <<Acc/binary,IP/binary,Port:16>>.

