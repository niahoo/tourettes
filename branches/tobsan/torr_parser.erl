-module(torr_parser).
-author("Tobias & David").

-import(string,[tokens/2]).
-import(lists,[map/2,all/2]).
-import(dict,[from_list/1,find/2]).
-import(erlang,[list_to_integer/2]).

-export([parse/2]).
-compile(export_all).

parse(<<"GET /announce?",Request/binary>>,Pid) -> 
    PairList = split($&,Request,<<>>),
    Pid ! {parse_ok,PairList}.

% Should split the request into key/value pairs
split(_Char,<<>>,<<>>) -> [];
split(_Char,<<>>,Acc) -> [Acc];
split(Char,<<Char,Rest/binary>>,<<>>) -> split(Char,Rest,<<>>);
split(Char,<<Char,Rest/binary>>,Acc) -> 
    Part = url_decode(Acc),
    [ <<Part/binary>> | split(Char,Rest,<<>>) ];
split(Char,<<Data:8,Rest/binary>>,<<>>) -> split(Char,Rest,<<Data>>);
split(Char,<<Data:8,Rest/binary>>,Acc) -> split(Char,Rest,<<Acc/binary,Data>>).

% TODO write this
url_decode(String) -> ok.
