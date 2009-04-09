-module(torr_parser).
-author("Tobias & David").

-import(string,[tokens/2]).
-import(lists,[map/2,all/2]).
-import(dict,[from_list/1,find/2]).
-import(erlang,[list_to_integer/2]).

-export([parse/2]).

parse(<<"GET /announce?",Request/binary>>,Pid) -> 
    PairList = split(Request),
    case announce(PairList) of

parse(<<"GET /scrape?",Request/binary>>,Pid) -> 
    Reply = parse_scrape(Request),
    Pid ! {scrape_parse,Request}.

% Should split the request into key/value pairs
split(Request) -> ok.


%% Build this into split?
%% Take care of this later.
%% Should fail in case of malformed encoded string
url_decode([]) -> [];
url_decode([$%,H,L|Tail]) ->
   Hex = list_to_integer([H,L],16),
   [Hex | url_decode(Tail)];
url_decode([H|Tail]) -> [H | url_decode(Tail)].
