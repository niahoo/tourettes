-module(torr_tracker).
-author("Tobias Olausson").

-import(dict,[fetch/2]).
-export([tracker/3]).

% The tracker is spawned once every time a 
% client has received a successful parse from
% the parser. The tracker talks to the mnesia
% distributed database to fetch info about torrents.

% Main function (the only exported one, too)
tracker(Type,Data,Pid) -> 
    case Action of
        announce -> 
            Hash  = fetch(<<"info_hash">>,Data), 
            IP    = fetch(<<"ip">>,Data),
            Port  = fetch(<<"port">>,Data), 
            Elem  = <<IP/binary,Port:16>>,
            Up    = fetch(<<"uploaded">>,Data),
            Down  = fetch(<<"downloaded">>,Data),
            Left  = fetch(<<"left">>,Data);

        scrape -> exit(scrape_unsup) 
    end

% Below goes a lot of handy functions
