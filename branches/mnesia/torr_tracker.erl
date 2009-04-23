-module(torr_tracker).
-author("Tobias Olausson").

-include_lib("stdlib/include/qlc.hrl").
-import(dict,[fetch/2]).
-export([init/0]).

-record(torrent,{
      info_hash, ipport,
      down, up, left, seeding}).

init() ->
   Pid = spawn_link(fun() -> tracker_init() end),
   register(torr_tracker,Pid),
   Pid.

tracker_init() ->
   torr_db:init(),
   tracker().

tracker() -> 
   receive 
      {{request,Action},Data,Pid} ->
         case Action of
            announce -> 
               Hash  = fetch(<<"info_hash">>,Data), 
               IP    = fetch(<<"ip">>,Data),
               Port  = fetch(<<"port">>,Data), 
               Elem  = <<IP/binary,Port:16>>,
               Up    = fetch(<<"uploaded">>,Data),
               Down  = fetch(<<"downloaded">>,Data),
               Left  = fetch(<<"left">>,Data),
             
               % Fetching matching data from mnesia
               Peers = mnesia_query(qlc:q([ X || X <- mnesia:table(torrent), 
                                          X#torrent.info_hash =:= Hash ])),
               io:format("Result from mnesia: ~w\n",[Peers]),

               Tuple = #torrent{info_hash = Hash, ipport = Elem,
                                down = Down, up = Up, left = Left,
                                seeding = true};
               
            scrape -> Pid ! {{response,scrape_error},<<"Not supported">>}
         end,
         tracker()
   end.

