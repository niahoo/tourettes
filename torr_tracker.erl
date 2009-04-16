-module(torr_tracker).
-author("Tobias Olausson").

-import(dict,[new/0,is_key/2,store/3,erase/2,fetch/2,find/2]).
-import(sets,[add_element/2]).

-export([init/0]).

init() ->
   Pid = spawn_link(fun() -> 
            process_flag(trap_exit,true),
            tracker(new()) 
         end),
   register(torr_tracker,Pid),
   Pid.

tracker(Torrents) -> 
   receive 
      {{request,Type},Data,Pid} ->
         case Type of
            announce -> 
               Hash = fetch(<<"info_hash">>,Data),
               IP = fetch(<<"ip">>,Data),
               Port = fetch(<<"port">>,Data),
               Elem = <<IP/binary,Port:16>>,
               case find(Hash,Torrents) of
                  {ok,Peers} -> 
                     Pid ! {{response,peers},Peers},
                     NewPeers = add_element(Elem,Peers),
                     tracker(store(Hash,NewPeers,Torrents));
                  error -> 
                     Pid ! {{response,error},<<"not found">>},
                     % REALLY open
                     PeerSet = add_element(Elem,sets:new()),
                     tracker(store(Hash,PeerSet,Torrents))
               end;
            scrape -> 
               Pid ! {{response,scrape_error},<<"Not supported">>},
               tracker(Torrents)
         end;
      {add,Hash} -> 
         case is_key(Hash,Torrents) of
            true -> tracker(Torrents);
            false -> tracker(store(Hash,sets:new(),Torrents))
         end;
      {remove,Hash} -> tracker(erase(Hash,Torrents))
   end.
