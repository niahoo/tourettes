-module(torr_tracker).
-author("Tobias Olausson").

-import(dict,[new/0,is_key/2,store/3,erase/2,fetch/2,find/2]).
-import(lists,[keysearch/3]).

-export([init/0]).

init() ->
   Pid = spawn_link(fun() -> 
            process_flag(trap_exit,true),
            TableID = ets:new(torr_db,[bag,private]),
            tracker(TableID) 
         end),
   register(torr_tracker,Pid),
   Pid.

tracker(TableID) -> 
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
               
               case ets:lookup(TableID,Hash) of
                  []       -> Pid ! {{response,error},<<"not found">>};
                  TorrData ->
                     Peers = [ Peer || {_Hash,Peer} <- TorrData],
                     io:format("Stripped TorrData: ~w\n",[Peers]),
                     Pid ! {{response,peers},Peers}
               end,

               Value = {Elem,Up,Down,Left},
               ets:insert(TableID,{Hash,Value}),
               tracker(TableID);
            
            scrape -> 
               Pid ! {{response,scrape_error},<<"Not supported">>},
               tracker(TableID)
         end
   end.

