-module(torr_tracker).
-author("Tobias Olausson").

-import(dict,[new/0,is_key/2,store/3,erase/2]).

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
      {request,Dict,Pid} -> ok;
      {scrape,List,Pid} -> ok;
      {add,Hash} -> 
         case is_key(Hash,Torrents) of
            true -> tracker(Torrents);
            false -> tracker(store(Hash,new(),Torrents))
         end;
      {remove,Hash} -> tracker(erase(Hash,Torrents))
   end.
