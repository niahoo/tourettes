-module(torr_server).
-author("Tobias Olausson").

-import(gen_tcp,[listen/2,accept/1,controlling_process/2]).
-import(torr_client,[handle/1]).

-export([init/1]).

init(Port) ->
   case listen(Port,[binary]) of
      {ok,Listen} ->
         Pid = spawn(fun() ->
                  process_flag(trap_exit,true),
                  server(Listen)
            end),
         register(torr_server,Pid),
         Pid;
      {error,Reason} -> exit(Reason)
   end.

server(Listen) ->
   case accept(Listen) of
      {ok,Socket} -> 
         Pid = spawn_link(fun() -> 
                  process_flag(trap_exit,true),
                  handle(Socket)
               end),
         controlling_process(Socket,Pid);
      {'EXIT',Pid,_Reason} -> 
         io:format("Client ~w terminated",[Pid]),
         handle(Listen);
      {error,Reason} -> exit(Reason)
   end.
