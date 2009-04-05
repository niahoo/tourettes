-module(torr_server).
-author("Tobias Olausson & David Hadzic").

-import(gen_tcp,[accept/1,listen/2,controlling_process/2]).
-import(inet,[peername/1]).
-import(torr_client,[handle/1]).
-export([init/1]).

init(Port) ->
   case listen(Port,[list,{active,true}]) of
      {ok,Listen} -> Pid = spawn(fun() -> loop(Listen) end),
					 register(torr_connector, Pid);
      {error,_} -> failed
   end.


loop(Listen) ->
   case accept(Listen) of
      {ok,Socket} ->
         {ok,{IP,_}} = peername(Socket),
         io:format("IP:~w connected \n",[IP]),
         Pid = spawn(fun() -> handle(Socket) end),
         controlling_process(Socket,Pid),
         loop(Listen);
      {error,Reason} -> {fail,Reason}
   end.
