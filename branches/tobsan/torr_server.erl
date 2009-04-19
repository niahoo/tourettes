-module(torr_server).
-author("Tobias Olausson").

-import(gen_tcp,[listen/2,accept/1,controlling_process/2]).
-import(gen_udp,[open/2,send/4]).
-import(torr_client,[handle_tcp/1,handle_udp/3]).

-export([init/2]).

init(tcp,Port) ->
   case listen(Port,[binary]) of
      {ok,Listen} ->
         Pid = spawn(fun() ->
                  process_flag(trap_exit,true),
                  tcp_loop(Listen)
            end),
         register(tcp_server,Pid),
         Pid;
      {error,Reason} -> exit(Reason)
   end;
init(udp,Port) ->
   case open(Port,[binary]) of
      {ok,Socket} ->
         Pid = spawn(fun() ->
                  process_flag(trap_exit,true),
                  udp_loop(Socket)
            end),
         register(udp_server,Pid),
         Pid;
      {error,Reason} -> exit(Reason)
   end.


udp_loop(Socket) ->
   io:format("Looping on UDP Socket\n"),
   receive
      {udp,Socket,Host,Port,Data} ->
         spawn_link(fun() -> handle_udp(Host,Port,Data) end);
      {reply,Address,Port,Data} -> send(Socket,Address,Port,Data);
      {'EXIT',_Pid,Reason} -> 
         io:format("UDP Client terminated with reason:~w\n",[Reason])
   end.

tcp_loop(Listen) ->
   io:format("Looping on TCP Socket\n"),
   case accept(Listen) of
      {ok,Socket} -> 
         {ok,{IP,_}} = inet:peername(Socket),
         io:format("~w connected\n",[IP]),
         Pid = spawn_link(fun() -> 
                  process_flag(trap_exit,true),
                  handle_tcp(Socket)
               end),
         controlling_process(Socket,Pid),
         tcp_loop(Listen);
      {'EXIT',Pid,_Reason} -> 
         io:format("TCP Client ~w terminated\n",[Pid]),
         tcp_loop(Listen);
      {error,Reason} -> exit(Reason)
   end.
