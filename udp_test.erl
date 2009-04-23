-module(udp_test).
-export([start_server/0]).

start_server() ->
   spawn(fun() -> server(60667) end).

server(Port) ->
   {ok,Socket} = gen_udp:open(Port,[binary]),
   io:format("Server started"),
   loop(Socket).

loop(Socket) ->
   receive
      {udp,Socket,Host,Port,Data} ->
         gen_udp:send(Socket,Host,Port,<<"boeg">>),
         loop(Socket)
   end.
