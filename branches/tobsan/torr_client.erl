-module(torr_client).
-author("Tobias Olausson").

-import(gen_tcp,[send/2,close/1]).
-import(inet,[peername/1]).
-import(dict,[is_key/2,find/2,store/3,from_list/1]).
-import(torr_parser,[parse/2]).
-import(bval,[bencode/1]).

-export([handle/1]).

handle(Socket) ->
   receive
      {tcp,Socket,Data} ->
         CPid = self(),
         spawn_link(fun() -> parse(CPid,Data) end),
         handle(Socket);
      {tcp_closed,Socket} ->
         exit(closed);
      {tcp_error,Socket,Reason} ->
         exit(Reason);
      {parse_ok,Dict} ->
         torr_tracker ! {request,Dict,self()},
         handle(Socket);
      {scrape_ok,List} ->
         %torr_tracker ! {scrape,List,self()},
         send(Socket,"HTTP/1.1 404 Not Found\r\n"),
         handle(Socket);
      {'EXIT',_Parser,Reason} -> 
         % Parser failed
         send(Socket,"d7:failure11:bad requeste"),
         close(Socket)
   end.
