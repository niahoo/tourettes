-module(torr_client).
-author("Tobias Olausson").

-import(gen_tcp,[send/2,close/1]).
-import(inet,[peername/1]).
-import(dict,[is_key/2,find/2,store/3,from_list/1]).
-import(torr_parser,[parse/2]).
-import(bval,[bencode/1,bencode_scrape/1]).

-export([handle/1]).

httpHeader(Type) -> 
   case Type of
      ok -> "HTTP/1.0 200 Ok\r\nContent-Type: text/plain\r\n\r\n";
      nf -> "HTTP/1.0 404 Not Found \n\rContent-Type: text/plain\r\n\r\n"
   end.

handle(Socket) ->
   receive
      {tcp,Socket,Data} ->
         CPid = self(),
         io:format("Received: ~w\n",[Data]),
         spawn_link(fun() -> parse(Data,CPid) end),
         handle(Socket);
      {tcp_closed,Socket} ->
         exit(closed);
      {tcp_error,Socket,Reason} ->
         exit(Reason);
      % Recieved from the parser
      {{parse_ok,Type},Data} ->
         io:format("Parse ok: ~w\n",[Data]),
         torr_tracker ! {{request,Type},Data,self()},
         handle(Socket);
      % All responses comes from the tracker
      {{response,Type},Data} ->
         io:format("Tracker response: ~w\n",[Data]),
         case Type of
            % Announce
            peers -> 
               send(Socket,httpHeader(ok)),
               send(Socket,"d8:intervali900e5:peers"),
               send(Socket,sets:size(Data)*6 ++ ":"),
               send(Socket,list_to_binary(sets:to_list(Data)));
            error ->
               send(Socket,httpHeader(nf)),
               send(Socket,"d8:failure3:404");
            % Scrape
            files -> 
               send(Socket,httpHeader(ok)),
               send(Socket,bencode_scrape(Data));
            scrape_error -> send(Socket,httpHeader(nf))
         end,
         close(Socket);
      % The parser is the only process created by the
      % client. Thus, if something fails, it is the parser
      {'EXIT',_Parser,normal} -> handle(Socket);
      {'EXIT',_Parser,Reason} ->
         io:format("Parser crashed: ~w\n",[Reason]),
         send(Socket,httpHeader(ok)),
         send(Socket,"d7:failure11:bad requeste"),
         close(Socket)
   end.
