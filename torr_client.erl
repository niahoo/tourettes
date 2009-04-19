-module(torr_client).
-author("Tobias Olausson").

-import(gen_tcp,[send/2,close/1]).
-import(inet,[peername/1]).
-import(dict,[is_key/2,find/2,store/3,from_list/1]).
-import(torr_parser,[parse/2]).
-import(bval,[bencode/1,bencode_scrape/1]).

-export([handle_tcp/1,handle_udp/3]).

httpHeader(Type) -> 
   case Type of
      ok -> "HTTP/1.0 200 Ok\r\nContent-Type: text/plain\r\n\r\n";
      nf -> "HTTP/1.0 404 Not Found\n\rContent-Type: text/plain\r\n\r\n"
   end.

handle_tcp(Socket) ->
   receive
      {tcp,Socket,Data} ->
         CPid = self(),
         spawn_link(fun() -> parse(Data,CPid) end),
         handle_tcp(Socket);
      {tcp_closed,Socket} ->
         exit("socket closed");
      {tcp_error,Socket,_Reason} ->
         exit("tcp error");
      % Recieved from the parser
      {{parse_ok,Type},Data} ->
         case Type of
            scrape -> torr_tracker ! {{request,Type},Data,self()};
            announce ->
               case is_key(<<"ip">>,Data) of
                  true -> torr_tracker ! {{request,Type},Data,self()};
                  false ->
                     {ok,{IP,_}} = peername(Socket),
                     IPData = store(<<"ip">>,ip2bin(IP),Data),
                     torr_tracker ! {{request,Type},IPData,self()}
               end
         end,
         handle_tcp(Socket);
      % All responses comes from the tracker
      {{response,Type},Data} ->
         case Type of
            % Announce
            peers -> 
               io:format("Tracker responded with peers\n"),
               send(Socket,httpHeader(ok)),
               send(Socket,<<"d8:intervali900e5:peers">>),
               send(Socket,integer_to_list(sets:size(Data)*6) ++ ":"),
               send(Socket,list_to_binary(sets:to_list(Data))),
               send(Socket,<<"e">>);
            error ->
               io:format("Tracker could not find torrent\n"),
               send(Socket,httpHeader(ok)),
               send(Socket,<<"d7:failure3:404e">>);
            % Scrape
            files -> 
               io:format("Tracker responded to scrape"),
               send(Socket,httpHeader(ok)),
               send(Socket,bencode_scrape(Data));
            scrape_error -> 
               io:format("Tracker could not scrape torrent"),
               send(Socket,httpHeader(ok))
         end,
         close(Socket);
      % The parser is the only process created by the
      % client. Thus, if something fails, it is the parser
      {'EXIT',_Parser,normal} -> handle_tcp(Socket);
      {'EXIT',_Parser,Reason} ->
         io:format("Parser crashed: ~w\n",[Reason]),
         send(Socket,httpHeader(ok)),
         send(Socket,<<"d7:failure11:bad requeste">>),
         close(Socket)
   end.

handle_udp(Host,Port,Data) ->
   io:format("Received datagram from ~w\n",[Host]),
   Size = bit_size(Data) div 8,
   case Size >= 16 of
      false -> exit("bad_packet_size");
      true -> 
         <<ConnID:64,Action:32,TransID:32,Rest/binary>> = Data,
         case ConnID == 41727101980 of % BitTorrent UDP identifier
            false -> exit("not_torrent_packet");
            true -> 
               io:format("Packet is valid, action is ~w\n",[Action]),
               case Action of
                  % Connection
                  0 ->
                     Response = <<0:32,TransID:32,ConnID:64>>,
                     udp_server ! {reply,Host,Port,Response};
                  1 -> case Size >= 96 of
                        false -> exit("bad_packet_size");
                        true ->
                           <<Hash:160, PeerID:160, Down:64,
                              Left:64, Up:64, Event:32, _IP:32, Key:32,
                              NumWant:32, _Port2:16, Crap/binary>> = Rest,
                              % Quite inefficient
                              Req = store(<<"info_hash">>,Hash,store(<<"ip">>,Host,store(<<"port">>,Port,dict:new()))),
                              io:format("Announcing\n"),
                              torr_tracker ! {{request,announce},Req,self()},
                              receive
                                 {{response,peers},Peers} ->
                                    io:format("Got peers\n"),
                                    Head = <<1:32,TransID:32,900:32,0:32,0:32>>,
                                    PS = list_to_binary(sets:to_list(Peers)),
                                    udp_server ! {reply,Host,Port,<<Head/binary,PS/binary>>};
                                 {{response,error},Data} ->
                                    io:format("No data found\n"),
                                    udp_server ! {reply,Host,Port,<<1:32,TransID:32,900:32,0:32,0:32>>}
                              end
                      end;
                  2 -> ok %scrape
               end
         end
   end.
      



ip2bin({A,B,C,D}) -> <<A,B,C,D>>.
