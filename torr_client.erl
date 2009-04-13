-module(torr_client).
-author("Tobias Olausson").

-import(gen_tcp,[send/2,close/1]).
-import(inet,[peername/1]).
-import(string,[str/2,substr/2,substr/3,tokens/2,concat/2]).
-import(dict,[is_key/2,find/2,store/3,from_list/1,to_list/1]).
-import(lists,[map/2,any/2,takewhile/2,dropwhile/2,reverse/1,foreach/2]).
-import(torr_parser,[parse_req/2]).
-import(bval,[bencode/1,bencode_bin/1]).

-export([handle/1]).
-compile(export_all).

handle(Socket) ->
    receive
        {tcp,Socket,Data} ->
            CPid = self(),
            io:format("Received data:~p\n",[Data]),
            spawn(fun() -> parse_req(Data,CPid) end),
            handle(Socket);
        {parse_ok,Dict} ->
            case is_valid(Dict) of
               true ->
                  case is_key(<<"ip">>,Dict) of
                     true -> torr_tracker ! {request,{self(),Dict}};
                     false ->
                        {ok,{IP,_}} = peername(Socket),
                        IPDict = store(<<"ip">>,ip2bin(IP),Dict),
                        torr_tracker ! {request,{self(),IPDict}}
                  end;
               false -> self() ! parse_fail
            end,
            handle(Socket);
         accept ->
            send(Socket,"HTTP/1.0 200 OK\r\n"),
            handle(Socket);
        torrent_fail ->
            io:format("Torrent not found\n"),
            self() ! parse_fail,
            handle(Socket);
        {peers,PeerDict} ->
            Pre = "d8:intervali900e5:peers",
            {Size,Bval} = bencode_bin(PeerDict),
            Response = Pre ++ Size ++ ":" ++ binary_to_list(Bval) ++ "e",
            io:format("Sending response: ~w\n",[Response]),
            send(Socket,"HTTP/1.0 200 OK\r\n"),
            send(Socket,"Content-Type: text/plain\r\n\r\n"),
            send(Socket,Response),
            close(Socket);
        parse_fail -> 
            io:format("Parse failure\n"),
            send(Socket,"HTTP/1.0 200 OK\r\n"),
            send(Socket,"Content-Type: text/plain\r\n\r\n"),
            send(Socket,"d7:failure11:Bad requeste"),
            close(Socket)
    end.

% Does it contain info_hash, peer_id and port?
is_valid(Dict) -> is_key(<<"info_hash">>,Dict) and 
         is_key(<<"peer_id">>,Dict) and
         is_key(<<"port">>,Dict).

ip2bin({A,B,C,D}) -> <<A,B,C,D>>.
ip_to_binary({A,B,C,D}) -> [integer_to_list(A),$.,integer_to_list(B),$.,
   integer_to_list(C),$.,integer_to_list(D)].
