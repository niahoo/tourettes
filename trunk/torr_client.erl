-module(torr_client).
-author("Tobias Olausson").

-import(gen_tcp,[send/2,close/1]).
-import(inet,[peername/1]).
-import(string,[str/2,substr/2,substr/3,tokens/2,concat/2]).
-import(dict,[is_key/2,find/2,store/3,from_list/1,to_list/1]).
-import(lists,[map/2,any/2,takewhile/2,dropwhile/2,reverse/1,foreach/2]).
-import(torr_parser,[parse_req/2]).
-import(bval,[bencode/1]).

-export([handle/1]).
-compile(export_all).

handle(Socket) ->
    receive
        {tcp,Socket,Data} ->
            CPid = self(),
            spawn(fun() -> parse_req(Data,CPid) end),
            handle(Socket);
        {parse_ok,Dict} ->
            case is_valid(Dict) of
                true ->
                    send(Socket,"parse_ok\n"),
		    case is_key(<<"ip">>,Dict) of
			true -> torr_tracker ! {request,{self(),Dict}};
                    	false ->
			   {ok,{IP,_}} = peername(Socket),
			   IPDict = store(<<"ip">>,ip_to_binary(IP),Dict),
			   torr_tracker ! {request,{self(),IPDict}}
			end,
			handle(Socket);
                false ->
                    send(Socket,"parse_fail\n"),
                    handle(Socket)
            end;
        torrent_fail -> 
            send(Socket,"torrent fail\n"),
            close(Socket);
        {peers,PeerDict} ->
	    send(Socket,"d8:intervali900e5:peers"),
            send(Socket,bencode(PeerDict)),
	    send(Socket,"e"),
	    close(Socket);
        parse_fail -> 
            send(Socket,"d7:failure11:Bad requeste"),
            close(Socket)
    end.

% Does it contain info_hash, peer_id and port?
is_valid(Dict) -> is_key(<<"info_hash">>,Dict) and 
         is_key(<<"peer_id">>,Dict) and
         is_key(<<"port">>,Dict).

ip_to_binary({A,B,C,D}) -> list_to_binary(
	[integer_to_list(A),$.,integer_to_list(B),$.,
	integer_to_list(C),$.,integer_to_list(D)]).



