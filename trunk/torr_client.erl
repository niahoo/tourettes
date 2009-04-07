-module(torr_client).
-author("Tobias Olausson").

-import(gen_tcp,[send/2,close/1]).
-import(string,[str/2,substr/2,substr/3,tokens/2,concat/2]).
-import(dict,[is_key/2,find/2,store/3,from_list/1]).
-import(lists,[map/2,any/2,takewhile/2,dropwhile/2,reverse/1,foreach/2]).
-import(torr_parser,[parse_req/2]).
-import(bval,[bencode/1]).

-export([handle/1]).

handle(Socket) ->
    receive
        {tcp,Socket,Data} -> 
            spawn(fun() -> parse_req(Data,self()) end),
            handle(Socket);
        {parse_ok,Dict} ->
            case is_valid(Dict) of
                true    -> ok;
                false   -> ok
            end,
            handle(Socket);
        {peers,PeerDict} ->
            Resp = bencode(PeerDict),
            send(Socket,Resp),
            close(Socket);
        parse_fail -> 
            send(Socket,"d7:failure11:Bad requeste"),
            close(Socket)
    end.

% Does it contain info_hash, peer_id and port?
is_valid(Dict) -> true.
