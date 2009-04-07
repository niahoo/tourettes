-module(torr_client).
-author("Tobias Olausson").

-import(gen_tcp,[send/2,close/1]).
-import(string,[str/2,substr/2,substr/3,tokens/2,concat/2]).
-import(dict,[is_key/2,find/2,store/3,from_list/1]).
-import(lists,[map/2,any/2,takewhile/2,dropwhile/2,reverse/1,foreach/2]).
-export([handle/1]).

handle(Socket) ->
    receive
        {tcp,Socket,Data} -> 
            spawn(fun() -> parse(Data,self()) end),
            handle(Socket);
        {parsed,Dict} ->
            case is_valid(Dict) of
                true    -> ok;
                false   -> ok
            end,
            handle(Socket);
        {response,Resp} ->
            % BEncode
            send(Socket,Resp),
            close(Socket);
        fail -> 
            send(Socket,"d7:failure11:Bad requeste"),
            close(Socket)
    end.

% Does it contain info_hash, peer_id and port?
is_valid(Dict) -> 
    case is_key(info_hash,Dict) and is_key(peer_id,Dict) of
        true    -> ok;
        false   -> ok
    end.

% Parse a GET request
parse(GetReq,Pid) ->
    case str(GetReq,"GET /?") of
        1 ->
            Stripped = takewhile(fun(A) -> A /= $ end,substr(GetReq,7)),
            Parts = tokens(Stripped,"&"),
            SFun  = fun(Elem) -> tokens(Elem,"=") end,
            Pairs  = map(SFun,Parts),
            case any(fun(Elem) -> length(Elem) /= 2 end,Pairs) of
                true  -> Pid ! fail;
                false -> 
                    TupleList = map(fun(E) -> list_to_tuple(E) end, Pairs),
                    Result = map(fun({Key,Val}) -> 
                        {list_to_atom(Key),urldecode(Val)} end,TupleList),
                    Pid ! {parsed, from_list(Result)}
            end;
        _ -> Pid ! fail
    end.

% öh...
from_hex(Str) -> erlang:list_to_integer(Str,16).
from_hexStr(Str) ->
    case length(Str) of
        0 -> error;
        1 -> Str;
        2 -> from_hex(Str);
        _ -> 
            Dec = from_hex(substr(Str,1,2)),
            [ Dec | substr(Str,3) ]
    end.

% Hehe
urldecode([]) -> [];
urldecode(Str) ->
    case hd(Str) of
        $% ->
            Parts = tokens(Str,"%"),
            F = fun(Part) -> from_hexStr(Part) end,
            Encode = map(F,Parts),
            lists:flatten(Encode);
        _ ->
            Parts = tokens(Str,"%"),
            lists:flatten([ hd(Parts) | urldecode(tl(Parts))])
    end.
