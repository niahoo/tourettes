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
            process_flag(trap_exit, true),
            spawn_link(fun() -> parse(CPid,Data) end),
            handle(Socket);
        {tcp_closed,Socket} ->
            exit(closed);
        {tcp_error,Socket,Reason} ->
            exit(Reason);
        {parse_ok,Data} ->
            torr_tracker ! {request,Data},
            handle(Socket);
        {'EXIT',_Parser,Reason} -> 
            % Parser failed
            send(Socket,"d7:failure11:bad requeste"),
            close(Socket)
    end.

% Verifies that the parsed data have the required field
verify(Data) -> ok.
