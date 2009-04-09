-module(torr_tracker).
-import(dict,[find/2, new/0, store/3, append/3, fetch/2]).
-export([init/0]).

init() ->
	Pid = spawn_link(fun() -> tracker(new()) end),
	register(torr_tracker,Pid),
	Pid.

tracker(Torrents) ->
	receive
		{request,{Cid,ReqDict}} ->
			Hash = fetch(<<"info_hash">>, ReqDict),
			case find(Hash,Torrents) of
				{ok,Peers} -> 
					Cid ! {peers,Peers},
					ID = fetch(<<"peer_id">>,ReqDict),
					case find(ID,Peers) of
						{ok,_} -> tracker(Torrents);
						error ->
							IP = fetch(<<"ip">>,ReqDict),
							Port = fetch(<<"port">>,ReqDict),
							NewPeers = store(ID,{IP,Port},Peers),
							tracker(store(Hash,NewPeers,Torrents))
					end;
				error -> 
                                    Cid ! torrent_fail,
                                    tracker(Torrents)
			end;

                {add,Hash} -> 
		    case find(Hash,Torrents) of
			{ok,_} -> tracker(Torrents);
			error ->  tracker(store(Hash,new(),Torrents))
		    end
	end.
