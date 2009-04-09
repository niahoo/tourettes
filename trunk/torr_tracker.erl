%%%-------------------------------------------------------------------
%%% File    : tracker_simple.erl
%%% Authors  : Tobias O & David H
%%% Description : 
%%%
%%% Created : 05 Apr 2009
%%%-------------------------------------------------------------------
-module(tracker_simple).

-import(dict,[find/2, new/0, store/3, append/3, fetch/2]).

-export([init/0]).

init() ->
	Pid = spawn(fun() -> tracker(new()) end),
	register(torr_tracker,Pid),
	Pid.

tracker(Torrents) ->
	receive
		{request,{Pid,ReqDict}} -> 
			Hash = fetch("info_hash", ReqDict),
			case find(Hash,Torrents) of
				{ok,Peers} -> 
					Pid ! {peers,Peers},
					ID = fetch("peer_id",ReqDict),
					case find(ID,Peers) of
						{ok,_} -> tracker(Torrents);
						error -> 
							IP = fetch("ip",ReqDict),
							Port = fetch("port",ReqDict),
							NewPeers = store(ID,{IP,Port},Peers),
							tracker(store(Hash,NewPeers,Torrents))
					end;
				error -> Pid ! torrent_fail
			end;
%
%		{request,{Pid,Hash,Id,Ip,Port}} ->
%			case find(Hash,Dict) of
%				{ok, Peers} ->
%					Pid ! {peers,Peers},
%					case find(Id,Peers) of
	%					{ok,_} -> tracker(Dict);
		%				error -> 
			%				NewPeers = store(Id,{Ip,Port},Peers),
				%			tracker(store(Hash,NewPeers,Dict))
%					%end;
	%			error -> Pid ! fail,
		%				 tracker(Dict)
			%end;		   

	{add,Hash} -> 
		case find(Hash,Torrents) of
			{ok,_} -> tracker(Torrents);
				error ->  tracker(store(Hash,new(),Torrents))
			end
	end.
