%%%-------------------------------------------------------------------
%%% File    : tracker_simple.erl
%%% Authors  : Tobias O & David H
%%% Description : 
%%%
%%% Created : 05 Apr 2009
%%%-------------------------------------------------------------------
-module(tracker_simple).
-import(dict,[find/2, new/0, store/3, append/3]).
-export([init/0]).

init() ->
	Pid = spawn(fun() -> tracker(new()) end),
	register(torr_tracker,Pid),
	Pid.

tracker(Dict) ->
	receive
		{request,{Pid,Hash,Id,Ip,Port}} ->
			case find(Hash,Dict) of
				{ok, Peers} ->
					Pid ! {peers,Peers},
					case find(Id,Peers) of
						{ok,_} -> tracker(Dict);
						error -> 
							NewPeers = store(Id,{Ip,Port},Peers),
							tracker(store(Hash,NewPeers,Dict))
					end;
				error -> Pid ! fail,
						 tracker(Dict)
			end;		   

		{add,Hash} -> 
			case find(Hash,Dict) of
				{ok,_} -> tracker(Dict);
				error ->  tracker(store(Hash,new(),Dict))
			end
	end.
