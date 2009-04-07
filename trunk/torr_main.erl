%%%-------------------------------------------------------------------
%%% File    : torr_main.erl
%%% Authors  : Tobias O & David H
%%% Description : 
%%%
%%% Created : 05 Apr 2009
%%%-------------------------------------------------------------------
-module(torr_main).
% -import(tracker_simple,[init/0]).
-import(torr_server,[init/1]).
-export([init/0,test/1]).

init() ->
	PidServer = torr_server:init(60666),
	PidTracker = tracker_simple:init().

%	test(PidTracker).

test(Pid) ->
	Pid ! {add, "AARNE"},
	Pid ! {request,{self(),"AARNE",id,ip,port}},
	receive
		{peers,Peers} -> io:format("~w",[dict:to_list(Peers)]);
		fail -> io:format("No AARNE :c")
	end.
