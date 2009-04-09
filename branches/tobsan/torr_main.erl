%%%-------------------------------------------------------------------
%%% File    : torr_main.erl
%%% Authors  : Tobias O & David H
%%% Description : 
%%%
%%% Created : 05 Apr 2009
%%%-------------------------------------------------------------------
-module(torr_main).
% -import(tracker_simple,[init/0]).
% -import(torr_server,[init/1]).
-export([init/1,test/1]).

init(Port) ->
	PidServer = torr_server:init(Port),
	PidTracker = tracker_simple:init().
%	torr_tracker ! {add, "AARNE"}.
	


%	test(PidTracker).

test(Pid) ->
	Pid ! {add, "AARNE"},
	Pid ! {request,{self(),"AARNE",1,2,3}},
	receive
		{peers,Peers} -> io:format("~w",[(Peers)]);
		fail -> io:format("No AARNE :c")
	end.
