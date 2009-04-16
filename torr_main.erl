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
-export([init/1]).

init(Port) ->
	PidServer = torr_server:init(Port),
	PidTracker = torr_tracker:init().
