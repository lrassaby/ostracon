-module(ostracon_collector).
% TODO: connect to everything else
% exports: start/2, taking in time interval and callback module
% also has: loop/2, with same args

% TODO: loop collects votes from ETS and empties it, then
% aggregates the votes and sends them as a descending histogram to the callback
% module

-export([start/2]).

start(TimeInt, CBackMod) ->
% Callback module starts off the collector and specifies the time interval
% TODO: have a somewhat working callback module BEFORE moving onto the collector
