-module(ostracon_collector).
% TODO: connect to everything else
% exports: start/2, taking in time interval and callback module
% also has: loop/2, with same args

% TODO: loop collects votes from ETS and empties it, then
% aggregates the votes and sends them as a descending histogram to the callback
% module

-export([start/2]).

start(TimeInt, CBackMod) ->
    ets:new(voteFreq, [set, named_table, private]),
    loop(TimeInt, CBackMod).

% Callback module starts off the collector and specifies the time interval
% TODO: have a somewhat working callback module BEFORE moving onto the collector

loop(TimeInt, CBackMod) ->
    % fold on voteDB to convert it to a Vote-Frequency table
    % then do a tab2list on the Vote-Freq table and do a sort by the second elem
    % in each tuple, the frequency
    % then delete_all_objects to empty voteDB without having to blow it up and
    % make a new one
    timer:sleep(TimeInt), % accumulate votes
    ets:foldr(incrementFreq, voteFreq, voteDB),
    ets:delete_all_objects(voteDB),
    FreqList = ets:tab2list(voteFreq),
    ets:delete_all_objects(voteFreq),
    VoteHist = lists:reverse(lists:ukeysort(2, FreqList)),
    CBackMod ! {self(), voteFreq, VoteHist},
    loop(TimeInt, CBackMod).

incrementFreq(FreqTbl, {_, Vote}) ->
    Curr = ets:lookup(FreqTbl, Vote),
    case Curr of
        [] ->
            ets:insert(FreqTbl, {Vote, 1});
        [{_, A}] ->
            ets:insert(FreqTbl, {Vote, A + 1});
        _ ->
            error
    end
    FreqTbl.


