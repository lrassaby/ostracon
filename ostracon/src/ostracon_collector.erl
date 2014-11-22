-module(ostracon_collector).

-export([start/0]).

start() ->
    callback_module:start(),
    ets:new(voteFreq, [set, named_table, public]),
    spawn(fun() -> loop() end),
    ok.

% Callback module starts off the collector and specifies the time interval
% TODO: have a somewhat working callback module BEFORE moving onto the collector

incrementFreq(FreqTbl, {_, Vote}) ->
    Curr = ets:lookup(FreqTbl, Vote),
    case Curr of
        [] ->
            ets:insert(FreqTbl, {Vote, 1});
        [{_, A}] ->
            ets:insert(FreqTbl, {Vote, A + 1});
        _ ->
            error
    end,
    FreqTbl.



loop() ->
    % fold on voteDB to convert it to a Vote-Frequency table
    % then do a tab2list on the Vote-Freq table and do a sort by the second elem
    % in each tuple, the frequency
    % then delete_all_objects to empty voteDB without having to blow it up and
    % make a new one
    timer:sleep(callback_module:timeInterval()), % accumulate votes
    ets:foldr(fun(F, V) -> incrementFreq(F, V) end, voteFreq, voteDB),
    ets:delete_all_objects(voteDB),
    FreqList = ets:tab2list(voteFreq),
    ets:delete_all_objects(voteFreq),
    VoteHist = lists:reverse(lists:ukeysort(2, FreqList)),
    callback_module:updateState(VoteHist),
    loop().


