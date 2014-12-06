-module(ostracon_collector).

-export([start/0]).

% Starts the callback module and collector, and initializes the voteFreq ETS, 
% which is used to sum the frequencies for large numbers of votes
start() ->
    callback_module:start(),
    ets:new(voteFreq, [set, named_table, public]),
    spawn(fun() -> loop() end),
    ok.

% Used for calculation of histogram in loop below
incrementFreq({_, {Vote, Team}}, FreqTbl) ->
    Curr = ets:lookup(FreqTbl, {Vote, Team}),
    case Curr of
        [] ->
            ets:insert(FreqTbl, {{Vote, Team}, 1});
        [{_, A}] ->
            ets:insert(FreqTbl, {{Vote, Team}, A + 1});
        _ ->
            error
    end,
    FreqTbl.

% Tail recursive loop to accumulate votes
loop() ->
    timer:sleep(callback_module:timeInterval()), % accumulate votes
    ets:foldr(fun(V, F) -> incrementFreq(V, F) end, voteFreq, voteDB),
    ets:delete_all_objects(voteDB),
    FreqList = ets:tab2list(voteFreq),
    ets:delete_all_objects(voteFreq),
    VoteHist = lists:reverse(lists:ukeysort(2, FreqList)),
    callback_module:updateState(VoteHist),
    loop().


