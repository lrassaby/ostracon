-module(ostracon_collector).

-export([start/0]).

start() ->
    callback_module:start(),
    ets:new(voteFreq, [set, named_table, public]),
    spawn(fun() -> loop() end),
    ok.

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

loop() ->
    timer:sleep(callback_module:timeInterval()), % accumulate votes
    ets:foldr(fun(V, F) -> incrementFreq(V, F) end, voteFreq, voteDB),
    ets:delete_all_objects(voteDB),
    FreqList = ets:tab2list(voteFreq),
    % io:format("["),
    % lists:map(fun({X, Y}) ->  io:format("{~p, ~p}", [X, Y]) end, FreqList),
    % io:format("]~n"),
    ets:delete_all_objects(voteFreq),
    VoteHist = lists:reverse(lists:ukeysort(2, FreqList)),
    callback_module:updateState(VoteHist),
    loop().


