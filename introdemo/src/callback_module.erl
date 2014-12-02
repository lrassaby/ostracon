-module(callback_module).

-export([reset/0, start/0, timeInterval/0, updateState/1]).

makeInitialStores() ->
    ets:insert(stateDB, {left, 0}),
    ets:insert(stateDB, {right, 0}),
    ets:insert(stateDB, {up, 0}),
    ets:insert(stateDB, {down, 0}).


reset() ->
    ets:delete_all_objects(stateDB),
    makeInitialStores(),
    stateDB.


start() ->
    reset().

timeInterval() ->
    16.


updateState(Votes) ->
    countVotes(Votes).

countVotes([{{Vote, _Keystroke}, Freq}|Rest]) ->
    case Vote of
        << "up" >> -> 
            [{_, Total}|_] = ets:lookup(stateDB, up),
            ets:insert(stateDB, {up, Total+Freq});
        << "down" >> -> 
            [{_, Total}|_] = ets:lookup(stateDB, down),
            ets:insert(stateDB, {down, Total+Freq});
        << "left" >> -> 
            [{_, Total}|_] = ets:lookup(stateDB, left),
            ets:insert(stateDB, {left, Total+Freq});
        << "right" >> -> 
            [{_, Total}|_] = ets:lookup(stateDB, right),
            ets:insert(stateDB, {right, Total+Freq});
        _ ->
            invalid_vote
    end,
    countVotes(Rest);
countVotes(_) ->
    stateDB.


