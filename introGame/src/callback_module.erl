-module(callback_module).

-export([reset/0, start/0, timeInterval/0, updateState/1]).

makeInitialStores() ->
    ets:insert(stateDB, {left, 0}),
    ets:insert(stateDB, {right, 0}),
    ets:insert(stateDB, {up, 0}),
    ets:insert(stateDB, {down, 0}),


reset() ->
    ets:delete_all_objects(stateDB),
    makeInitialStores(),
    stateDB.


start() ->
    reset().

timeInterval() ->
    4000.

% updateState(Count, [{{Vote, Keystroke}, Freq}|Rest]) ->
%     % use votehist to update stateDB and return stateDB
%     [{Keystroke, Total}|_] = ets:lookup(stateDB, Keystroke),
%     ets:insert(stateDB, {Keystroke, Total+Freq});
%     updateState(Count, Rest);
updateState([{{Vote, _Keystroke}, Freq}|Rest]) ->
    case Vote of
        << "up" >> -> 
            ets:insert(stateDB, {up, Freq});
        << "down" >> -> 
            ets:insert(stateDB, {down, Freq});
        << "left" >> -> 
            ets:insert(stateDB, {left, Freq});
        << "right" >> -> 
            ets:insert(stateDB, {right, Freq});
        _ ->
            invalid_vote
    end,
    updateState(Rest).
updateState(_, _) ->
    stateDB.


