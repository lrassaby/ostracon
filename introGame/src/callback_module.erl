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
    50.

% inBounds(X, Y) ->
%     (X =< 1) and (Y =< 1) and (X >= 0) and (Y >= 0).

% tryUpdate(XAtom, YAtom, X, Y) ->
%     InBounds = inBounds(X, Y),
%     if 
%         InBounds ->
%             ets:insert(stateDB, {XAtom, X}), 
%             ets:insert(stateDB, {YAtom, Y});
%         true ->
%             out_of_bounds
%     end.

updateState(Count, [{{Vote, Keystroke}, Freq}|Rest]) ->
    % use votehist to update stateDB and return stateDB
    [{Keystroke, Total}|_] = ets:lookup(stateDB, Keystroke),
    ets:insert(stateDB, {Keystroke, Total+Freq});
    updateState(Count, Rest);
updateState(_, _) ->
    stateDB.


