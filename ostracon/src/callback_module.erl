-module(callback_module).

-export([reset/0, start/0, timeInterval/0, updateState/1]).

reset() ->
    ets:delete_all_objects(stateDB),
    ets:insert(stateDB, {x, 0.5}),
    ets:insert(stateDB, {y, 0.5}),
    stateDB.

start() ->
    reset().

timeInterval() ->
    1000.

inBounds(X, Y) ->
    (X =< 1) and (Y =< 1) and (X >= 0) and (Y >= 0).

tryUpdate(X, Y) ->
    InBounds = inBounds(X, Y),
    if 
        InBounds ->
            ets:insert(stateDB, {x, X}), 
            ets:insert(stateDB, {y, Y});
        true ->
            out_of_bounds
    end.

updateState([{Vote, _Freq}|_]) ->
    % use votehist to update stateDB and return stateDB
    X = ets:lookup(stateDB, x),
    Y = ets:lookup(stateDB, y),
    case Vote of
        up    -> 
            tryUpdate(X, Y + 0.05);
        down  -> 
            tryUpdate(X, Y - 0.05);
        left  ->
            tryUpdate(X - 0.05, Y);
        right ->
            tryUpdate(X + 0.05, Y);
        _ ->
            invalid_vote
    end,
    stateDB;
updateState(_) ->
    stateDB.

