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
    50.

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
    [{x, X}|_] = ets:lookup(stateDB, x),
    [{y, Y}|_] = ets:lookup(stateDB, y),
    Delta = 0.02,
    case Vote of
        <<"up">> -> 
            tryUpdate(X, Y - Delta);
        <<"down">> -> 
            tryUpdate(X, Y + Delta);
        <<"left">> -> 
            tryUpdate(X - Delta, Y);
        <<"right">> -> 
            tryUpdate(X + Delta, Y);
        _ ->
            invalid_vote
    end,
    stateDB;
updateState(_) ->
    stateDB.

