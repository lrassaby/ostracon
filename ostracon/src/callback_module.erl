-module(callback_module).

-export([reset/0, start/0, timeInterval/0, updateState/1]).

reset() ->
    ets:delete_all_objects(stateDB),
    ets:insert(stateDB, {x, 0.5}),
    ets:insert(stateDB, {y, 0.5}),
    stateDB.

start() ->
    ok.

timeInterval() ->
    30.

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

    

% ENSURE NO EXPLICIT CONCURRENCY IN THIS MODULE -- ABSTRACT THE LOGIC TO THE 
% OSTRACON_APP.ERL

% Contract: There exists a registered state_ets that this module has access to

% TODO: export:
% updateState/1, takes in vote histogram and attempts to update state ETS, failing 
% if out of bounds
% reset/0, which sets the state ETS to its initial form

