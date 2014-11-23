-module(callback_module).

-export([reset/0, start/0, timeInterval/0, updateState/1]).

makeProfessors() ->
    ets:insert(stateDB, {markX, random:uniform()}),
    ets:insert(stateDB, {markY, random:uniform()}),
    ets:insert(stateDB, {benX, random:uniform()}),
    ets:insert(stateDB, {benY, random:uniform()}),
    ets:insert(stateDB, {mingX, random:uniform()}),
    ets:insert(stateDB, {mingY, random:uniform()}),
    ets:insert(stateDB, {couchX, random:uniform()}),
    ets:insert(stateDB, {couchY, random:uniform()}),
    ets:insert(stateDB, {noahX, random:uniform()}),
    ets:insert(stateDB, {noahY, random:uniform()}).

makeMonaco() ->
    ets:insert(stateDB, {monacoX, random:uniform()}),
    ets:insert(stateDB, {monacoY, random:uniform()}).


reset() ->
    ets:delete_all_objects(stateDB),
    makeProfessors(),
    makeMonaco(),
    stateDB.


start() ->
    reset().

timeInterval() ->
    50.

inBounds(X, Y) ->
    (X =< 1) and (Y =< 1) and (X >= 0) and (Y >= 0).

tryUpdate(XAtom, YAtom, X, Y) ->
    InBounds = inBounds(X, Y),
    if 
        InBounds ->
            ets:insert(stateDB, {XAtom, X}), 
            ets:insert(stateDB, {YAtom, Y});
        true ->
            out_of_bounds
    end.

getAtoms(Team) ->
    case Team of
        "noah" -> {noahX, noahY};
        "ben" -> {benX, benY};
        "ming" -> {mingX, mingY};
        "couch" -> {couchX, couchY};
        "mark" -> {markX, markY}
    end.

updateState(Votes) ->
    Count = lists:foldr(fun({_, Freq}, Sum) -> (Sum + Freq) end, 0, Votes),
    [{monacoX, MonacoX}|_] = ets:lookup(stateDB, monacoX),
    [{monacoY, MonacoY}|_] = ets:lookup(stateDB, monacoY),
    DeltaX = 0.02 * (random:uniform() - 0.5),
    DeltaY = 0.02 * (random:uniform() - 0.5),
    tryUpdate(monacoX, monacoY, MonacoX + DeltaX, MonacoY + DeltaY),
    movePlayers(Count, Votes).


movePlayers(Count, [{{Vote, Team}, Freq}|Rest]) ->
    % use votehist to update stateDB and return stateDB
    {XAtom, YAtom} = getAtoms(Team),
    [{XAtom, X}|_] = ets:lookup(stateDB, XAtom),
    [{YAtom, Y}|_] = ets:lookup(stateDB, YAtom),
    Delta = 0.05 * (Freq/Count),
    case Vote of
        <<"up">> -> 
            tryUpdate(XAtom, YAtom, X, Y - Delta);
        <<"down">> -> 
            tryUpdate(XAtom, YAtom, X, Y + Delta);
        <<"left">> -> 
            tryUpdate(XAtom, YAtom, X - Delta, Y);
        <<"right">> -> 
            tryUpdate(XAtom, YAtom, X + Delta, Y);
        _ ->
            invalid_vote
    end,
    movePlayers(Count, Rest);
movePlayers(_, _) ->
    stateDB.


