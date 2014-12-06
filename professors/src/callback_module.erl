-module(callback_module).

-export([reset/0, start/0, timeInterval/0, updateState/1]).

makeProfessors() ->
    ets:insert(stateDB, {markX, random:uniform()}),
    ets:insert(stateDB, {markY, random:uniform()}),
    ets:insert(stateDB, {markScore, 0}),
    ets:insert(stateDB, {mingX, random:uniform()}),
    ets:insert(stateDB, {mingY, random:uniform()}),
    ets:insert(stateDB, {mingScore, 0}),
    ets:insert(stateDB, {couchX, random:uniform()}),
    ets:insert(stateDB, {couchY, random:uniform()}),
    ets:insert(stateDB, {couchScore, 0}),
    ets:insert(stateDB, {noahX, random:uniform()}),
    ets:insert(stateDB, {noahY, random:uniform()}),
    ets:insert(stateDB, {noahScore, 0}).

makeMonaco() ->
    ets:insert(stateDB, {monacoX, random:uniform()}),
    ets:insert(stateDB, {monacoY, random:uniform()}),
    ets:insert(stateDB, {monacoVelX, 0.02 * random:uniform()}),
    ets:insert(stateDB, {monacoVelY, 0.02 * random:uniform()}).


reset() ->
    ets:delete_all_objects(stateDB),
    makeProfessors(),
    makeMonaco(),
    stateDB.

% called to start the callback module
start() ->
    reset().

% sets the time for each round
timeInterval() ->
    50.



hardLimit(X, _, Max) when X > Max ->
    Max;
hardLimit(X, Min, _) when X < Min ->
    Min;
hardLimit(X, _, _) -> X.



updatePos(XAtom, YAtom, X, Y) ->
    XLimited = hardLimit(X, 0, 1),
    YLimited = hardLimit(Y, 0, 1),
    ets:insert(stateDB, {XAtom, XLimited}), 
    ets:insert(stateDB, {YAtom, YLimited}),
    {XLimited, YLimited}.
    
updateMonaco() ->
    [{monacoX, OldMonacoX}|_] = ets:lookup(stateDB, monacoX),
    [{monacoVelX, OldMonacoVelX}|_] = ets:lookup(stateDB, monacoVelX),
    [{monacoY, OldMonacoY}|_] = ets:lookup(stateDB, monacoY),
    [{monacoVelY, OldMonacoVelY}|_] = ets:lookup(stateDB, monacoVelY),
    AccelX = 0.03 * (random:uniform() - 0.5),
    AccelY = 0.02 * (random:uniform() - 0.5),

    Damping = 0.9,
    NewMonacoX = OldMonacoVelX + OldMonacoX,
    NewMonacoVelX = hardLimit(Damping * OldMonacoVelX + AccelX, -0.03, 0.03),
    NewMonacoY = OldMonacoVelY + OldMonacoY, 
    NewMonacoVelY = hardLimit(Damping * OldMonacoVelY + AccelY, -0.03, 0.03),

    ets:insert(stateDB, {monacoVelX, NewMonacoVelX}),
    ets:insert(stateDB, {monacoVelY, NewMonacoVelY}),
    {FinalX, FinalY} = updatePos(monacoX, monacoY, NewMonacoX, NewMonacoY),
    if 
        FinalX =:= 0; FinalX =:= 1 ->  % if we hit a wall, reverse direction
            ets:insert(stateDB, {monacoVelX, NewMonacoVelX * -1});
        FinalY =:= 0; FinalY =:= 1 -> 
            ets:insert(stateDB, {monacoVelY, NewMonacoVelY * -1});
        true ->
            ok
    end,
    {FinalX, FinalY}.

getAtoms(Team) ->
    case Team of
        << "noah" >> -> {noahX, noahY, noahScore};
        << "ming" >> -> {mingX, mingY, mingScore};
        << "couch" >> -> {couchX, couchY, couchScore};
        << "mark" >> -> {markX, markY, markScore};
        _ -> io:format("~p~n", Team)
    end.

% called with a histogram of votes at the end of each round
updateState(Votes) ->
    Count = lists:foldr(fun({_, Freq}, Sum) -> (Sum + Freq) end, 0, Votes),
    {NewMonacoX, NewMonacoY} = updateMonaco(),
    MonacoBox = 
        {NewMonacoX, (NewMonacoX + 0.0364), NewMonacoY, (NewMonacoY + 0.085)}, 
    %.0664 = 30/512-60 = PlayerSize / CanvasSize
    movePlayers(Count, Votes, MonacoBox).

collisionCheck ({AX1, AX2, AY1, AY2}, {BX1, BX2, BY1, BY2}) 
    when AX1 < BX2, AX2 > BX1, AY1 < BY2, AY2 > BY1 ->
    true;
collisionCheck (_, _) ->
    false.
    
processCollision(ScoreAtom) ->
    [{ScoreAtom, Score}|_] = ets:lookup(stateDB, ScoreAtom),
    NewScore = Score + 1,
    ets:insert(stateDB, {ScoreAtom, NewScore}),
    makeMonaco().

movePlayers(Count, [{{Vote, Team}, Freq}|Rest], MonacoBox) ->
    % use votehist to update stateDB and return stateDB
    {XAtom, YAtom, ScoreAtom} = getAtoms(Team),
    [{XAtom, X}|_] = ets:lookup(stateDB, XAtom),
    [{YAtom, Y}|_] = ets:lookup(stateDB, YAtom),
    Delta = 0.05 * (Freq/Count),
    case Vote of
        << "up" >> -> 
            updatePos(XAtom, YAtom, X, Y - Delta);
        << "down" >> -> 
            updatePos(XAtom, YAtom, X, Y + Delta);
        << "left" >> -> 
            updatePos(XAtom, YAtom, X - Delta/2, Y); % /2 b/c width = height*2
        << "right" >> -> 
            updatePos(XAtom, YAtom, X + Delta/2, Y);
        _ ->
            invalid_vote
    end,
    [{XAtom, NewX}|_] = ets:lookup(stateDB, XAtom),
    [{YAtom, NewY}|_] = ets:lookup(stateDB, YAtom),
    PlayerBox = {NewX, (NewX + 0.0364), NewY, (NewY + 0.0794)},
    IsCollision = collisionCheck(PlayerBox, MonacoBox),
    case IsCollision of 
        true -> 
            processCollision(ScoreAtom);
        false ->
            ok   
    end,
    movePlayers(Count, Rest, MonacoBox);
movePlayers(_, _, _) ->
    stateDB.


