-module(callback_module).

-export([reset/0, start/0, timeInterval/0, updateState/1]).

makeInitialStores() ->
    ets:insert(stateDB, {left, 0}),
    ets:insert(stateDB, {right, 0}),
    ets:insert(stateDB, {up, 0}),
    ets:insert(stateDB, {down, 0}).

% Resets the state
reset() ->
    ets:delete_all_objects(stateDB),
    makeInitialStores(),
    stateDB.

% Called at the beginning
start() ->
    reset().

% Time interval in milliseconds for each voting round
timeInterval() ->
    16.

% Called with a histogram of votes and the end of every voting round
updateState(Votes) ->
    countVotes(Votes).

% Counts the votes in each direction
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


