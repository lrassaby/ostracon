-module(ostracon_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

% Starts everything!
start(_Type, _Args) ->
    ets:new(voteDB, [set, public, named_table]),
    ets:new(stateDB, [public, named_table, set]),
    ostracon_collector:start(), 

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ostracon_handler, []}, 
            {"/", cowboy_static, {priv_file, ostracon, "index.html"}},
            {"/[...]", cowboy_static, {priv_dir, ostracon, ""}}
        ]}
    ]),
    cowboy:start_http(http, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),

    ostracon_sup:start_link().
    
stop(_State) ->
	ok.
