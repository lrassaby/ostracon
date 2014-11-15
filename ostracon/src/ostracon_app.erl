-module(ostracon_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ostracon_handler, []}, 
            {"/", cowboy_static, {priv_file, ostracon, "index.html"}},
            {"/maze", cowboy_static, {priv_file, ostracon, "maze.html"}},
            {"/[...]", cowboy_static, {priv_dir, ostracon, "assets"}}
        ]}
    ]),
    cowboy:start_http(http, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    ostracon_sup:start_link().

stop(_State) ->
	ok.
