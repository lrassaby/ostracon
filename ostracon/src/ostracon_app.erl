-module(ostracon_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    % TODO: initialize callback module, collector
    % TODO: create state and vote ETS's
    % TODO: take in the name of the callback module's erl file, compile and initialize
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ostracon_handler, []}, % TODO: pass as arg callback module
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
