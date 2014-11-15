-module(ostracon).
-export([start/0, stop/0]).

start() ->
  Dispatch = cowboy_router:compile([
      {'_', [
        {"/", cowboy_static, {priv_file, ostracon, "index.html"}},
        {"/websocket", ostracon_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 10100}],
        [{env, [{dispatch, Dispatch}]}]),
      ostracon_supervisor:start_link().

stop() ->
    ok.
