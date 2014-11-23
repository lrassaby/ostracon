{application, ostracon, [
	{description, "A library for distributed realtime vote collection over
    web sockets in Erlang."},
	{vsn, "0.1.0"},
	{id, "a77a9e5-dirty"},
	{modules, ['callback_module', 'ostracon_app', 'ostracon_collector', 'ostracon_handler', 'ostracon_sup']},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		cowboy,
        jiffy
	]},
	{mod, {ostracon_app, []}},
	{env, []}
]}.
