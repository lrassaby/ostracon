{application, ostracon, [
	{description, "A library for distributed realtime vote collection over
    web sockets in Erlang."},
	{vsn, "0.1.0"},
	{id, "b651b0d-dirty"},
	{modules, ['ostracon_collector', 'ostracon_handler', 'callback_module', 'ostracon_app', 'ostracon_sup']},
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
