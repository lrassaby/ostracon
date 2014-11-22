{application, ostracon, [
	{description, "A library for distributed realtime vote collection over
    web sockets in Erlang."},
	{vsn, "0.1.0"},
	{id, "7c54e7e-dirty"},
	{modules, ['callback_module', 'ostracon_app', 'ostracon_collector', 'ostracon_handler', 'ostracon_sup']},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		cowboy
	]},
	{mod, {ostracon_app, []}},
	{env, []}
]}.
