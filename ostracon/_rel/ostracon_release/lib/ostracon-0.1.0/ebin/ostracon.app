{application, ostracon, [
	{description, "A library for distributed realtime vote collection in Erlang."},
	{vsn, "0.1.0"},
	{id, "b8512c7-dirty"},
	{modules, ['ostracon_app', 'ostracon_handler', 'ostracon_sup']},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		cowboy
	]},
	{mod, {ostracon_app, []}},
	{env, []}
]}.
