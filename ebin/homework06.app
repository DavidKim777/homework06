{application, 'homework06', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['homework06','homework06_app','homework06_sup']},
	{registered, [homework06_sup]},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{mod, {homework06_app, []}},
	{env, []}
]}.