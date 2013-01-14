-module(trace_load).

-export ([load_file/1]).

load_file(Filename) ->
	Proc = spawn_link (fun() -> {trace_ubi, self()}:load_file (Filename) end),
	{trace_ubi, Proc}.