-module(simple_spawner).

-export([start/0, start/1, start_no_wait/1, do_nothing/1]).

start() ->
	start(100).

start (0) ->
	ok;
start (Qty) ->
	spawn(fun () -> wait_and_die(Qty * 100) end),
	start (Qty - 1).
	
wait_and_die(Time) ->
	receive
	after
		Time ->	ok
	end.

start_no_wait(0) ->
	ok;
start_no_wait(Qty) ->
	spawn(simple_spawner, do_nothing, [Qty]),
	start_no_wait(Qty - 1).
	
do_nothing (Index) ->
	case Index rem 2 of
		3 -> nossa;
		_ -> ok
	end.