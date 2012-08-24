-module(pg).

-export([do/1]).

do(Times) ->
	B1 = os:timestamp(),
	utils:times_repeat(Times, fun () -> os:timestamp() end),
	TS1 = timer:now_diff(os:timestamp(), B1) / Times,
	B2 = os:timestamp(),
	utils:times_repeat(Times, fun () -> now() end),
	TS2 = timer:now_diff(os:timestamp(), B2)  / Times,
	io:format("TS ~p Now ~p ~n", [TS1, TS2]).


