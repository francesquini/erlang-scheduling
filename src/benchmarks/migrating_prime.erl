%% Author: emilio
%% Created: 17/05/2012
%% Description: TODO: Add description to migrating_prime
-module(migrating_prime).

-export([start/0, stop/1]).


start() ->
	erlang:system_flag(scheduler_bind_type, no_spread),	
	spawn_link(fun loop/0).

stop(Pid) ->
	Pid ! stop.

loop() ->
	loop (2, 3).
loop (Max, NextToTest) ->
	receive
		{max, From} ->
			From ! {max, Max},
			loop (Max, NextToTest);
		{jump, From, NewCPU} ->
			From ! utils:set_cpu(NewCPU),
			loop (Max, NextToTest);
		stop ->
			io:format("Stopping. Max prime: ~p", [Max])
	after 0 ->
			case utils:is_prime(NextToTest) of
				0 -> loop (NextToTest, NextToTest + 2);
				_ -> loop (Max, NextToTest + 2)
			end
	end.
