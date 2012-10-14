%% Author: emilio
%% Created: 14 oct. 2012
%% Description: TODO: Add description to suites
-module(ip_rounds).


-export([run/0]).

%%
%% API Functions
%%

run() ->
	Times = 20,
	Size = small,
	scheduling:bind_no_spread(),
	run_ip_default(Size, Times),
	run_ip_circular(Size, Times),
	run_ip_random(Size, Times).


run_ip_default(Size, Times) ->	
	scheduling:set_all_strategies_default(),
	bench:do ("Default", Size, Times).

run_ip_circular(Size, Times) ->	
	scheduling:set_all_strategies_default(),
	sched_ip_strategies:set_circular(),
	bench:do ("Circular", Size, Times),
	Waits = lists:seq(50, 1000, 50),
	[run_in_strategy (circular, After, Size, Times) || After <- Waits].

run_ip_random(Size, Times) ->	
	scheduling:set_all_strategies_default(),
	sched_ip_strategies:set_random(),
	bench:do ("Random", Size, Times),
	Waits = lists:seq(50, 1000, 50),
	[run_in_strategy (random, After, Size, Times) || After <- Waits].


run_in_strategy (Strategy, After, Size, Times) when is_atom(Strategy) andalso is_integer(After) andalso is_atom(Size) andalso is_integer(Times) ->
	bench:do(
	  utils:to_string(Strategy) ++ "-" ++ utils:to_string(After), Size, Times, 
	  fun() -> 
			  scheduling:set_all_strategies_default(),
			  sched_ip_strategies:set_strategy(Strategy),
			  sched_ip_strategies:set_strategy_after(default, After)
	  end). 
