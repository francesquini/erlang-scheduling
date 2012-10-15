%% Author: emilio
%% Created: 14 oct. 2012
%% Description: TODO: Add description to suites
-module(ip_rounds).


-export([run/0, run/1, generate_trace_script/1]).

%%
%% API Functions
%%

run ([IPStrategy, Rounds, Size, Times]) ->
	scheduling:bind_no_spread(),
	RoundsN = utils:to_int(Rounds),
	TimesN = utils:to_int(Times),
	run_in_strategy(IPStrategy, RoundsN, Size, TimesN). 

run() ->
	Times = 20,
	Size = small,
	scheduling:bind_no_spread(),
	run_ip_default(Size, Times),
	run_ip_circular(Size, Times),
	run_ip_random(Size, Times).

generate_trace_script([OutFileName]) ->
	io:format("#!/bin/bash\n"),
	io:format("DIR=$(dirname $0)\n"),
	
	[io:format("$DIR/../erl_prof  $DIR/~s.~p.~p.trace ip_rounds run ~p ~p small 1 >$DIR/~s.~p.~p.res\n" ++
			   "$DIR/../prof2paje $DIR/~s.~p.~p.trace $DIR/~s.~p.~p.paje\n", 
			   [OutFileName, Str, Rounds, Str, Rounds, OutFileName, Str, Rounds,
				OutFileName, Str, Rounds, OutFileName, Str, Rounds]) || 
		Str <- sched_ip_strategies:get_strategies(), Str /= default, 
		Rounds <- lists:seq(50, 1000, 50)].

%%
%% Local Functions
%%


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
	Ref = make_ref(),
	Self = self(),
	spawn_link (
	  fun() -> 
		bench:do(
		  utils:to_string(Strategy) ++ "-" ++ utils:to_string(After), Size, Times, 
		  fun() -> 
				  scheduling:set_all_strategies_default(),
				  sched_ip_strategies:set_strategy(Strategy),
				  sched_ip_strategies:set_strategy_after(default, After)
		  end),
		Self ! Ref	
	  end),
	receive Ref -> ok end. 
