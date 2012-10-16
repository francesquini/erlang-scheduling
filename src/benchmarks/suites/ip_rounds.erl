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
	run_ip (default, Size, Times),
	run_ip (circular, Size, Times),
	run_ip (random, Size, Times).

generate_trace_script([OutFileName, Size, RoundMax, RoundStep]) ->
	io:format(
	  	"#!/bin/bash\n" ++
		"DIR=$(dirname $0)\n" ++
		"TRACE_DIR=$DIR/../../traces/\n" ++
		"SIZE=~s\n" ++
		"REPTS=1\n" ++
		"if [ \"$(whoami)\" != \"root\" ]; then\n" ++
		"	echo \"Sorry, to probe the Erlang VM you must run this script as root.\"\n" ++
		"	exit 1\n" ++
		"fi\n",
		[Size]),  
	  
	RoundList = lists:seq(0, utils:to_int(RoundMax), utils:to_int(RoundStep)),	
	[io:format("$DIR/../erl_prof  $TRACE_DIR/~s.~p.~p.trace ip_rounds run ~p ~p $SIZE $REPTS >$TRACE_DIR/~s.~p.~p.res\n" ++
			   "$DIR/../prof2paje $TRACE_DIR/~s.~p.~p.trace $TRACE_DIR/~s.~p.~p.paje\n", 
			   [OutFileName, Str, Rnds, Str, Rnds, OutFileName, Str, Rnds,
				OutFileName, Str, Rnds, OutFileName, Str, Rnds]) || 
		Str <- sched_ip_strategies:get_strategies(), Str /= default, 
		Rnds <- RoundList].

%%
%% Local Functions
%%

run_ip (Strategy, Size, Times) ->
	Waits = lists:seq(0, 100, 5),
	[run_in_strategy (Strategy, After, Size, Times) || After <- Waits].


run_in_strategy (Strategy, 0, Size, Times) when is_atom(Strategy) andalso is_atom(Size) andalso is_integer(Times) ->
	InitFun = fun() -> 
				  scheduling:set_all_strategies_default(),
				  sched_ip_strategies:set_strategy(Strategy)
			  end,
	run_in_strategy (Strategy, 0, Size, Times, InitFun);
run_in_strategy (Strategy, After, Size, Times) when is_atom(Strategy) andalso is_integer(After) andalso is_atom(Size) andalso is_integer(Times) ->
	InitFun = fun() ->
				  scheduling:set_all_strategies_default(),
				  sched_ip_strategies:set_strategy(Strategy),
				  sched_ip_strategies:set_strategy_after(default, After)
		  	  end,
	run_in_strategy (Strategy, After, Size, Times, InitFun). 

run_in_strategy (Strategy, After, Size, Times, InitFun) when is_function (InitFun) ->
	Ref = make_ref(),
	Self = self(),
	spawn_link (
	  fun() -> 
		bench:do(
		  utils:to_string(Strategy) ++ "-" ++ utils:to_string(After), 
		  	Size, Times, InitFun),
		Self ! Ref
	  end),
	receive Ref -> ok end.

