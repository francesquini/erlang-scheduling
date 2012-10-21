%% Author: emilio
%% Created: 14 oct. 2012
%% Description: TODO: Add description to suites
-module(ip_rounds).

-export([run/1, generate_trace_script/1]).

%%
%% API Functions
%%

run ([Bench, IPStrategy, Rounds, Size, Times]) ->
	scheduling:bind_no_spread(),
	RoundsN = utils:to_int(Rounds),
	TimesN = utils:to_int(Times),
	run_in_strategy(Bench, IPStrategy, RoundsN, Size, TimesN). 


generate_trace_script([OutFileName, Bench, Size, Repetitions, RoundMax, RoundStep]) ->
	io:format(
	  	"#!/bin/bash\n" ++
		"DIR=$(dirname $0)\n" ++
		"DIR=$(readlink -f \"$DIR\")\n"++
		"TRACE_DIR=$DIR/../../traces/\n" ++
		"TRACE_DIR=$(readlink -f \"$TRACE_DIR\")\n" ++	
		"SIZE=~s\n" ++
		"REPTS=~s\n" ++
		"if [ \"$(whoami)\" != \"root\" ]; then\n" ++
		"	echo \"Sorry, to probe the Erlang VM you must run this script as root.\"\n" ++
		"	exit 1\n" ++
		"fi\n",
		[Size, Repetitions]),  
	  
	RoundList = lists:seq(0, utils:to_int(RoundMax), utils:to_int(RoundStep)),
	
	[generate_script_step(OutFileName, Bench, Str, Rnds) ||
		Str <- sched_ip_strategies:get_strategies(), 
		Rnds <- RoundList],

	io:format("$DIR/../res2plot ~s.~p ~p $TRACE_DIR > $TRACE_DIR/~s.~p.rounds.gp\n" ++
			  "gnuplot $TRACE_DIR/~s.~p.rounds.gp\n",
			  [OutFileName, Bench, 	Bench, 	OutFileName, Bench, 
			   OutFileName, Bench]).

%%
%% Local Functions
%%


generate_script_step (OutFileName, Bench, Strategy, Rounds) ->
	FilePrefix = string:join([utils:to_string(S) || S <- [OutFileName, Bench, Strategy, Rounds]], "."),
	io:format("$DIR/../erl_prof   $TRACE_DIR/~s.trace ip_rounds run ~p ~p ~p $SIZE 1 >$TRACE_DIR/~s.one\n" ++ %1
			   "$DIR/../erl_run   ip_rounds run ~p ~p ~p $SIZE $REPTS >$TRACE_DIR/~s.res\n" ++					 %2
			   "$DIR/../prof2paje $TRACE_DIR/~s.trace $TRACE_DIR/~s.paje\n" ++								 %3
			   "$DIR/../prof2plot ~s.trace $TRACE_DIR >$TRACE_DIR/~s.gp\n" ++								 %4
			   "gnuplot $TRACE_DIR/~s.gp\n",  																 %5
			   [FilePrefix, 	Bench, Strategy, Rounds, 	FilePrefix, %1
				Bench, Strategy, Rounds,	FilePrefix,							%2
				FilePrefix, 	FilePrefix,								%3
				FilePrefix, 	FilePrefix,								%4
				FilePrefix]).											%5


run_in_strategy (Bench, Strategy, RoundsBeforeChange, Size, Times) when 
  is_atom(Bench) andalso is_atom(Strategy) andalso is_atom(Size) andalso is_integer(Times) ->
	run_in_strategy2 (Bench, Strategy, RoundsBeforeChange, Size, Times).

run_in_strategy2 (Bench, Strategy, 0, Size, Times) ->
	InitFun = fun() -> 
				  scheduling:set_all_strategies_default(),
				  sched_ip_strategies:set_strategy(Strategy)
			  end,
	run_in_strategy3 (Bench, Strategy, 0, Size, Times, InitFun);
run_in_strategy2 (Bench, Strategy, After, Size, Times) ->
	InitFun = fun() ->
				  scheduling:set_all_strategies_default(),
				  sched_ip_strategies:set_strategy(Strategy),
				  sched_ip_strategies:set_strategy_after(default, After)
		  	  end,
	run_in_strategy3 (Bench, Strategy, After, Size, Times, InitFun). 

run_in_strategy3 (Bench, Strategy, After, Size, Times, InitFun) when is_function (InitFun) ->
	Ref = make_ref(),
	Self = self(),
	spawn_link (
	  fun() -> 
		bench:do(Bench, utils:to_string(Strategy) ++ "\t" ++ utils:to_string(After), 
		  	Size, Times, InitFun),
		Self ! Ref
	  end),
	receive Ref -> ok end.

