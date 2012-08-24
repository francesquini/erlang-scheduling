-module(placement_strategies).

%%
%% Include files
%%

-include("strategy.hrl").

%%
%% Exported Functions
%%
-export([
		 %Strategy constructors
		 default/0, 
		 random/0, random/1,
		 circular/0,
		 fixed_set/1,
		 %Strategy usage
		 next/1
		]).

% Local Exports
-export([default_strategy_impl/1, random_strategy_impl/1, circular_strategy_impl/1,fixed_set_strategy_impl/1]).

%%
%% API Functions
%%

default() ->
	#plc_str{name=default, module=?MODULE, function=default_strategy_impl}.

random() ->
    random(now()).
	
random(Seed) -> 
	Max = scheduling:scheduler_count(),
	#plc_str{
		name=random, 
		module=?MODULE,
		function=random_strategy_impl,
		state={Max, Seed}
	}.

circular() ->
	Max = scheduling:scheduler_count(),
	#plc_str{
		name=circular, 
		module=?MODULE,
		function=circular_strategy_impl,
		state={Max, Max - 1}
	}.

fixed_set(SchedList) when is_list(SchedList) ->
	#plc_str{
		name=fixed_set, 
		module=?MODULE,
		function=fixed_set_strategy_impl,
		state={SchedList, SchedList}
	}.

next(Strategy = #plc_str{module=M, function=F, state=S}) ->
	{Next, NState} = apply(M, F, [S]),
	{Next, Strategy#plc_str{state=NState}}.

%%
%% Local Functions
%%

default_strategy_impl (State) ->
	{0, State}.

random_strategy_impl (_State = {Max, Seed}) ->
	{Next, NewSeed} = random:uniform_s(Max, Seed),
	{Next, {Max, NewSeed}}.

circular_strategy_impl (_State = {Max, Last}) ->
	Next = ((Last + 1) rem Max),
	{Next + 1, {Max, Next}}.

fixed_set_strategy_impl (_State = {[], SchedListOrig}) ->
	fixed_set_strategy_impl({SchedListOrig, SchedListOrig});
fixed_set_strategy_impl (_State = {[Sched|Rest], SchedListOrig}) ->
	{Sched, {Rest, SchedListOrig}}.
