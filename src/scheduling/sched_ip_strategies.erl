-module(sched_ip_strategies).

%%
%% Exported Functions
%%
-export([
		 get_current_strategy/0,
		 set_strategy/1,
		 set_strategy_after/2,
		 cancel_scheduled_strategy_change/0,
		 set_default/0, 
		 set_random/0,
		 set_circular/0,
		 get_strategies/0
		]).

%%
%% API Functions
%%

get_current_strategy() ->
	StId = erlang:system_info(scheduler_ip_strategy),
	strategy_name(StId).

set_default() ->
	set_strategy(default).
set_random() ->
    set_strategy(random).
set_circular() ->
	set_strategy(circular).

set_strategy (Strategy) ->
	scheduling:check_scheduler_bindings(),
	StId = strategy_id(Strategy),
	OldValue = erlang:system_flag(scheduler_ip_strategy, StId),
	strategy_name(OldValue).

set_strategy_after (Strategy, After_CBS) when is_integer(After_CBS) andalso After_CBS > 0 ->
	scheduling:check_scheduler_bindings(),
	StId = strategy_id(Strategy),
	OldValue = erlang:system_flag(scheduler_ip_strategy, {StId, 'after', After_CBS}),
	strategy_name(OldValue).
	
cancel_scheduled_strategy_change () ->
	strategy_name(erlang:system_flag(scheduler_ip_strategy, {0, 'after', 0})).

get_strategies() ->
	[Str || {Str, _} <- strategies()].

%%
%% Local Functions
%%

strategies() ->
	[{default, 0}, {random, 1}, {circular, 2}].

strategy_name(StrategyId) ->
	{Name, StrategyId} = lists:keyfind(StrategyId, 2, strategies()),
	Name.

strategy_id (StrategyName) ->
	{StrategyName, StId} = lists:keyfind(StrategyName, 1, strategies()),
	StId.