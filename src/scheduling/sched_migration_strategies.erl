-module(sched_migration_strategies).

%%
%% Exported Functions
%%
-export([
		 get_current_strategy/0,
		 set_strategy/1,
		 set_default/0, 
		 set_disabled/0
		]).

%%
%% API Functions
%%

get_current_strategy() ->
	StId = erlang:system_info(scheduler_migration_strategy),
	strategy_name(StId).

set_default() ->
	set_strategy(default).
set_disabled() ->
    set_strategy(disabled).

set_strategy (Strategy) ->
	scheduling:check_scheduler_bindings(),
	StId = strategy_id(Strategy),
	OldValue = erlang:system_flag(scheduler_migration_strategy, StId),
	strategy_name(OldValue).

%%
%% Local Functions
%%

strategies() ->
	[{default, 0}, {disabled, 1}, {numa, 2}].

strategy_name(StrategyId) ->
	{Name, StrategyId} = lists:keyfind(StrategyId, 2, strategies()),
	Name.

strategy_id (StrategyName) ->
	{StrategyName, StId} = lists:keyfind(StrategyName, 1, strategies()),
	StId.