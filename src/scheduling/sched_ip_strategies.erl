-module(sched_ip_strategies).

%%
%% Exported Functions
%%
-export([
		 get_current_regular_strategy/0,
		 get_current_hub_strategy/0,

		 set_regular_strategy/1,
		 set_hub_strategy/1,

		 set_strategy_after/3,
		 cancel_scheduled_strategy_change/0,
		 set_default/0, 
		 set_random/0,
		 set_circular/0,
		 set_compact/0,
		 set_scatter/0,
		 get_strategies/0
		]).

regular_key() ->
	scheduler_ip_strategy_regular.
hub_key() ->
	scheduler_ip_strategy_hub.

get_current_strategy(Key) ->
	StId = erlang:system_info(Key),
	strategy_name(StId).

%% API
get_current_regular_strategy() ->
	get_current_strategy(regular_key()).

%% API
get_current_hub_strategy() ->
	get_current_strategy(hub_key()).

%% API
set_regular_strategy(Strategy) ->
	set_strategy (Strategy, regular_key()).

%% API	
set_hub_strategy(Strategy) ->
	set_strategy (Strategy, hub_key()).

set_default() ->
	set_regular_strategy(default),
	set_hub_strategy(default).
set_random() ->
    set_regular_strategy(random),
    set_hub_strategy(random).
set_circular() ->
	set_regular_strategy(circular),
	set_hub_strategy(random).
set_compact() ->
	set_regular_strategy(compact),
	set_hub_strategy(compact).
set_scatter() ->
	set_regular_strategy(scatter),
	set_hub_strategy(scatter).

set_strategy (Strategy, Key) ->
	scheduling:check_scheduler_bindings(),
	StId = strategy_id(Strategy),
	OldValue = erlang:system_flag(Key, StId),
	strategy_name(OldValue).

set_strategy_after (Strategy, After_CBS, hub) ->
	set_strategy_after_for (Strategy, After_CBS, hub_key());
set_strategy_after (Strategy, After_CBS, regular) ->
	set_strategy_after_for (Strategy, After_CBS, regular_key()).

set_strategy_after_for (Strategy, After_CBS, Key) when is_integer(After_CBS) andalso After_CBS > 0 ->
	scheduling:check_scheduler_bindings(),
	StId = strategy_id(Strategy),
	OldValue = erlang:system_flag(Key, {StId, 'after', After_CBS}),
	strategy_name(OldValue).
	
cancel_scheduled_strategy_change () ->
	strategy_name(erlang:system_flag(regular_key(), {0, 'after', 0})). %Any key will do

get_strategies() ->
	[Str || {Str, _} <- strategies()].

%%
%% Local Functions
%%

strategies() ->
	[ {default,        0}, 
	  {random,         1}, 
	  {circular,       2}, 
	  {simple_random,  3}, 
	  {local_circular, 4},
	  {scatter,        5},
      {compact,        6}
	 ].

strategy_name(StrategyId) ->
	{Name, StrategyId} = lists:keyfind(StrategyId, 2, strategies()),
	Name.

strategy_id (StrategyName) ->
	{StrategyName, StId} = lists:keyfind(StrategyName, 1, strategies()),
	StId.