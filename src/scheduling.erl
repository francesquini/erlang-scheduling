-module(scheduling).

-define(SCHEDULER_PROCESS_NAME, scheduler).

%%
%% Include files
%%

-include("strategy.hrl").

%%
%% Exported Functions
%%
-export ([
		  % Spawning
		  spawn/1, spawn_link/1,
		  spawn_pin/1, spawn_pin_link/1,
		  % Initial Placement Strategy
		  placement_strategy/1,
		  placement_strategy/0,
		  set_placement_random/0,
		  set_placement_circular/0,
		  set_placement_fixed_set/1,
		  set_placement_default/0,
		  % Misc
		  scheduler_count/0,
		  bind_no_spread/0,
		  % Process Pinning
		  current_scheduler/0, set_scheduler/1, unset_scheduler/0, 
		  current_cpu/0, set_cpu/1, unset_cpu/0,
		  % Conversion 
		  cpu_to_scheduler/1, scheduler_to_cpu/1]).

% Local exports
-export ([
			scheduler_loop/1
		]).

%%
%% API Functions
%%

% Spawning
spawn(Fun) ->
	spawn_nopin(Fun, []).

spawn_link(Fun) ->
	spawn_nopin(Fun, [link]).

spawn_pin(Fun) ->
	spawn_pinned (Fun, []).

spawn_pin_link(Fun) ->
	spawn_pinned (Fun, [link]).

% Initial Placement Strategy
placement_strategy (random)    -> set_placement_random ();
placement_strategy (circular)  -> set_placement_circular (); 
placement_strategy (default)   -> set_placement_default ().

set_placement_random() ->
	set_placement_strategy(placement_strategies:random()).

set_placement_default() ->
	set_placement_strategy(placement_strategies:default()).

set_placement_circular() ->
	set_placement_strategy(placement_strategies:circular()).

set_placement_fixed_set (SchedList) ->
  	set_placement_strategy(placement_strategies:fixed_set(SchedList)).

placement_strategy() ->
	Strategy = get_placement_strategy(),
	Strategy#plc_str.name.

% Misc
scheduler_count() ->
	erlang:system_info(schedulers).

bind_no_spread() ->
	erlang:system_flag(scheduler_bind_type, no_spread).

% Actor Pinning
current_scheduler() ->
	erlang:system_info(scheduler_id).

set_scheduler (NewSched) ->
	process_flag(scheduler, NewSched).

unset_scheduler () ->
	set_scheduler (0).

current_cpu() ->
	scheduler_to_cpu(current_scheduler()).

set_cpu (CPUId) ->
	scheduler_to_cpu(set_scheduler(cpu_to_scheduler(CPUId))).

unset_cpu() ->
	unset_scheduler().

% Conversion 

cpu_to_scheduler(CPUID) ->
	cpu_to_scheduler(CPUID, bindings_as_list ()).

scheduler_to_cpu(Scheduler) ->
	Binds = bindings_as_list(),
	scheduler_to_cpu(Scheduler, Binds).

%%
%% Local Functions
%%

bindings_as_list () ->
	tuple_to_list(erlang:system_info(scheduler_bindings)).

% Process pinning and conversion

cpu_to_scheduler(CPUID, Bindings) ->
	cpu_to_scheduler(CPUID, Bindings, 0).

cpu_to_scheduler (_, [], _) ->
	undefined;
cpu_to_scheduler(CPUID, [CPUID | _], Last) ->
	Last + 1;
cpu_to_scheduler(CPUID, [_ | BindTail], Last) ->
	cpu_to_scheduler(CPUID, BindTail, Last + 1).

scheduler_to_cpu(Scheduler, Bindings) when Scheduler =< 0 orelse Scheduler > length(Bindings) ->
	undefined;
scheduler_to_cpu(Scheduler, Bindings) ->
	lists:nth(Scheduler, Bindings).

% Process Spawning

get_next_scheduler() ->
	Server  = get_scheduler_server(),
	Ref = make_ref(),
	Server ! {self(), Ref, next},
	receive
		{Ref, Next} ->
			Next
	end.

get_placement_strategy () ->
	Ref = make_ref(),
	get_scheduler_server() ! {self(), Ref, strategy_get},
	receive
		{Ref, Strategy} ->
			Strategy
	end.

set_placement_strategy (Strategy) ->
	Ref = make_ref(),
	get_scheduler_server() ! {self(), Ref, set_strategy, Strategy},
	receive 
		{Ref, strategy_set, OldStrategy} -> 
			OldStrategy
	end.

get_scheduler_server() ->
	case whereis (?SCHEDULER_PROCESS_NAME) of
		undefined ->
			check_scheduler_bindings(),
			Pid = spawn(?MODULE, scheduler_loop, [placement_strategies:default()]),
			register(?SCHEDULER_PROCESS_NAME, Pid),
			Pid;
		P -> P
	end.

check_scheduler_bindings() ->
	Bs = tuple_to_list(erlang:system_info(scheduler_bindings)),
	NotBnd = lists:any(fun (Each) -> Each == unbound end, Bs),
	if
		NotBnd -> io:format("WARNING: At least one scheduler is not bound!~nCheck erlang:system_flag(scheduler_bind_type, How)~n");
		true -> ok
	end.
		

scheduler_loop (Strategy) ->
	receive
		{Pid, Ref, next} ->
			{Next, NewStrategy} = placement_strategies:next(Strategy),
			Pid ! {Ref, Next},
			scheduler_loop (NewStrategy);
	 	{Pid, Ref, set_strategy, NewStrategy} ->
			Pid ! {Ref, strategy_set, Strategy},
			scheduler_loop(NewStrategy);
		{Pid, Ref, strategy_get} ->
			Pid ! {Ref, Strategy},
			scheduler_loop(Strategy);
		stop ->
			ok;
		Msg ->
			io:format("ERROR: Received unknown message: ~p", [Msg]),
			scheduler_loop (Strategy)
	end.

spawn_nopin (Fun, Opts) ->
  Sched = get_next_scheduler(),
  spawn_opt(Fun, [{scheduler, Sched} | Opts]).

spawn_pinned (Fun, Opts) ->
	Sched = get_next_scheduler(),
	spawn_opt(
	  fun () ->  %For the time being, to avoid migrations
	  	set_scheduler (Sched),
		apply(Fun, [])
	  end,
	  [{scheduler, Sched} | Opts]).
