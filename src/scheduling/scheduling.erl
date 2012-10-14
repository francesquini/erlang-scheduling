-module(scheduling).

%%
%% Exported Functions
%%
-export ([
	% Misc
	scheduler_count/0,
	bind_no_spread/0,
	check_scheduler_bindings/0,
	set_all_strategies_default/0,
		  		  
 	% Conversion 
	cpu_to_scheduler/1, scheduler_to_cpu/1,

	% Process Pinning
	current_scheduler/0, set_scheduler/1, unset_scheduler/0, 
	current_cpu/0, set_cpu/1, unset_cpu/0		 
		 		  
]).


%%
%% API Functions
%%


% Misc

scheduler_count() ->
	erlang:system_info(schedulers).

bind_no_spread() ->
	erlang:system_flag(scheduler_bind_type, no_spread).

check_scheduler_bindings() ->
	Bs = tuple_to_list(erlang:system_info(scheduler_bindings)),
	NotBnd = lists:any(fun (Each) -> Each == unbound end, Bs),
	if
		NotBnd -> 
			io:format("WARNING: At least one scheduler is not bound!~nCheck erlang:system_flag(scheduler_bind_type, How)~n"),
			nok;
		true -> ok
	end.

set_all_strategies_default() ->
	sched_ip_strategies:cancel_scheduled_strategy_change(),
	sched_ip_strategies:set_default(),
	sched_migration_strategies:set_default(),
	sched_ws_strategies:set_default().


% Conversion 

cpu_to_scheduler(CPUID) ->
	cpu_to_scheduler(CPUID, bindings_as_list ()).

scheduler_to_cpu(Scheduler) ->
	Binds = bindings_as_list(),
	scheduler_to_cpu(Scheduler, Binds).


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

