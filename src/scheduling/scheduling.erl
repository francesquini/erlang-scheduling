-module(scheduling).

%%
%% Exported Functions
%%
-export ([
	% Misc
	scheduler_count/0,
	check_scheduler_bindings/0, check_scheduler_bindings/1,
	set_all_strategies_default/0,
	
	hubs_only/0, set_hubs_only/1,	
	hub_process/0, set_hub_process/1,
	hub_processes/0, hub_processes_count/0,
	hub_process_spawn_flag/0,
		  		  
 	% Conversion 
	cpu_to_scheduler/1, scheduler_to_cpu/1,

	% Process Pinning
	current_scheduler/0, set_scheduler/1, unset_scheduler/0, 
	current_cpu/0, set_cpu/1, unset_cpu/0,
		 
	% Memory
	memory_allocation_policy/0, deferred_memory_allocation/0	 
		 		  
]).


%%
%% API Functions
%%


% Misc

scheduler_count() ->
	erlang:system_info(schedulers).

check_scheduler_bindings(Bool) ->
	case check_scheduler_bindings() of
		nok when Bool -> erlang:halt(13);
		X -> X		
	end.

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
	sched_ws_strategies:set_default(),
	set_hubs_only(false).

hubs_only() ->
	erlang:system_info(scheduler_hubs_only).

set_hubs_only(Bool) when Bool == true orelse Bool == false ->
	erlang:system_flag(scheduler_hubs_only, Bool).

hub_process() ->
	erlang:system_info(hub_process).

set_hub_process(Bool) ->
	erlang:system_flag(hub_process, Bool).

hub_processes() ->
	erlang:system_info(hub_processes).

hub_processes_count() ->
	erlang:system_info(hub_processes_count).

hub_process_spawn_flag () ->
	hub_process.


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


% Memory
memory_allocation_policy() ->
	case proc_mem_state() band 1 of 
		0 -> default;
		1 -> preferred
	end.

deferred_memory_allocation() ->
	case proc_mem_state() band 2 of 
		0 -> false;
		2 -> true
	end.

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

proc_mem_state() ->
	erlang:system_info(proc_mem_state).