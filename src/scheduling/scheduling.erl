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
	memory_allocation_policy/0, deferred_memory_allocation/0,

	%Scheduler distances
	scheduler_set_distances/1, numa32_scheduler_distances/0
		 		  
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


%Scheduler Distances

numa32_scheduler_distances() ->
	[[1,5,9,13,17,21,25,29,2,3,4,6,7,8,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28,30,31,32],
	[2,6,10,14,18,22,26,30,3,4,5,7,8,9,11,12,13,15,16,17,19,20,21,23,24,25,27,28,29,31,32,1],
	[3,7,11,15,19,23,27,31,4,5,6,8,9,10,12,13,14,16,17,18,20,21,22,24,25,26,28,29,30,32,1,2],
	[4,8,12,16,20,24,28,32,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,1,2,3],
	[5,1,9,13,17,21,25,29,6,7,8,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28,30,31,32,2,3,4],
	[6,2,10,14,18,22,26,30,7,8,9,11,12,13,15,16,17,19,20,21,23,24,25,27,28,29,31,32,1,3,4,5],
	[7,3,11,15,19,23,27,31,8,9,10,12,13,14,16,17,18,20,21,22,24,25,26,28,29,30,32,1,2,4,5,6],
	[8,4,12,16,20,24,28,32,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,1,2,3,5,6,7],
	[9,1,5,13,17,21,25,29,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28,30,31,32,2,3,4,6,7,8],
	[10,2,6,14,18,22,26,30,11,12,13,15,16,17,19,20,21,23,24,25,27,28,29,31,32,1,3,4,5,7,8,9],
	[11,3,7,15,19,23,27,31,12,13,14,16,17,18,20,21,22,24,25,26,28,29,30,32,1,2,4,5,6,8,9,10],
	[12,4,8,16,20,24,28,32,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,1,2,3,5,6,7,9,10,11],
	[13,1,5,9,17,21,25,29,14,15,16,18,19,20,22,23,24,26,27,28,30,31,32,2,3,4,6,7,8,10,11,12],
	[14,2,6,10,18,22,26,30,15,16,17,19,20,21,23,24,25,27,28,29,31,32,1,3,4,5,7,8,9,11,12,13],
	[15,3,7,11,19,23,27,31,16,17,18,20,21,22,24,25,26,28,29,30,32,1,2,4,5,6,8,9,10,12,13,14],
	[16,4,8,12,20,24,28,32,17,18,19,21,22,23,25,26,27,29,30,31,1,2,3,5,6,7,9,10,11,13,14,15],
	[17,1,5,9,13,21,25,29,18,19,20,22,23,24,26,27,28,30,31,32,2,3,4,6,7,8,10,11,12,14,15,16],
	[18,2,6,10,14,22,26,30,19,20,21,23,24,25,27,28,29,31,32,1,3,4,5,7,8,9,11,12,13,15,16,17],
	[19,3,7,11,15,23,27,31,20,21,22,24,25,26,28,29,30,32,1,2,4,5,6,8,9,10,12,13,14,16,17,18],
	[20,4,8,12,16,24,28,32,21,22,23,25,26,27,29,30,31,1,2,3,5,6,7,9,10,11,13,14,15,17,18,19],
	[21,1,5,9,13,17,25,29,22,23,24,26,27,28,30,31,32,2,3,4,6,7,8,10,11,12,14,15,16,18,19,20],
	[22,2,6,10,14,18,26,30,23,24,25,27,28,29,31,32,1,3,4,5,7,8,9,11,12,13,15,16,17,19,20,21],
	[23,3,7,11,15,19,27,31,24,25,26,28,29,30,32,1,2,4,5,6,8,9,10,12,13,14,16,17,18,20,21,22],
	[24,4,8,12,16,20,28,32,25,26,27,29,30,31,1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23],
	[25,1,5,9,13,17,21,29,26,27,28,30,31,32,2,3,4,6,7,8,10,11,12,14,15,16,18,19,20,22,23,24],
	[26,2,6,10,14,18,22,30,27,28,29,31,32,1,3,4,5,7,8,9,11,12,13,15,16,17,19,20,21,23,24,25],
	[27,3,7,11,15,19,23,31,28,29,30,32,1,2,4,5,6,8,9,10,12,13,14,16,17,18,20,21,22,24,25,26],
	[28,4,8,12,16,20,24,32,29,30,31,1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27],
	[29,1,5,9,13,17,21,25,30,31,32,2,3,4,6,7,8,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28],
	[30,2,6,10,14,18,22,26,31,32,1,3,4,5,7,8,9,11,12,13,15,16,17,19,20,21,23,24,25,27,28,29],
	[31,3,7,11,15,19,23,27,32,1,2,4,5,6,8,9,10,12,13,14,16,17,18,20,21,22,24,25,26,28,29,30],
	[32,4,8,12,16,20,24,28,1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31]].


scheduler_set_distances(Dists) ->
    [scheduler_distances(Line) || Line <- Dists];
    ok.
    
scheduler_distances([From | Tos]) ->
    scheduler_distances(From, Tos).

scheduler_distances(From, Tos) ->
    scheduler_distances(From, Tos, 1).

scheduler_distances(From, [], _Pos) ->
    ok;
scheduler_distances(From, [To |Tos], Pos) ->    
    erlang:system_flag(scheduler_distances, {From, To, Pos},
    scheduler_distances(From, Tos, Pos + 1).