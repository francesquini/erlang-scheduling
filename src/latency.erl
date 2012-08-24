-module(latency).

-export([run_tests/0, run_tests/4]).

%local export
-export([sender_loop/4, receiver_loop/2, generate_payloads/1]).

run_tests() -> %defaults
	run_tests("/tmp/Run10kIgn10.txt", 1000, 16, 100).

run_tests(OutputFileName, N, PayloadSize, IgnoreFirst) ->
	file:delete(OutputFileName),
	{ok, File} = file:open(OutputFileName, [read, write]),
	Res = run_tests(N, PayloadSize, IgnoreFirst),
	write_results(File, Res, IgnoreFirst, N),
	file:close(File).

run_tests(N, PayloadSize, IgnoreFirst) ->
	HeapSize = 1 bsl (PayloadSize + 2),
	VHeapSize = 200 * HeapSize,
	erlang:system_flag(min_bin_vheap_size, VHeapSize),	
	erlang:system_flag(min_heap_size, HeapSize),
	io:format("~p (Requested: ~p)~n~p (Requested: ~p)~n",
		[erlang:system_info(min_bin_vheap_size), VHeapSize, erlang:system_info(min_heap_size), HeapSize]),
	erlang:system_flag(scheduler_bind_type, no_spread),
	Scheds = lists:seq(1, utils:scheduler_count()),
	io:format("Bind type: ~p ~n", [erlang:system_info(scheduler_bind_type)]),
	io:format("Bindings: ~p ~n", [erlang:system_info(scheduler_bindings)]),	
	io:format("Running with schedulers: ~p~n", [Scheds]),
	[{S, R, run_tests(S, R, N, PayloadSize, IgnoreFirst)} || S <- Scheds, R <- Scheds].

run_tests(SSched, RSched, N, PayloadSize, IgnoreFirst) ->
	Res = [{byte_size(list_to_binary(Payload)), do_run_tests(SSched, RSched, N, Payload)}
			|| Payload <- generate_payloads(PayloadSize)],
	[{
	  Size, 
	  utils:min_max_average(List), 
	  utils:min_max_average(lists:nthtail(IgnoreFirst, lists:reverse(List)))
	 } 
	 || {Size, List} <- Res].		  
	
do_run_tests(SSched, RSched, N, Payload) ->	
	Receiver = spawn_link(?MODULE, receiver_loop, [self(), RSched]),
	Sender = spawn_link(?MODULE, sender_loop, [Receiver, N, SSched, Payload]),
	Receiver ! Sender,
	receive 
		Acc -> Acc			
	end.


sender_loop (Receiver, Count, Scheduler, Payload) ->
	utils:set_scheduler(Scheduler),
	Scheduler = utils:current_scheduler(),
	sender_loop(Receiver, Count, Payload).
sender_loop(Receiver, 0, _Payload) ->
	Receiver ! stop;
sender_loop(Receiver, Count, Payload) ->
	Receiver ! {now(), Payload},
	receive
		ok -> sender_loop(Receiver, Count -1, Payload)
	end.

receiver_loop(Orig, Scheduler) ->
	utils:set_scheduler(Scheduler),
	Scheduler = utils:current_scheduler(),
	receive 
		Sender when is_pid(Sender) ->
			utils:flush_message_queue(),
			Sender ! ok,
			receiver_loop(Orig, Sender, [])
	end.
receiver_loop(Orig, Sender, Acc) ->
	receive
		stop ->
			Orig ! Acc;
		{Start, _} -> 
			End = now(),			
			TEnd = utils:time_to_number(End),
			TStart =  utils:time_to_number(Start),
			NAcc = [TEnd - TStart | Acc],
			garbage_collect(),
			Sender ! ok,
			receiver_loop(Orig, Sender, NAcc)
	end.

generate_payloads(N) ->
	generate_payloads(N, [[42]]).
generate_payloads(0, Acc) ->
	Acc;
generate_payloads(N, Acc = [H | _]) ->
	generate_payloads(N - 1, [H ++ H | Acc]).
	
write_results(File, Res, IgnoreFirst, N) ->
	io:fwrite(File, "#N=~p\tIgn:~p~n", [N, IgnoreFirst]),
	io:fwrite(File, "#~s\t~s\t~s\t~s\t~s\t~s\t~s\t~s\t~s~n", ["From", "To", "Size", "Min", "Max", "Avg", "MinIgn", "MaxIgn", "AvgIgn"]),
	[write_result(File, S, R, List) || {S, R, List} <- Res].
write_result (File, S, R, List) ->
	[io:fwrite(File, "~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p~n", [S, R, Size, Min, Max, Avg, MinIgn, MaxIgn, AvgIgn]) 
		|| {Size, {Min, Max, Avg}, {MinIgn, MaxIgn, AvgIgn}} <- List].
	