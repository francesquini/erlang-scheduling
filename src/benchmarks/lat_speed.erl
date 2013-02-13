-module(lat_speed).

-export([run_tests/1, run_tests/5]).

%local export
-export([sender_loop/6, receiver_loop/2, generate_payloads/2]).

combinations(warp) ->
	[{0, 0}, {0, 1}, {0, 2}]; %Warp
combinations(idrouille) ->
	[{0, 0}, {0, 1}, {0, 4}]; %idrouille
combinations(idkonn) ->
	[{0, 0}, {0, 12}, {0, 4}, {0, 1}]. %idkonn

run_tests([Machine]) ->
	run_tests(Machine);
run_tests(Machine) -> %defaults
	io:format("Starting at: ~p\n", [utils:time_to_number(now())]),
	run_tests("/tmp/", 10000, 1, 15, Machine).

run_tests(OutputPath, N, MinPayloadSize, MaxPayloadSize, Machine) ->
	HeapSize = 1 bsl (MaxPayloadSize + 2),
	VHeapSize = 200 * HeapSize,
	erlang:system_flag(min_bin_vheap_size, VHeapSize),	
	erlang:system_flag(min_heap_size, HeapSize),
	io:format("~p (Requested: ~p)~n~p (Requested: ~p)~n",
		[erlang:system_info(min_bin_vheap_size), VHeapSize, erlang:system_info(min_heap_size), HeapSize]),
	erlang:system_flag(scheduler_bind_type, no_spread),
	io:format("Bind type: ~p ~n", [erlang:system_info(scheduler_bind_type)]),
	io:format("Bindings: ~p ~n", [erlang:system_info(scheduler_bindings)]),	
	io:format("Running with CPUS: ~p~n", [combinations(Machine)]),
	FileName = OutputPath ++ "Latency",
 	file:delete(FileName),
	{ok, File} = file:open(FileName, [read, write]),
	io:fwrite(File, "from\tto\tsize\ttime\n", []),
	[run_tests(File, S, R, N, MinPayloadSize, MaxPayloadSize) || {S, R} <- combinations(Machine)],
	file:close(File),
	ok.

run_tests(File, SCpu, RCpu, N, MinPayloadSize, MaxPayloadSize) ->
	io:format("Started ~p -> ~p  : ~p\n", [SCpu, RCpu, utils:time_to_number(now())]),	
	[do_run_tests(File, SCpu, RCpu, N, Payload) || Payload <- generate_payloads(MinPayloadSize, MaxPayloadSize)],	
	io:format("Ended ~p -> ~p  : ~p\n", [SCpu, RCpu, utils:time_to_number(now())]),
	ok.		  
	
do_run_tests(File, SCpu, RCpu, N, Payload) ->	
	Receiver = spawn_link(?MODULE, receiver_loop, [self(), RCpu]),
	Sender = spawn_link(?MODULE, sender_loop, [Receiver, N, SCpu, RCpu, Payload, File]),
	Receiver ! Sender,
	receive ended -> ok end,
	garbage_collect().


sender_loop (Receiver, Count, SCpu, RCpu, Payload, File) ->
	scheduling:set_cpu(SCpu),
	SCpu = scheduling:current_cpu(),
	receive ok -> ok end,
	sender_loop2(Receiver, SCpu, RCpu, Count, Payload, File).
sender_loop2(Receiver, _SCpu, _RCpu, 0, _Payload, _File) ->
	Receiver ! stop;
sender_loop2(Receiver, SCpu, RCpu, Count, Payload, File) ->
	Before = os:timestamp(),
	Receiver ! Payload,
	receive 
		_Payload2 -> 
			Diff = timer:now_diff(os:timestamp(), Before),
			D2 = if 
				Diff == 0 -> 
					0.000001;
				true -> 
					Diff
			end,
			Size = byte_size(list_to_binary(Payload)),
			%Each list element uses an extra word
			%http://www.erlang.org/doc/efficiency_guide/advanced.html
			io:fwrite(File,"~p\t~p\t~p\t~p~n", [SCpu, RCpu, Size * 2, D2]), 
			sender_loop2(Receiver, SCpu, RCpu, Count -1, Payload, File)
	end.

receiver_loop(Orig, Cpu) ->
	scheduling:set_cpu(Cpu),
	Cpu = scheduling:current_cpu(),
	receive 
		Sender when is_pid(Sender) ->
			Sender ! ok,
			do_receiver_loop(Orig, Sender)
	end.

do_receiver_loop(Orig, Sender) ->
	receive
		stop ->
			Orig ! ended;
		 Payload ->
			Sender ! Payload,
			do_receiver_loop(Orig, Sender)
	end.

generate_payloads(Min, Max) ->
	lists:reverse(generate_payloads(Min, Max, [])).


generate_payloads(Min, Max, Acc) ->
	case Min > Max of
		true -> Acc;
		false -> 
			Pl = [42 || _ <- lists:seq(0, (1 bsl (Min - 1)) - 1)],
			generate_payloads(Min + 1, Max, [Pl | Acc])
	end.


