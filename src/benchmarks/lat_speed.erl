-module(lat_speed).

-export([run_tests/0, run_tests/4]).

%local export
-export([sender_loop/5, receiver_loop/2, generate_payloads/2]).

combinations() -> %OS CPU ID
	[ {0, 0}, {0, 1}, {0, 2} ]. %Warp
	%[ {0, 0}, {0, 12}, {0, 4}, {0, 1} ]. %idkonn
	%[ {0, 1}, {0, 4}]. %idrouille

run_tests() -> %defaults
	io:format("Starting at: ~p\n", [utils:time_to_number(now())]),
	run_tests("/tmp/res/", 1000, 10, 10).

run_tests(OutputPath, N, MinPayloadSize, MaxPayloadSize) ->
	HeapSize = 1 bsl (MaxPayloadSize + 2),
	VHeapSize = 200 * HeapSize,
	erlang:system_flag(min_bin_vheap_size, VHeapSize),	
	erlang:system_flag(min_heap_size, HeapSize),
	io:format("~p (Requested: ~p)~n~p (Requested: ~p)~n",
		[erlang:system_info(min_bin_vheap_size), VHeapSize, erlang:system_info(min_heap_size), HeapSize]),
	erlang:system_flag(scheduler_bind_type, no_spread),
	io:format("Bind type: ~p ~n", [erlang:system_info(scheduler_bind_type)]),
	io:format("Bindings: ~p ~n", [erlang:system_info(scheduler_bindings)]),	
	io:format("Running with CPUS: ~p~n", [combinations()]),
	[run_tests(OutputPath, S, R, N, MinPayloadSize, MaxPayloadSize) || {S, R} <- combinations()],
	ok.

run_tests(OutputPath, SCpu, RCpu, N, MinPayloadSize, MaxPayloadSize) ->
	io:format("Started ~p -> ~p  : ~p\n", [SCpu, RCpu, utils:time_to_number(now())]),
	[do_run_tests(OutputPath, SCpu, RCpu, N, Payload) || Payload <- generate_payloads(MinPayloadSize, MaxPayloadSize)],
	io:format("Ended ~p -> ~p  : ~p\n", [SCpu, RCpu, utils:time_to_number(now())]),
	ok.		  
	
do_run_tests(OutputPath, SCpu, RCpu, N, Payload) ->	
	Label = utils:to_string(SCpu) ++ "-" ++ utils:to_string(RCpu),
	Size = utils:to_string((byte_size(list_to_binary(Payload)))),
	Filename = OutputPath ++ "Latency-" ++ Label ++ "-" ++ Size,
	file:delete(Filename),
	{ok, File} = file:open(Filename, [read, write]),
	io:fwrite(File, "~s~n", [Size]),
	Receiver = spawn_link(?MODULE, receiver_loop, [self(), RCpu]),
	Sender = spawn_link(?MODULE, sender_loop, [Receiver, N, SCpu, Payload, File]),
	Receiver ! Sender,
	receive ended -> ok end,
	file:close(File),
	garbage_collect().


sender_loop (Receiver, Count, CPUId, Payload, File) ->
	utils:set_cpu(CPUId),
	CPUId = utils:current_cpu(),
	receive ok -> ok end,
	sender_loop(Receiver, Count, Payload, File).
sender_loop(Receiver, 0, _Payload, _File) ->
	Receiver ! stop;
sender_loop(Receiver, Count, Payload, File) ->
	Before = os:timestamp(),
	Receiver ! Payload,
	receive 
		_Payload -> 
			Diff = timer:now_diff(os:timestamp(), Before),
			D2 = if 
				Diff == 0 -> 
					0.000001;
				true -> 
					Diff
			end,
			io:fwrite(File,"~p~n", [D2]),
			sender_loop(Receiver, Count -1, Payload, File)
	end.

receiver_loop(Orig, Cpu) ->
	utils:set_cpu(Cpu),
	Cpu = utils:current_cpu(),
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
			Pl = [42 || _ <- lists:seq(1, 1 bsl Min)],
			generate_payloads(Min + 1, Max, [Pl | Acc])
	end.


