%% Author: emilio
%% Created: 23 août 2012

-module(bench).

%%
%% Include files
%%
-compile(export_all).

%%
%% API Functions
%%

bench() ->
	bench(small).

bench(Size) ->
	schedulinga:bind_no_spread(),
	Initial_Placement_Strategies = [0, 1, 2], %Default, Random, Circular
	Migration_Strategies = [0, 1], %Default, Disabled
	Work_Stealing_Strategies = [0, 1], %Default, Disabled
	io:format("prefix\tmap_reduce\tmr\tbig_bang~n", []),
	[configure_do(IPS, MS, WSS, Size) || IPS <- Initial_Placement_Strategies,
										   MS <-  Migration_Strategies,
										   WSS <- Work_Stealing_Strategies],
	ok.
%% 		||
%% 	
%% 	Strats = [
%% 		{"Default", fun scheduling:set_placement_default/0, []},
%% 		{"Random", fun scheduling:set_placement_random/0, []},
%% 		{"Circular", fun scheduling:set_placement_circular/0, []},
%% 		{"FS-1", fun scheduling:set_placement_fixed_set/1, [[1]]},
%% 		{"FS-1,2", fun scheduling:set_placement_fixed_set/1, [[1,2]]},
%% 		{"FS-1,3", fun scheduling:set_placement_fixed_set/1, [[1,3]]},
%% 		{"FS-1,7", fun scheduling:set_placement_fixed_set/1, [[1,7]]},
%%  		{"FS-1PSckt", fun scheduling:set_placement_fixed_set/1, [[1,7,13,19]]},
%%  		{"FS-Odd", fun scheduling:set_placement_fixed_set/1, [[1,3,5,7,9,11,13,15,17,19,21,23]]}
%% 	],
%% 	io:format("prefix\tmap_reduce\tmr\tbig_bang~n", []),
%% 	lists:foreach (
%% 		fun ({Pre, Str, Par}) ->
%% 			apply(Str, Par),
%% 			do(Pre, small)
%% 		end,
%% 		Strats
%% 	).

configure_do (IPS, MS, WS, Size) ->
	erlang:system_flag(scheduler_ip_strategy, IPS),
	erlang:system_flag(scheduler_migration_strategy, MS),
	erlang:system_flag(scheduler_ws_strategy, WS),
	do (integer_to_list(IPS) ++ integer_to_list(MS) ++ integer_to_list(WS), Size).

do() ->
	do(small).

do (Size) ->
	do ("", Size).

do (Prefix, Size) when is_atom(Size) ->
	Times = 1,
	T1 = lists:sum([do_map_reduce(Size) || _ <-lists:seq(1,Times)]) / Times,
	T2 = lists:sum([do_mr (Size) || _ <-lists:seq(1,Times)]) / Times,
	T3 = lists:sum([do_big_bang(Size) || _ <-lists:seq(1,Times)]) / Times,
	io:format("~s\t~p\t~p\t~p~n", [Prefix, T1, T2, T3]).

do_big_bang(small) ->
	big:bang(1000);
do_big_bang(big) ->
	big:bang(2000);
do_big_bang(huge) ->
	big:bang(4000).

do_map_reduce(small) ->
	do_map_reduce(10);
do_map_reduce(big) ->
	do_map_reduce(20);
do_map_reduce(huge) ->
	do_map_reduce(50);
do_map_reduce(Size) when is_integer(Size) ->	
	%io:format("Generating seeds..", []),
	Seeds = generate_seeds (Size),
	%io:format("Ok.~n", []),
	%io:format("Starting mapreduce...~n", []),
	{Time, _Val} = timer:tc(fun() -> bench_map_reduce (Seeds) end),
	%io:format("mapreduce: ~p~n~p~n", [Time/1000000, Val]).
	Time.

do_mr(small) ->
	do_mr(2000);
do_mr(big) ->
	do_mr(10000);
do_mr(huge) ->
	do_mr(20000);
do_mr (Size) when is_integer(Size) ->
	%io:format("Generating seeds..", []),
	Seeds = generate_seeds (Size),
	%io:format("Ok.~n", []),
	%io:format("Starting mr...~n", []),
	{Time, _Val} = timer:tc(fun() -> bench_mr (Seeds) end),
	%io:format("mr: ~p~n~p~n", [Time/1000000, Val]).
	Time.

%%
%% Local Functions
%%

bench_map_reduce(Seeds) ->
	F1 = fun generate_numbers/2,
	F2 = fun count_numbers/3,	
	L1 = mapreduce:mapreduce(F1, F2, [], Seeds),
	lists:reverse(lists:sort(L1)).

generate_numbers (Pid, Seed) ->
	random:seed(Seed),
	[Pid ! {random:uniform(10), 1} || _ <- lists:seq(1, 10000)].

count_numbers(Key, Vals, A) ->
	[{length(Vals), Key}|A].

bench_mr (L) ->
	Map = fun (Seed) -> 
		random:seed(Seed),
		lists:foldl (
			fun (_Pos, Dict) ->
				Elem = random:uniform(10),
				case dict:find(Elem, Dict) of
					error ->
						dict:store(Elem, 1, Dict);
					{ok, Value} ->
						dict:store(Elem, Value + 1, Dict)
				end
			end,
			dict:new(),		 
			lists:seq(1, 10000))
	end,
	Red = fun (Dic1, Dic2) ->
		dict:merge(
			fun (_Key, V1, V2) -> V1 + V2 end,
			Dic1,
			Dic2
		)
	end,
	FinalDict = mr:mr(Map, Red, 0, L),
	FinalList = [invert_tuple(X) || X <- dict:to_list(FinalDict)],
	lists:reverse(lists:sort(FinalList)).
	
invert_tuple ({A, B}) -> 
	{B, A}.

generate_seeds (N) ->
	[generate_seed() || _ <- lists:seq(1, N)].
generate_seed () ->
	{random:uniform(1000000), random:uniform(1000000), random:uniform(1000000)}. 

