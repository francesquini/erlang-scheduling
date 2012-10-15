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

run ([Size, IP, MS, WS]) -> 
	bench(Size, IP, MS, WS, 1).

bench() ->
	bench(small, 1).

bench(Size, Times) ->
	scheduling:bind_no_spread(),
	Initial_Placement_Strategies = [default, random, circular], %Default, Random, Circular
	Migration_Strategies = [default, disabled], %Default, Disabled
	Work_Stealing_Strategies = [default, disabled], %Default, Disabled
	[bench(Size, IPS, MS, WSS, Times) || IPS <- Initial_Placement_Strategies,
										   MS <-  Migration_Strategies,
										   WSS <- Work_Stealing_Strategies],
	ok.

bench(Size, IP, MS, WS, Times) ->
	scheduling:bind_no_spread(),
	io:format("prefix\tmap_reduce\tmr\tbig_bang~n", []),
	configure_do(IP, MS, WS, Size, Times),
	ok.

configure_do (IPS, MS, WS, Size, Times) ->
	sched_ip_strategies:set_strategy(IPS),
	sched_migration_strategies:set_strategy(MS),
	sched_ws_strategies:set_strategy(WS),
	do (atom_to_list(IPS) ++ "," ++ atom_to_list(MS) ++ "," ++ atom_to_list(WS), Size, Times).

do(Times) ->
	do(small, Times).

do (Size, Times) ->
	do ("", Size, Times).

do (Prefix, Size, Times) ->
	do (Prefix, Size, Times, fun() -> ok end).

do (Prefix, Size, Times, InitFun) when is_list(Prefix) andalso is_atom(Size) andalso is_integer(Times) andalso is_function(InitFun)->
%	T1 = lists:sum([init_do_map_reduce(InitFun, Size) || _ <-lists:seq(1,Times)]) / Times,
	T1 = 0,
	T2 = lists:sum([init_do_mr (InitFun, Size) || _ <-lists:seq(1,Times)]) / Times,
	T3 = lists:sum([init_do_big_bang(InitFun, Size) || _ <-lists:seq(1,Times)]) / Times,
	io:format("~s\t~p\t~p\t~p~n", [Prefix, T1, T2, T3]).


init_do_big_bang(InitFun, Size) ->
	InitFun(),
	do_big_bang(Size).

do_big_bang(small) ->
	big:bang(1000);
do_big_bang(big) ->
	big:bang(2000);
do_big_bang(huge) ->
	big:bang(4000).


init_do_map_reduce(InitFun, Size) ->
	InitFun(), 
	do_map_reduce(Size).

do_map_reduce(small) ->
	do_map_reduce(10);
do_map_reduce(big) ->
	do_map_reduce(20);
do_map_reduce(huge) ->
	do_map_reduce(50);
do_map_reduce(Size) when is_integer(Size) ->	
	Seeds = generate_seeds (Size),
	{Time, _Val} = timer:tc(fun() -> bench_map_reduce (Seeds) end),
	Time.


init_do_mr(InitFun, Size) ->
	InitFun(),
	do_mr(Size).

do_mr(small) ->
	do_mr(2000);
do_mr(big) ->
	do_mr(10000);
do_mr(huge) ->
	do_mr(20000);
do_mr (Size) when is_integer(Size) ->
	Seeds = generate_seeds (Size),
	{Time, _Val} = timer:tc(fun() -> bench_mr (Seeds) end),
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

