%% Author: emilio
%% Created: 23 août 2012

-module(bench).
-export([available_benchmarks/0, bench/0]).

-compile(export_all).

%%
%% API Functions
%%

available_benchmarks() ->
	[map_reduce, mr, big_bang].

bench() ->
	do (all, "", small, 1, fun() -> ok end).

do (all, Prefix, Size, Times, InitFun) ->
	do (available_benchmarks(), Prefix, Size, Times, InitFun);
do (Bench, Prefix, Size, Times, InitFun) when is_atom(Bench) ->
	do ([Bench], Prefix, Size, Times, InitFun);
do (_, _, _, 0, _)->
	ok;
do (Benchs, Prefix, Size, Times, InitFun) 
  when is_list(Prefix) andalso is_atom(Size) andalso is_integer(Times) andalso is_function(InitFun)->
	Res = [init_do (Bench, InitFun, Size) || Bench <- Benchs],
	Pattern = string:join(["~p" || _ <- Benchs], "\t"),
	io:format("~s\t" ++ Pattern ++ "~n", [Prefix | Res]),
	do (Benchs, Prefix, Size, Times - 1, InitFun).

init_do (Bench, InitFun, Size) ->
	InitFun(),
	do_bench(Bench, Size).

do_bench (big_bang, Size) ->
	do_big_bang(Size);
do_bench (map_reduce, Size) ->
	do_map_reduce(Size);
do_bench (mr, Size) ->
	do_mr(Size).

						
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
	Seeds = generate_seeds (Size),
	{Time, _Val} = timer:tc(fun() -> bench_map_reduce (Seeds) end),
	Time.


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

