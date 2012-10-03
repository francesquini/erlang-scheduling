%% Author: emilio
%% Created: 23 août 2012
%% Description: TODO: Add description to mr
-module(mr).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([mr/4, mr_ex/1]).

%%
%% API Functions
%%

mr (Map, Red, Id, L) ->
	Parent = self(),
	spawn_link(fun () -> mr_do(Parent, Map, Red, Id, L) end),
	receive Res -> Res end.

mr_ex (N) ->
	L = [random:uniform() || _ <- lists:seq(1, N)],
	Map = fun (X) -> utils:floor(X * 10) end,
	Red = fun (A, B) -> A + B end,
	mr (Map, Red, 0, L). 

%%
%% Local Functions
%%

split(L) ->
	split (L, [], []).

split ([], L1, L2) ->
	{L1, L2};
split ([E], L1, L2) ->
	{[E|L1], L2};
split ([E1, E2|T], L1, L2) ->
	split (T, [E1|L1], [E2|L2]).

mr_do (Parent, _Map, _Red, Id, []) ->
	Parent ! Id;
mr_do (Parent, Map, _Red, _Id, [E]) ->
	Parent ! apply (Map, [E]);
mr_do (Parent, Map, Red, Id, L) ->
	{L1, L2} = split (L),
	NParent = self(),
	spawn_link(fun () -> mr_do(NParent, Map, Red, Id, L1) end),
	spawn_link(fun () -> mr_do(NParent, Map, Red, Id, L2) end),
	Acc = receive A ->		
		receive B ->
			apply (Red, [A, B])
		end
	end,	
	Parent ! Acc.	
	