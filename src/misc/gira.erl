%% Author: emilio
%% Created: 14 oct. 2012
%% Description: TODO: Add description to gira
-module(gira).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([gira/1]).

%%
%% API Functions
%%

gira(0) ->
	ok;
gira(N) ->
	spawn(fun() -> loop (0) end),
	gira (N - 1).	  
	  
loop(N) ->
	loop(N + 1).
