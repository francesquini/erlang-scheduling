%% Author: emilio
%% Created: 17/04/2012
-module(utils).

%% List Functions
-export([min_max_average/1]).

%% Type Conversion
-export([to_string/1, to_dbstring/1, to_int/1]).

%% Math Functions
-export([floor/1, ceiling/1, is_prime/1]).

%% Processes
-export([spawn_and_phone_home/1, spawn_and_phone_home/3, receive_times/1]).

%% Time
-export([time_to_number/1]).

%% Messaging
-export ([flush_message_queue/0]).

%% Misc
-export ([times_repeat/2]).


%%%%%%%%%%%%%%%%%%%%
%% List Functions %%
%%%%%%%%%%%%%%%%%%%%

min_max_average(X) -> min_max_average(X, 0, 0, undef, undef).
min_max_average([H|T], Length, Sum, Min, Max) ->
	NMin = case Min of
				undef -> H;
				Min when H < Min -> H;
				Min -> Min
			end,
	NMax = case Max of
				undef -> H;
				Max when H > Max -> H;
				Max -> Max
			end,	
	min_max_average(T, Length + 1, Sum + H, NMin, NMax);
min_max_average([], Length, Sum, Min, Max) ->
	{Min, Max, Sum / Length}.

%%%%%%%%%%%%%%%%%%%%%
%% Type Conversion %%
%%%%%%%%%%%%%%%%%%%%%

to_string (X) when is_integer(X)->
	integer_to_list(X);
to_string (X) when is_float(X)->
	float_to_list(X);
to_string (X) when is_atom(X)->
	atom_to_list(X);
to_string (X) when is_pid(X)->
	pid_to_list(X);
to_string (X) when is_list(X)->
	X;
to_string (X) ->
	lists:flatten(io_lib:format("~p", [X])).

to_dbstring (X) ->
	lists:flatten(io_lib:format("~p", [to_string(X)])).


to_int(X) when is_list(X) ->
	{Res, _} = string:to_integer(X),
	Res.


%%%%%%%%%%%%%%%%%%%%
%% Math Functions %%
%%%%%%%%%%%%%%%%%%%%

floor(X) when is_number (X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) when is_number (X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

is_prime (N) when is_integer(N) andalso N > 2 andalso N rem 2 == 0 ->
	2;
is_prime (N) when is_integer(N) ->
	First = 3,
	Last = ceiling(math:sqrt(N)),
	is_prime (N, First, Last).

is_prime (_N, First, Last) when First > Last ->
	0;
is_prime (N, First, _Last) when N rem First == 0 ->
	First;
is_prime (N, First, Last) ->
	is_prime(N, First + 2, Last).

%%%%%%%%%%%%%%%
%% Processes %%
%%%%%%%%%%%%%%%

spawn_and_phone_home (Module, Function, Args) ->
	spawn_and_phone_home (
		fun () ->
			apply(Module, Function, Args)
		end).

spawn_and_phone_home (Fun) when is_function(Fun, 0)->
	Me = self(),
	NewFun = fun() ->
		Fun(),
		Me ! {phone_home, self()}
	end,
	spawn_link(NewFun).

receive_times (Pid) when is_pid(Pid) ->
	receive_times([Pid]);
receive_times ([]) ->
	ok;
receive_times ([Pid|T]) when is_pid(Pid) ->
	receive
		{phone_home, Pid} ->
			receive_times(T)
	end.

%%%%%%%%%%
%% Time %%
%%%%%%%%%%

time_to_number ({MegaSeconds, Secs, Microsecs}) ->
	(MegaSeconds * 1000000 + Secs) * 1000000 + Microsecs.


%%%%%%%%%%%%%%%
%% Messaging %%
%%%%%%%%%%%%%%%

flush_message_queue () ->
	receive
		_Any -> flush_message_queue()
	after 
		0 -> ok
	end.

%%%%%%%%%%
%% Misc %%
%%%%%%%%%%

times_repeat (0, _) -> 
	ok;
times_repeat (Times, Fun) ->
	Fun(),
	times_repeat(Times - 1, Fun).
	
	