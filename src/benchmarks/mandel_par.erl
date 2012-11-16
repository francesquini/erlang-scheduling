-module(mandel_par).

-include("mandel.hrl").

-export([run/1, calculate_image/1]).

-import(mandel_shared, [calculate_pixel/3]).

run(Size) when is_integer(Size) ->
	mandel_shared:run(Size, fun calculate_image/1).

calculate_image (Mandel) ->
	Size = Mandel#mandel.size,
	Orig = self(),	
	[ spawn_link(fun () -> calculate_column (X, Mandel, Orig) end) || X <- lists:seq(1, Size)],	
	lists:flatten(array:to_list(receive_columns(array:new(Size), Size))).
	
receive_columns(Columns, 0) ->
	Columns;
receive_columns(Columns, Remaining) ->
	receive
		{Column, Pixels} ->
			receive_columns (array:set(Column - 1, Pixels, Columns), Remaining -1)
	end.

calculate_column (Column, Mandel, Orig) ->
	Pixels = [calculate_pixel(Mandel, Column, Y) || Y <- lists:seq(1, Mandel#mandel.size)],
	Orig ! {Column, Pixels}.

