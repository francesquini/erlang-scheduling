%% Author: emilio
%% Created: 17/04/2012
-module(gnuplot).

-export([
	gnuplot_close/1,
	gnuplot_create_file/1,
	gnuplot_plot/3,
	gnuplot_plot/5,
	gnuplot_set_output_png/2,
	gnuplot_set_log/2,
	gnuplot_set_log_x/1,
	gnuplot_set_log_y/1,
	gnuplot_set_boxwidth/2,
	gnuplot_set_style/2,
	gnuplot_set_title/2,
	gnuplot_set_xlabel/2,
	gnuplot_set_xrange/3,	
	gnuplot_set_ylabel/2,
	gnuplot_set_yrange/3,
	gnuplot_set_xtics/2,
	gnuplot_unset_log_x/1,
	gnuplot_unset_log_y/1,
	gnuplot_variable/3,
	gnuplot_run/1]).

-import (utils, [to_string/1, to_dbstring/1]).

gnuplot_create_file(Filename) ->
	file:open(Filename, [read, write]).

gnuplot_set_output_png (File, OutputFileName) ->
	gnuplot_write_line(File, "set terminal png size 1024, 768"),
	gnuplot_write_line(File, "set output  " ++ to_dbstring(OutputFileName)).

gnuplot_set_title (File, Title) ->
	gnuplot_set(File, "title", to_dbstring(Title)).
gnuplot_set_xlabel (File, Label) ->
	gnuplot_set(File, "xlabel", to_dbstring(Label)).
gnuplot_set_ylabel (File, Label) ->
	gnuplot_set(File, "ylabel", to_dbstring(Label)).	

gnuplot_set_log_x (File) ->
	gnuplot_set_log(File, "x").
gnuplot_set_log_y (File) ->
	gnuplot_set_log(File, "y").
gnuplot_set_log (File, Axis) ->
	gnuplot_set(File, "log", Axis).

gnuplot_unset_log_x (File) ->
	gnuplot_unset_log(File, "x").
gnuplot_unset_log_y (File) ->
	gnuplot_unset_log(File, "y").
gnuplot_unset_log (File, Axis) ->
	gnuplot_unset(File, "log", Axis).

gnuplot_set_xrange (File, Start, End) ->
	gnuplot_set_range(File, "x", Start, End).
gnuplot_set_yrange (File, Start, End) ->
	gnuplot_set_range(File, "y", Start, End).
gnuplot_set_range (File, Axis, Start, End) ->
	gnuplot_set(File, Axis ++ "range", "[" ++ to_string(Start) ++ ":" ++ to_string(End) ++"]").

gnuplot_set_xtics (File, Tics) ->
	gnuplot_set (File, "xtics", Tics).

gnuplot_set_boxwidth(File, Width) ->
	gnuplot_set (File, "boxwidth", Width).

gnuplot_set_style(File, Val) ->
	gnuplot_set (File, "style", Val).

gnuplot_variable (File, Var, Value) ->
	gnuplot_write_line(File, Var ++ "=" ++ Value).

gnuplot_set (File, Var, Par) ->
	gnuplot_write_line (File, "set " ++ Var ++ " " ++ to_string(Par)).
gnuplot_unset (File, Var, Par) ->
	gnuplot_write_line (File, "unset " ++ Var ++ " " ++ Par).

gnuplot_plot (File, Filename, Cols) ->
	gnuplot_plot (File, Filename, Cols, "p", "").	
gnuplot_plot (File, Filename, Cols, With, Suffix) ->
	gnuplot_write_line(File, "plot " ++ to_dbstring(Filename) ++ " u " ++ Cols ++ " w " ++ With ++ " notitle" ++ Suffix).

gnuplot_close(File) ->
	file:close(File).

gnuplot_run (Filename) ->
 	[] = os:cmd("gnuplot " ++ Filename),
	ok.

gnuplot_write_line (File, Line) ->
	io:fwrite(File, Line ++ "~n", []).