-module(trace_analysis).

-include("proc.hrl").

%% Public exports
-export([analyze_file/1, generate_derived_files/1]).

%% Local exports
-export([analyze_duration/4, analyze_actor_count/3, analyze_execution_time/2, analyze_messages/2]).
-export([generate_plots/2, generate_duration_proc_time/2, execution_time_calculate/2, compress_new_edges/1]).

-import(utils, [spawn_and_phone_home/1, spawn_and_phone_home/3, receive_times/1, ceiling/1, time_to_number/1]).
-import(graphviz, [digraph/2, add_edge/5, to_dot/2, to_gif/1, to_svg/1]). 
-import(gnuplot, [
				  gnuplot_close/1,
				  gnuplot_create_file/1,
				  gnuplot_plot/3,
				  gnuplot_plot/5,
				  gnuplot_run/1,
				  gnuplot_set_boxwidth/2,
				  gnuplot_set_log_x/1,
				  gnuplot_set_log_y/1,
				  gnuplot_set_output_png/2,
				  gnuplot_set_style/2,
				  gnuplot_set_title/2,
				  gnuplot_set_xlabel/2,
				  gnuplot_set_xrange/3,
				  gnuplot_set_ylabel/2,
				  gnuplot_set_xtics/2,
				  gnuplot_unset_log_x/1,
				  gnuplot_unset_log_y/1,
				  gnuplot_variable/3]).

-define(COLOR_SCHEME, oranges9).
-define(NO_COLORS, 9).

-define(ANIMATION_WINDOW, 200000). %microseconds

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyze_file(Filename) ->
	
	[Start, End, PCount] = proc_prof:read_info_file(Filename),
	
	DurationPid = spawn_and_phone_home(?MODULE, analyze_duration, [self(), Filename, time_to_number(Start), time_to_number(End)]),
	CountPid = spawn_and_phone_home(?MODULE, analyze_actor_count, [self(), Filename, PCount]),
	ExecutionPid = spawn_and_phone_home(?MODULE, analyze_execution_time, [self(), Filename]),
	MessagesPid = spawn_and_phone_home(?MODULE, analyze_messages, [self(), Filename]),
	
	Handler = fun (T) ->
					   MessagesPid ! DurationPid ! CountPid ! ExecutionPid ! T
			  end,
	
	proc_prof:read_file(Filename, Handler),	
	receive_times([DurationPid, CountPid, ExecutionPid, MessagesPid]),
	io:format("Done reading files~n"),
	generate_derived_files (Filename, time_to_number(Start), time_to_number(End)),
	io:format("Done derived files~n").

generate_derived_files(Filename) ->
	[Start, End, _] = proc_prof:read_info_file(Filename),
	generate_derived_files (Filename, time_to_number(Start), time_to_number(End)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%%% MESSAGES %%% 
%%%%%%%%%%%%%%%%

analyze_messages (Parent, Filename) ->
	analyze_messages_loop(Parent, dict:new(), Filename, []).

analyze_messages_loop (Parent, Dict, Filename, Sizes) ->
	receive		
		{send, Pid, To, Ts, Size} ->
			update_graph_table(Parent, Dict, Filename, Pid, To, Ts, [Size | Sizes]);
		{send_to_non_existing_process, Pid, To, Ts, Size} ->
			update_graph_table(Parent, Dict, Filename, Pid, To, Ts, [Size | Sizes]);
		{end_of_trace} ->
			%Movie
			Pid1 = spawn_and_phone_home(fun()-> generate_intermediate_graphs(Dict, Filename) end),				
			%Complete graph
			Pid2 = spawn_and_phone_home(fun() ->
					WeightedEdges = get_edge_weights(Dict),
					write_weighted(WeightedEdges, Filename),
					Graph = generate_graph (WeightedEdges),
					to_dot(Graph, Filename ++".messages.dot")
				end),
			Pid3 = spawn_and_phone_home(fun() ->
				MesSizeFile = Filename ++ ".messages.size",
				{ok, File} = file:open(MesSizeFile, [read, write]),
				[io:fwrite(File, "~p~n", [Size]) || Size <- Sizes],
				file:close(File)					
			end),
			receive_times([Pid1, Pid2, Pid3]);
		_Other -> 
			analyze_messages_loop(Parent, Dict, Filename, Sizes)
	end.


write_weighted(WeightedEdges, FileName) ->
	Dest = FileName ++ ".messages.edges",
	{ok, File} = file:open(Dest, [read, write]),
	Dest2 = FileName ++ ".messages.count",
	{ok, Mcount} = file:open(Dest2, [read, write]),
	lists:foreach(fun({From, Tos}) ->
			[io:fwrite(File, "~p\t~p\t~p~n", [From, To, Weight]) || {To, Weight} <- Tos],
			NMes = lists:sum([W ||{_,W} <-Tos]),
			Difs = length(Tos),
			io:fwrite(Mcount, "~p\t~p\t~p~n", [From, NMes, Difs])	  
		end, 
		WeightedEdges),
	file:close(File),
	file:close(Mcount).


update_graph_table(Parent, Dict, Filename, From, To, Ts, Sizes) ->
	Nlist = case dict:find (From, Dict) of 
				error -> [{Ts, To}];
				{ok, L} when is_list(L) -> [{Ts, To} | L]
			end,	
	analyze_messages_loop(Parent, dict:store(From, Nlist, Dict), Filename, Sizes).

generate_graph (WeightedEdges) ->
	MaxWeight = find_max_weight(WeightedEdges),
	I = (MaxWeight - 1) / ?NO_COLORS ,
	add_weighted_edges(digraph("Messages", ["node [shape=point];"]), WeightedEdges, I).

find_max_weight(WeightedEdges) ->
	find_max_weight(WeightedEdges, -1).
find_max_weight([], Max) ->
	Max;
find_max_weight([{_, Tos} | T], MaxP) ->
	MaxWeight = lists:foldl(
	  fun ({_,Weight}, Max) ->
	    max(Max, Weight)
      end,
      MaxP,
	  Tos),
	find_max_weight(T, MaxWeight).

add_weighted_edges(Graph, [], _I) ->
	Graph;
add_weighted_edges(Graph, [{From, Tos} | T], I) ->
	NGraph = lists:foldl(
	  fun ({Vertex, Weight}, G) ->
		add_edge(G, From, Vertex, Weight,
				 [{weight, Weight}, {colorscheme, ?COLOR_SCHEME}, {color, ceiling(max(Weight - 1, 1)/I)}])  
	  end, 
	  Graph, 
	  Tos),
	add_weighted_edges (NGraph,  T, I).
	
get_edge_weights(Dict) ->
	[get_vertex_edge_weights (Elem) || Elem <- dict:to_list(Dict)]. 
	
get_vertex_edge_weights ({From, Tos}) when is_list(Tos) ->
	Fun = fun ({_Ts, Vertex}, Dict) ->
		N = case dict:find(Vertex, Dict) of 
			{ok, C} -> C;
			error -> 0				
		end,
		dict:store(Vertex, N + 1, Dict)
	end,
	Count = lists:foldl(Fun, dict:new(), Tos),
	{From, dict:to_list(Count)}.


flatten_edges_tuple ({From, Tos}) ->
	flatten_edges_tuple(From, Tos, []).

flatten_edges_tuple (_From, [], Acc) ->
	Acc;
flatten_edges_tuple (From, [{Ts, To} | T], Acc) ->
	flatten_edges_tuple(From, T, [{time_to_number(Ts), From, To} | Acc]).

flatten_and_sort_edges_dict (Dict) ->
	TsFromTo = lists:flatten([flatten_edges_tuple(X) || X <- dict:to_list(Dict)]),
	lists:sort(
		fun({A,_,_}, {B,_,_}) ->
			A < B
		end,
		TsFromTo).

dump_all_messages (Filename, Edges) ->
	Dest = Filename ++ ".messages",
	{ok, File} = file:open(Dest, [read, write]),	
	[io:fwrite(File, "~p\t~p\t~p~n", tuple_to_list(Each)) || Each <- Edges],
	file:close(File).

generate_intermediate_graphs(Dict, Filename) ->
	Edges = flatten_and_sort_edges_dict(Dict),
	dump_all_messages(Filename, Edges),
	[{Time, _, _}|_] = Edges, 
	generate_intermediate_dots([], Edges, Time, Filename, 1),
	generate_animation_py(Edges, Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_animation_py (Edges, Filename) ->
	Dest = Filename ++ "_anim.py",
	[{Start, _, _}|_] = Edges, 
	{ok, File} = file:open(Dest, [read, write]),
	io:fwrite(File, "import xmlrpclib~n", []),
	io:fwrite(File, "import time~n", []),
	io:fwrite(File, "server_url = 'http://127.0.0.1:20738/RPC2'~n", []),
	io:fwrite(File, "server = xmlrpclib.Server(server_url)~n", []),
	io:fwrite(File, "G = server.ubigraph;~n", []),
	io:fwrite(File, "G.clear();~n", []),
	
 	io:fwrite(File, "G.set_vertex_style_attribute(0, \"color\", \"#12AF31\")~n", []),
	io:fwrite(File, "G.set_vertex_style_attribute(0, \"shape\", \"sphere\")~n", []),
	io:fwrite(File, "G.set_vertex_style_attribute(0, \"size\", \"0.5\")~n", []),
	
	generate_animation_py (dict:new(), dict:new(), Edges, Start, File, 1),																  
	file:close(File).


generate_animation_py (DictVertex, DictEdges, Edges, Start, File, Seq) ->
%%	io:format("Xisto no espaco ~p, ~p~n", [length(Edges), dict:size(DictEdges)]),
	case length(Edges) == 0 andalso dict:size(DictEdges) == 0 of 
		true ->
			ok;			
		false ->
			io:fwrite(File, "time.sleep(0.5)~n", []),
%% 			io:format("A1~n"),
			{FilteredDictVertex, FilteredDictEdges} = filter_dict_edges (DictVertex, DictEdges, File),
%% 			io:format("A2~n"),
			End = Start + ?ANIMATION_WINDOW,
%% 			io:format("A3~n"),
			{NewEdges, TEdges} = lists:splitwith(fun ({Ts,_,_})-> Ts < End end, Edges),
%% 			io:format("A4~n"),
			{NDV, NDE, NSeq} = calculate_animation_new_current_edges (FilteredDictVertex, FilteredDictEdges, NewEdges, Seq, File),
%% 			io:format("A5 ~p~n", [NSeq]),
			generate_animation_py (NDV, NDE, TEdges, End, File, NSeq)
	end.


calculate_animation_new_current_edges (DictVertex, DictEdges, NewEdges, Sequence, File) ->
	NewEdges2 = [{?NO_COLORS, Ts, From, To} || {Ts, From, To} <- NewEdges],
	lists:foldl(
		fun({Color, Ts, From, To}, {DV, DE, Seq}) ->
	  		%Adding vertices
			%Seq + 0
			DV2 = case dict:find(From, DV) of
				{ok, {IDv1, Fs1, Ts1}} ->
					dict:store(From, {IDv1, sets:add_element(To, Fs1), Ts1}, DV);			
				error -> %new vertex
					animation_add_vertex(Seq, File),					 
					dict:store(From, {Seq, sets:from_list([To]), sets:new()}, DV)
			end,
			%Seq + 1
	  		DV3 = case dict:find(To, DV2) of
				{ok, {IdV2, Fs2, Ts2}} ->
					dict:store(To, {IdV2, Fs2, sets:add_element(From, Ts2)}, DV2);		
				error -> %new vertex
					animation_add_vertex(Seq + 1, File),
					dict:store(To, {Seq + 1, sets:new(), sets:from_list([From])}, DV2)
			end,

			%Adding edge
			%Seq + 2
			%if there is already an edge, we just drop it
			FoundEdge = dict:to_list(dict:filter(
			  fun (_, {_, _, F1, T1}) ->
			  	From == F1 andalso To == T1
			  end,
			  DE)),
			DE2 = case length(FoundEdge) > 0 of
				true ->
					%io:format("Bumping edge!!"),
					{E3, {_, Ts3, F3, T3}} = hd(FoundEdge),  
					dict:store(E3, {?NO_COLORS, Ts3, F3, T3}, DE); 
				false ->
					animation_add_edge (Seq + 2, Color, From, To, DV3, File),
					dict:store(Seq + 2, {Color, Ts, From, To}, DE)
				end,	
			{DV3, DE2, Seq + 3}
		end,	
	  	{DictVertex, DictEdges, Sequence},
	  	NewEdges2).

animation_add_command(File, Command, Subs) ->
	io:fwrite(File, Command, Subs),
	io:fwrite(File, "time.sleep(0.01)~n", []).

animation_add_vertex(VId, File) ->
	animation_add_command(File, "G.new_vertex_w_id(~p)~n", [VId]).

animation_add_edge (EdgeId, _Color, From, To, DV, File) ->
	{FId, _, _ } = dict:fetch(From, DV),
	{TId, _, _ } = dict:fetch(To, DV),
	animation_add_command(File, "G.new_edge_w_id(~p, ~p, ~p)~n", [EdgeId, FId, TId]),
 	animation_add_command(File, "G.set_edge_attribute(~p, \"arrow\", \"true\")~n", [EdgeId]).
	
animation_remove_edge(EdgeId, File) ->
	animation_add_command(File, "G.remove_edge(~p)~n", [EdgeId]).

animation_remove_vertex(VertexId, File) ->
	animation_add_command(File, "G.remove_vertex(~p)~n", [VertexId]).

animation_change_edge_color (EdgeId, NewColor, File) ->
	ColorMap = [
		"#111111",
		"#444444",
		"#666666",
		"#888888",
		"#AAAAAA",
		"#BBBBBB",
		"#CCCCCC",
		"#EEEEEE",
		"#FFFFFF"],
	animation_add_command(File, "G.set_edge_attribute(~p, \"color\", ~p)~n", [EdgeId, lists:nth(NewColor, ColorMap)]).

remove_edge (EId, EDict, File) ->
	  	animation_remove_edge(EId, File),
		dict:erase(EId, EDict).

treat_edge_removal_from(From, To, DV, File) ->
	{Vid, Outs, Ins} = dict:fetch(From, DV),
	case sets:size(Outs) =< 0 andalso sets:size(Ins) =< 0 of
		true -> io:format("BUEMBA~n");
		false -> ok
	end,
	NOuts = sets:del_element(To, Outs),	
	case sets:size(NOuts) == 0 andalso sets:size(Ins) == 0 of
		true ->
			animation_remove_vertex(Vid, File),
			dict:erase(From, DV);
		false ->
			dict:store(From, {Vid, NOuts, Ins}, DV)
	end.

treat_edge_removal_to(From, To, DV, File) ->
	{Vid, Outs, Ins} = dict:fetch(To, DV),
	case sets:size(Outs) =< 0 andalso sets:size(Ins) =< 0 of
		true -> io:format("BUEMBA~n");
		false -> ok
	end,
	NIns = sets:del_element(From, Ins),
	case sets:size(Outs) == 0 andalso sets:size(NIns) == 0 of
		true ->
			animation_remove_vertex(Vid, File),
			dict:erase(To, DV);
		false ->
			dict:store(To, {Vid, Outs, NIns}, DV)
	end.

treat_edge_removal(From, To, DV, File) ->
	DV2 = treat_edge_removal_from(From, To, DV, File),
	treat_edge_removal_to(From, To, DV2, File).
	
update_colors(DictE, File) ->	
	lists:foldl(
		fun ({EId, {Color, Ts, From, To}}, DE) ->
			animation_change_edge_color(EId, Color - 1, File),
			dict:store(EId, {Color - 1, Ts, From, To}, DE)
		end,
		DictE,
		dict:to_list(DictE)).
	
	
filter_dict_edges (DictVertex, DictEdges, File) ->
%%	Edges = [dict:to_list(DictEdges)],
%% 	io:format(">>>>>>>>>>> ~p ~n", [Edges]),
	%lists:filter(fun({_,{Color, _. _, _}})-> Color == 1 , List1) 
	RemovedEdges = 	[X || X = {_, {Color, _, _, _}} <- dict:to_list(DictEdges), Color == 1],
	{NDV, NDE} = lists:foldl(
	  fun ({EId, {_, _, From, To}}, {DV, DE}) ->
		DE2 = remove_edge(EId, DE, File),
%% 		case EId == 36 orelse  EId == 3 orelse EId == 15 of
%% 			true ->
%% 				io:format("~p F: ~p (~p) To: ~p (~p)\n", [EId, From, dict:fetch(From, DV), To, dict:fetch(To, DV)]);
%% 			false -> ok
%% 		end,
		DV2 = treat_edge_removal(From, To, DV, File),
		{DV2, DE2}
	  end,
	  {DictVertex, DictEdges},
	  RemovedEdges),
	{NDV, update_colors(NDE, File)}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


generate_intermediate_dots ([], [], _, _, _) ->
	ok;
generate_intermediate_dots (CurrentEdges, Edges, Start, Filename, Seq) ->
	FilteredCurrentEdges = filter_current_edges (CurrentEdges),
	End = Start + ?ANIMATION_WINDOW,
	{NewEdges, TEdges} = lists:splitwith(fun ({Ts,_,_})-> Ts < End end, Edges),
	CurrentEdges2 = calculate_new_current_edges (FilteredCurrentEdges, NewEdges),	
	Pid = spawn_and_phone_home (fun () ->
			StrSeq = lists:flatten(io_lib:format("~5..0B", [Seq])),
			Graph = add_intermediate_edges (digraph(StrSeq, ["node [shape=point];"]), CurrentEdges2),
			to_dot(Graph, Filename ++".messages." ++ StrSeq ++ ".dot")
		end),	
	Ret = generate_intermediate_dots (CurrentEdges2, TEdges, End, Filename, Seq + 1),
	receive_times(Pid),
	Ret.

filter_current_edges (List) ->
	[{Color - 1, Ts, From, To} || {Color, Ts, From, To} <- List, Color > 1].	

calculate_new_current_edges (CurrentEdges, NewEdges) ->
	NewEdges2 = [{?NO_COLORS, Ts, From, To} || {Ts, From, To} <- NewEdges],
	SNE = lists:sort(
	  	fun ({_, _, From1, To1}, {_, _, From2, To2}) ->
				 From1 < From2 orelse ((From1 == From2) andalso (To1 < To2))
		end,
		CurrentEdges ++ NewEdges2),
	compress_new_edges(SNE).

compress_new_edges(List) ->
	compress_new_edges(List, []).

compress_new_edges([], Acc) ->
	Acc;
compress_new_edges([H|T], []) ->
	compress_new_edges (T, [H]);
compress_new_edges([{C1, _, From, To} = H1 | Rest], [{C2, _, From, To} = H2 | RestAcc]) ->
	H = case C1 > C2 of
		true -> H1;		
		false -> H2
	end,
	compress_new_edges (Rest, [H | RestAcc]);
compress_new_edges([H|T], Acc) ->
	compress_new_edges (T, [H | Acc]). 
	

add_intermediate_edges (Graph, []) ->
	Graph;
add_intermediate_edges (Graph, [{Color, _When, From, To}| TEdges]) ->
	Graph2 = add_edge(Graph, From, To, Color,
				 [{weight, Color}, {colorscheme, ?COLOR_SCHEME}, {color, Color}]),
	add_intermediate_edges (Graph2, TEdges).


%%%%%%%%%%%%%%%%
%%% DURATION %%% 
%%%%%%%%%%%%%%%%

analyze_duration (Parent, Filename, Start, End) ->
	Dest = Filename ++ ".duration",
	{ok, File} = file:open(Dest, [read, write]),	
	analyze_duration_loop(Parent, File, dict:new(), Start, End).

analyze_duration_loop(Parent, File, Dict, Start, End) ->
	receive
		{spawn, Pid, Ppid, When} ->
			{Dict2, Pinfo} = procInfo(Pid, Dict),
			Pinfo2 = Pinfo#proc_info{start_ts=time_to_number(When), ppid = Ppid},
			Dict3 = duration_flush_pinfo_if_possible(Pinfo2, Dict2, File),
			analyze_duration_loop (Parent, File, Dict3, Start, End);
		{exit, Pid, When} ->
			{Dict2, Pinfo} = procInfo(Pid, Dict),
			Pinfo2 = Pinfo#proc_info{end_ts=time_to_number(When), exited=true},
			Dict3 = duration_flush_pinfo_if_possible(Pinfo2, Dict2, File),
			analyze_duration_loop (Parent, File, Dict3, Start, End);
		{out_exited, Pid, When} ->
			{Dict2, Pinfo} = procInfo(Pid, Dict),
			Pinfo2 = Pinfo#proc_info{end_ts=time_to_number(When), out_exited=true},
			Dict3 = duration_flush_pinfo_if_possible(Pinfo2, Dict2, File),
			analyze_duration_loop (Parent, File, Dict3, Start, End);
		{end_of_trace} ->
			duration_flush_all_pinfos (Dict, File, Start, End),
			file:close(File),
			io:format("Duration: ok~n");
		_Other -> 
			analyze_duration_loop(Parent, File, Dict, Start, End)
	end.

duration_flush_pinfo_if_possible (Pinfo, Dict, File) ->
	case pinfo_duration_complete (Pinfo) of
		true ->
			write_proc_info (Pinfo, File),
			dict:erase(Pinfo#proc_info.pid, Dict);
		false ->
			dict:store(Pinfo#proc_info.pid, Pinfo, Dict)
	end.

duration_flush_all_pinfos (Dict, File, Start, End) ->
	List = dict:to_list(Dict),
	[write_proc_info(correct_undefined_ts(PInfo, Start, End), File) || {_, PInfo} <- List].	

correct_undefined_ts(ProcInfo, Start, End) ->
	NProcInfo = correct_undefined_start_ts (ProcInfo, Start),
	correct_undefined_end_ts (NProcInfo, End).

correct_undefined_start_ts(PInfo = #proc_info{start_ts = undef}, Start) ->
	PInfo#proc_info{start_ts = Start};
correct_undefined_start_ts(PInfo, _Start) ->
	PInfo.

correct_undefined_end_ts(PInfo = #proc_info{end_ts = undef}, End) ->
	PInfo#proc_info{end_ts = End};
correct_undefined_end_ts(PInfo, _End) ->
	PInfo.

procInfo(Pid, Dict) ->
	case dict:find(Pid, Dict) of
		{ok, Value} -> {Dict, Value};
		error -> 
			NProc = #proc_info{pid=Pid},
			{dict:store(Pid, NProc, Dict), NProc}
	end.

pinfo_duration_complete (#proc_info{start_ts=undef}) ->
	false;
pinfo_duration_complete (#proc_info{end_ts=undef}) ->
	false;
pinfo_duration_complete (Pinfo) ->
	Pinfo#proc_info.exited andalso Pinfo#proc_info.out_exited.

write_proc_info (Pinfo, File) ->
	Pid = Pinfo#proc_info.pid,
	Start = Pinfo#proc_info.start_ts,
	End = Pinfo#proc_info.end_ts,
	Dur = End - Start,
	io:fwrite(File, "~p\t~p\t~p\t~p~n", [Pid, Start, End, Dur]).


%%%%%%%%%%%%%%%%%%%
%%% ACTOR COUNT %%% 
%%%%%%%%%%%%%%%%%%%

analyze_actor_count(Parent, Filename, PCount) ->
	OutputFileName = Filename ++ ".count",
	{ok, File} = file:open(OutputFileName, [read, write]),
	analyze_actor_count_loop(Parent, File, PCount, []).


analyze_actor_count_loop(Parent, File, Pcount, List) ->
	receive
		{spawn, _, _, When} ->
			analyze_actor_count_loop(Parent, File, Pcount, [{time_to_number(When), 1} | List]);
		{out_exited, _, When} ->
			analyze_actor_count_loop(Parent, File, Pcount, [{time_to_number(When), -1} | List]);
		{end_of_trace} ->
			io:format("Actor Count: ok~n"),
			actor_count_flush_all (File, Pcount, List),
			file:close(File);
		_Other -> 
			analyze_actor_count_loop(Parent, File, Pcount, List)
	end.

actor_count_flush_all(File, PCount, List) ->
	SList = lists:sort(fun({A, _}, {B, _}) -> A < B end, List),
	lists:foldl(
	  fun ({TS, Delta}, Count) -> 
			   NVal = Count + Delta,
			   io:fwrite(File, "~p\t~p~n", [TS, NVal]),
			   NVal
	  end, 
	  PCount, 
	  SList).


%%%%%%%%%%%%%%%%%%%%%%
%%% EXECUTION TIME %%% 
%%%%%%%%%%%%%%%%%%%%%%

analyze_execution_time(Parent, Filename) ->	
	Dest = Filename ++ ".sched",
	{ok, File} = file:open(Dest, [read, write]),	
	analyze_execution_time_loop(Parent, File, dict:new()).


analyze_execution_time_loop(Parent, File, Dict) ->
	receive		
		{in, Pid, When} ->
			{Dict2, Pinfo} = procInfo(Pid, Dict),
			Changes = Pinfo#proc_info.context_switches,
			Ins = Pinfo#proc_info.sched_ins,
			Dict3 = dict:store(Pid, Pinfo#proc_info{
													context_switches=Changes + 1, 
													sched_ins = [time_to_number(When) | Ins]
												   }, Dict2),
			analyze_execution_time_loop(Parent, File, Dict3);
		{out, Pid, When} ->
			{Dict2, Pinfo} = procInfo(Pid, Dict),
			Outs = Pinfo#proc_info.sched_outs,
			Dict3 = dict:store(Pid, Pinfo#proc_info{sched_outs=[time_to_number(When) | Outs]}, Dict2),
			analyze_execution_time_loop(Parent, File, Dict3);
		{out_exited, Pid, When} ->
			{Dict2, Pinfo} = procInfo(Pid, Dict),
			case Pinfo#proc_info.context_switches of
				0 ->
					analyze_execution_time_loop(Parent, File, Dict);
				_ ->
					Outs = Pinfo#proc_info.sched_outs,
					Dict3 = dict:store(Pid, Pinfo#proc_info{sched_outs=[time_to_number(When) | Outs]}, Dict2),
					analyze_execution_time_loop(Parent, File, Dict3)
			end;
		{end_of_trace} ->
			io:format("Execution time 1/2: ok~n"),
			List = dict:to_list(Dict),
			execution_time_calculate_and_write(File, List),			
			file:close(File),
			io:format("Execution time 2/2: ok~n");		
		_Other -> 
			analyze_execution_time_loop(Parent, File, Dict)
	end.


execution_time_calculate_and_write(File, List) ->
	WriteProc = spawn_link(fun () -> execution_time_write_loop (File) end),
	Size = length(List),
	Schedulers = erlang:system_info(schedulers),
	PartSize = Size div Schedulers, 
	SpawnedCount = execution_time_spawn_and_calculate (WriteProc, List, PartSize, Size, 0),
	WriteProc ! {wait_for, self(), SpawnedCount},
	receive count_ended -> ok end.


execution_time_spawn_and_calculate (_WriteProc, [], _PartSize, 0, Count) ->
	Count;
execution_time_spawn_and_calculate (WriteProc, List, PartSize, Remaining, Count) when Remaining > 0 ->
	L1Size = min(Remaining, PartSize),
	{List1, List2} = lists:split(L1Size, List),
	spawn_link (fun() -> execution_time_calculate_all (WriteProc, List1) end),
	execution_time_spawn_and_calculate (WriteProc, List2, PartSize, Remaining - L1Size, Count + 1).	 


execution_time_calculate_all (WriteProc, L) when is_list(L) ->
	[execution_time_calculate(WriteProc, Each) || Each <- L],
	WriteProc ! spawn_finished,
	ok.

%% execution_time_calculate (_WriteProc, Pinfo) when is_record(Pinfo, proc_info) ->
%% 	% This process has never scheduled out and stayed all 
%% 	% the time running, or it has never run. Either way
%% 	% we ignore it
%% 	ok;
execution_time_calculate (WriteProc, {Pid, Pinfo}) when is_record(Pinfo, proc_info) ->
	Changes = Pinfo#proc_info.context_switches,
	[HIns |_] = SIns = lists:sort(Pinfo#proc_info.sched_ins),
	SOuts = lists:dropwhile(fun (O) -> O < HIns end, lists:sort(Pinfo#proc_info.sched_outs)),
	execution_time_calculate (WriteProc, Pid, Changes, SIns, SOuts).

execution_time_calculate (WriteProc, Pid, Changes, SIns, SOuts) ->
	TotalTime = execution_time_calculate_intervals (SIns, SOuts),
	WriteProc ! {Pid, Changes, TotalTime}.

execution_time_calculate_intervals (SIns, SOuts) ->
	execution_time_calculate_intervals (SIns, SOuts, 0).
execution_time_calculate_intervals ([], [], Total) ->
	Total;
execution_time_calculate_intervals (SIns, [], Total)  when length(SIns) < 2 -> %more than 2 might indicate some problem
	Total;
execution_time_calculate_intervals ([HIn|TIn], [HOut|TOut], Total) when HIn < HOut ->
	execution_time_calculate_intervals (TIn, TOut, Total + HOut - HIn).


execution_time_write_loop (File) ->
	receive
		{wait_for, EndProc, Spawns} ->
			execution_time_write_loop (File, EndProc, Spawns)
	end.
execution_time_write_loop(_File, EndProc, 0) ->
	EndProc ! count_ended;
execution_time_write_loop(File, EndProc, Spawns) ->
	receive
		{Pid, Changes, TotalTime} ->
			io:fwrite(File, "~p\t~p\t~p~n", [Pid, Changes, TotalTime]),
			execution_time_write_loop(File, EndProc, Spawns);
		spawn_finished ->
			execution_time_write_loop (File, EndProc, Spawns - 1)
	end.


generate_derived_files (Filename, StartTs, EndTs) ->
	generate_duration_proc_time (Filename, integer_to_list(EndTs - StartTs)),
	Pid1 = spawn_and_phone_home ( 
		fun () -> generate_plots(Filename, StartTs) end),
	Pid2 = spawn_and_phone_home (
		fun () -> generate_graphs (Filename) end),
	Pid3 = spawn_and_phone_home (
		fun () -> generate_movie (Filename) end),
	receive_times([Pid1, Pid2, Pid3]).


%%%%%%%%%%%%%%%%%%%%%
%%% Derived Files %%% 
%%%%%%%%%%%%%%%%%%%%%
generate_duration_proc_time (Filename, DefDur) ->
	
	SortFun = fun ({A, _}, {B, _}) -> A < B end,	
	
	{ok, Dur} = file:open(Filename ++ ".duration", [read]),
	D1 = file_map (Dur, [], 
				   fun (Line) ->
							[Pid, _, _, Duration] = string:tokens(Line, "\t\n"),
							{Pid, Duration}
				   end),	
	D2 = lists:sort(SortFun, D1),
	
	{ok, Sched} = file:open(Filename ++ ".sched", [read]),
	P1 = file_map (Sched, [], 
				   fun (Line) ->
							[Pid, _, ProcTs] = string:tokens(Line, "\t\n"),
							{Pid, ProcTs}
				   end),
	P2 = lists:sort(SortFun, P1),
	
	%% If no duration was recorded, then it existed before de profiling began and ended after the profile ended
	%% therefore its duration is at least as big as DefDur
	%% If there is no scheduling recorded, then during the profile it has not executed therefore used 0 
	J = join_lists (D2, DefDur, P2, 0), 
	
	{ok, JFile} = file:open(Filename ++ ".duration_vs_proc", [read, write]),
	[io:fwrite(JFile, "~s\t~s\t~s~n", [P, D, Pt]) || {P, D, Pt} <- J],
	file:close(JFile).


file_map(File, Result, F) ->
	case io:get_line(File, "") of
		eof  -> file:close(File), Result;
		Line ->
			Elem = F(Line),
			file_map(File, [Elem | Result], F)
	end.


%% Assumes L1 and L2 are in order
join_lists (L1, DefVal1, L2, DefVal2) ->
	join_lists (L1, DefVal1, L2, DefVal2, []).

join_lists ([],_, [], _, Acc) ->
	Acc;
join_lists ([], DefVal1, [{Key2, Val2} | T2], DefVal2, Acc) ->
	join_lists ([], DefVal1, T2, DefVal2, [{Key2, DefVal1, Val2} | Acc]);
join_lists ([{Key1, Val1} | T1], DefVal1, [], DefVal2, Acc) ->
	join_lists (T1, DefVal1, [], DefVal2, [{Key1, Val1, DefVal2} | Acc]);
join_lists ([{Key, Val1} | T1], DefVal1, [{Key, Val2} | T2], DefVal2, Acc) -> % Both keys are the same
	join_lists (T1, DefVal1, T2, DefVal2, [{Key, Val1, Val2} |Acc]);
join_lists ([{Key1, Val1} | T1], DefVal1, L2 = [{Key2, _} | _], DefVal2, Acc) when Key1 < Key2 -> 
	join_lists (T1, DefVal1, L2, DefVal2, [{Key1, Val1, DefVal2} | Acc]);
join_lists (L1 = [{Key1, _} | _], DefVal1, [{Key2, Val2} | T2], DefVal2, Acc) when Key1 > Key2 -> 
	join_lists (L1, DefVal1, T2, DefVal2, [{Key2, DefVal1, Val2} | Acc]).




%%%%%%%%%%%%%
%%% Plots %%% 
%%%%%%%%%%%%%

generate_plots(Filename, Start) ->
	{ok, File} = gnuplot_create_file(Filename ++ ".gp"),
	generate_duration_plot (File, Filename, Start),
	generate_actor_count_plot (File, Filename, Start),
	generate_duration_proc_time_plot (File, Filename),
	generate_active_time_percentage_plot(File, Filename),
	generate_message_plots(File, Filename),
	gnuplot_close(File),
	gnuplot_run(Filename ++ ".gp").

generate_duration_plot (File, Filename, Start) ->
	gnuplot_set_output_png (File, Filename ++ ".duration.png"),
	gnuplot_set_title(File, "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB"),
	gnuplot_set_xlabel(File, "Application Execution Time (seconds)"),
	gnuplot_set_ylabel(File, "Process lifespan (seconds) - Log Scale"),
	gnuplot_unset_log_x(File),
	gnuplot_set_log_y(File),
	gnuplot_plot(File, Filename ++ ".duration", 
				 "(($2 - " ++ integer_to_list(Start) ++ ".0)/1000000.0):($4/1000000.0)").

generate_actor_count_plot(File, Filename, Start) ->
	gnuplot_set_output_png (File, Filename ++ ".count.png"),
	gnuplot_set_title(File, "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB"),
	gnuplot_set_xlabel(File, "Application Execution Time (seconds)"),
	gnuplot_set_ylabel(File, "Number of actors"),
	gnuplot_unset_log_x(File),
	gnuplot_unset_log_y(File),
	gnuplot_plot(File, Filename ++ ".count", 
				 "(($1 - " ++ integer_to_list(Start) ++ ".0)/1000000.0):2",
				 "l",
				 "").

generate_duration_proc_time_plot (File, Filename) ->
	gnuplot_set_output_png (File, Filename ++ ".duration_vs_proc.png"),
	gnuplot_set_title(File, "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB"),
	gnuplot_set_xlabel(File, "Process lifespan (seconds)"),
	gnuplot_set_ylabel(File, "Processing time (seconds)"),
	gnuplot_set_log_x(File),
	gnuplot_set_log_y(File),	
	gnuplot_plot(File, Filename ++ ".duration_vs_proc", 
				 "($2/1000000):($3/1000000)", "p",
				 ",x title \"100%\", x/4 title \"25%\", x/20 title \"5%\", x/100 title \"1%\"").

generate_active_time_percentage_plot (File, Filename) ->	
	generate_histogram (
	  File, Filename ++ ".active_time.png", Filename ++ ".duration_vs_proc", 
	  "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB", 
	  "Activity Percentage - Running time/Lifespan Time (%)", "Number of Actors", 
	  0, 100, 5, "0.5", 
	  "$3/$2*100").

generate_message_plots (File, Filename) ->
	generate_message_plots (File, Filename, "Total", "$2"),
	generate_message_plots (File, Filename, "Distinct_Nodes", "$3"),
	generate_message_total_vs_distinct(File, Filename),
	generate_message_sizes(File, Filename).

generate_message_sizes(File, Filename) ->
	Suf = ".messages.size",	
	gnuplot_set_output_png (File, Filename ++ Suf ++ ".png"),
	gnuplot_set_title(File, "YCSB x CouchDB - Message size"),
	gnuplot_set_xlabel(File, "Message size (bytes)"),
	gnuplot_set_ylabel(File, "Number of messages"),
	gnuplot_set_log_x(File),
	gnuplot_unset_log_y(File),
	gnuplot_set_xrange(File, "8", "32768"),
	gnuplot_set_xtics(File, "2"),
	gnuplot_plot(File, Filename ++ Suf,
				 "($1):(1.0)"
					 " smooth freq", "lines", "").
	


generate_message_total_vs_distinct(File, Filename) ->	
	gnuplot_set_output_png (File, Filename ++ ".messages.count.TotalVsDistinct.png"),
	gnuplot_set_title(File, "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB"),
	gnuplot_set_xlabel(File, "Total Messages"),
	gnuplot_set_ylabel(File, "Distinct Actors"),
	gnuplot_set_xrange(File, "*", "*"),
	gnuplot_set_log_x(File),
	gnuplot_set_log_y(File),	
	gnuplot_plot(File, Filename ++ ".messages.count", 
				 "($2):($3)", "p",
				 ",x title \"100%\", x/4 title \"25%\", x/20 title \"5%\", x/100 title \"1%\"").
	
generate_message_plots (File, Filename, Suffix, Field) ->
	generate_histogram (
	  File, Filename ++ ".messages.count.0_10000." ++ Suffix ++".png", Filename ++ ".messages.count", 
	  "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB", 
	  "Number of sent messages", "Number of Actors", 
	  0, 10000, 1000, "50", 
	  Field),
	generate_histogram (
	  File, Filename ++ ".messages.count.0_1000." ++ Suffix ++ ".png", Filename ++ ".messages.count", 
	  "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB", 
	  "Number of sent messages", "Number of Actors", 
	  0, 1000, 100, "10", 
	  Field),
	generate_histogram (
	  File, Filename ++ ".messages.count.0_200." ++ Suffix ++ ".png", Filename ++ ".messages.count", 
	  "Yahoo! Cloud Serving Benchmark (YCSB) vs. CouchDB", 
	  "Number of sent messages", "Number of Actors", 
	  0, 100, 5, "1", 
	  Field).

generate_histogram (File, OutputFileName, InputFileName, Title, Xlabel, Ylabel, Xrangemin, Xrangemax, Ticks, BinWidth, Column) ->
	gnuplot_set_output_png (File, OutputFileName),
	gnuplot_set_title(File, Title),
	gnuplot_set_xlabel(File, Xlabel),
	gnuplot_set_ylabel(File, Ylabel),
	gnuplot_unset_log_x(File),
	gnuplot_unset_log_y(File),
	gnuplot_set_xrange(File, Xrangemin, Xrangemax),
	gnuplot_set_style(File, "histogram clustered gap 1"),
	gnuplot_set_style(File, "fill solid border -1"),
	gnuplot_set_xtics(File, Ticks),
	gnuplot_variable(File, "binwidth", BinWidth),
	gnuplot_set_boxwidth(File, "binwidth"),
	gnuplot_variable(File, "bin(x,width)", "width*floor(x/width) + binwidth/2.0"),
	gnuplot_plot(File, InputFileName,
				 "(bin((" ++ Column ++ "),binwidth)):(1.0)"
					 " smooth freq", "boxes",
				 "").


%%%%%%%%%%%%%%
%%% Graphs %%% 
%%%%%%%%%%%%%%

generate_graphs (Filename) ->
	to_svg(Filename ++".messages.dot").

generate_movie (Filename) ->
	Dir = filename:dirname(Filename),
	Basename = filename:basename(Filename),
	Files = filelib:wildcard(Basename ++ ".messages.?????.dot", Dir),
	Pids = [spawn_and_phone_home(fun()-> to_gif(Dir ++ "/" ++ F) end) || F <- Files],
	receive_times(Pids).
	
	