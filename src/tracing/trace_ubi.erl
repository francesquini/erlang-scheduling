-module(trace_ubi, [Inst]).

-export ([load_file/1, draw/0, draw_events/0, clear/0, stop/0]).

%%Local exports
-export ([draw_edge/2, process_file/1]).

-record(
   gd, 
   	{
		vertices = dict:new(),
		edges = dict:new(),
		actors_birth = dict:new(),
		actors_lifespan = dict:new(),
		actors_parent = dict:new(),
		events=[]
  	}
).

load_file(Filename) ->
	{ok, InFile} = file:open(Filename, [read]),
	ok = process_file(InFile),
	ok = file:close(InFile),
	initialize().	

draw () ->
	Inst ! draw.

clear () ->
	Inst ! clear.

stop () ->
	Inst ! stop.

draw_events() ->
	Inst ! draw_events.

%% -----------------------
%% -----------------------

initialize () ->
	erlubi:start(),
	erlubi:clear(),
	State = #gd{},
	loop(State).

loop (State) ->
	receive
		stop -> ok;
		{add_vertex, Info} -> loop (add_vertex(State, Info));
		{remove_vertex, Info} -> loop (remove_vertex(State, Info));
		{create_edge, Info} -> loop (create_edge(State, Info));
		clear -> loop(do_clear(State));
		draw -> loop(do_draw(State));
		draw_events -> loop(do_draw_events(State))
	end.


process_file(InFile) ->
	case file:read_line(InFile) of
		{ok, Line} ->
			Event = string:tokens(Line, "\t "),
			process_event (Event),						
			process_file(InFile);
		 eof -> 
			ok
	end.

process_event (["p" | Info]) ->
	Inst ! {add_vertex, Info};  
process_event (["s" | Info]) ->
	Inst ! {create_edge, Info};  
process_event (["e" | Info]) ->
	Inst ! {remove_vertex, Info};  
process_event (X) ->
	io:format("Unknown format received ~p~n", [X]).

add_vertex (State, [V_Name, V_Parent, When]) ->
	WhenInt = utils:to_int(When),
	NewBirth = dict:store(V_Name, WhenInt, State#gd.actors_birth),
	NewParent = dict:store(V_Name, V_Parent, State#gd.actors_parent),
	NewEvents = [{spawn, V_Name, V_Parent, WhenInt} | State#gd.events],
	State#gd{
		actors_birth=NewBirth,
		actors_parent=NewParent,
		events=NewEvents}.

remove_vertex (State, [Name, When]) ->
	Birth = dict:fetch(Name, State#gd.actors_birth),
	WhenInt = utils:to_int(When), 
	NewLS = WhenInt - Birth,
	NewLsDict = dict:store(Name, NewLS, State#gd.actors_lifespan),
	NewEvents = [{exit, Name, WhenInt} | State#gd.events],
	State#gd{
		actors_lifespan = NewLsDict,
		events=NewEvents}.

create_edge (State, [Sender, Receiver, Size, When]) ->
	WhenInt =  utils:to_int(When), 
	MsgSize = max(utils:to_int(Size), 1),
	Key = {Sender,Receiver}, 
	OldValue = case dict:is_key(Key, State#gd.edges) of
		true -> dict:fetch(Key, State#gd.edges);
		false -> 0
	end,
	NewEvents = [{send, Sender, Receiver, MsgSize, WhenInt} | State#gd.events],
	State#gd{
		edges=dict:store(Key, OldValue + MsgSize, State#gd.edges),
		events=NewEvents}.

do_draw (State) ->
	Edges = dict:fetch_keys(State#gd.edges),
	lists:foldl (fun draw_edge/2, State, Edges).

draw_edge ({From_V, To_V}, State) ->
	{Vertex1, State1} = get_vertex(From_V, State),
	{Vertex2, State2} = get_vertex(To_V, State1),
	{ok, _Edge} = erlubi:edge(Vertex1, Vertex2),
	State2.

do_draw_events(State) ->
	SortedEvents = lists:sort (fun compare_events/2, State#gd.events),
	lists:foldl (fun draw_event/2, State, SortedEvents).

draw_event({spawn, V_Name, _Parent, _When}, State) ->
	{_, State2} = get_vertex(V_Name, State),
	State2;
draw_event({send, From_V, To_V, _Size, _When}, State) ->
	draw_edge ({From_V, To_V}, State);	
draw_event({exit, Name, _When}, State) ->
 	OldDict = State#gd.vertices,
 	Vertex = dict:fetch(Name, OldDict),
 	NewDict = dict:erase(Name, OldDict),
 	Vertex:remove(),
 	State#gd{vertices=NewDict}.
	
compare_events (Event1, Event2) when is_tuple(Event1) andalso is_tuple(Event2) ->
	event_when(Event1) < event_when (Event2).

event_when (Event) when is_tuple(Event) ->
	element(tuple_size(Event), Event).

get_vertex(V_Name, State) ->
	case dict:is_key(V_Name, State#gd.vertices) of
		true  ->
			Vertex = dict:fetch(V_Name, State#gd.vertices),
			{Vertex, State};
		false ->
   			{ok, Vertex} = erlubi:vertex(),
 			NewVDict = dict:store(V_Name, Vertex, State#gd.vertices),
			{Vertex, State#gd{vertices=NewVDict}}
		end.

do_clear (State) ->
	erlubi:clear(), 
	State#gd{vertices=dict:new()}.