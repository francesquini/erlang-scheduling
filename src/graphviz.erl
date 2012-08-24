-module(graphviz).

-export([graph/2, digraph/2, add_vertex/2, add_edge/5, to_dot/2, to_file/3, to_png/1, to_png/2, to_svg/1, to_svg/2, to_gif/1, to_gif/2]).

-include("graph.hrl").

-import(utils, [to_dbstring/1, to_string/1]).

graph(Id, Options) ->
	#graph{id=Id, type=graph, edge_type="--", options=Options}.
digraph(Id, Options) -> 
	#graph{id=Id, type=digraph, edge_type="->", options=Options}.


add_vertex(Graph, Id) ->
	Graph#graph{vertices=[Id | Graph#graph.vertices]}.

add_edge(Graph, VertexOne, VertexTwo, Weight, Options) ->
	NewEdge = #edge{
		source=to_dbstring(VertexOne), 
		dest=to_dbstring(VertexTwo),
		weight=Weight,
		options=Options}, 
	Graph#graph{edges=[NewEdge | Graph#graph.edges]}.


to_dot(Graph, Filename) ->
	{ok, File} = file:open(Filename, [write]),
	print_graph(File, Graph),
	print_vertices(File, Graph),   
	print_edges(File, Graph),
	io:format(File, "}~n", []),
	file:close(File).

to_file(Graph, DotFile, Format) ->
	to_dot(Graph, DotFile),
	run_dot(Format, DotFile).

run_dot (Format, DotFile) ->	
	DotCommant = lists:concat(["dot -Kneato -T", Format, " -O ", DotFile]),
	[] = os:cmd(DotCommant).

to_png(Graph, File) ->
	to_file(Graph, File, "png").

to_png(File) ->
	run_dot("png", File).

to_svg(Graph, File) ->
	to_file(Graph, File, "svg").

to_svg(File) ->
	run_dot("svg", File).

to_gif(Graph, File) ->
	to_file(Graph, File, "gif").

to_gif(File) ->
	run_dot("gif", File).

print_graph(File, Graph) ->
	io:format(File, "~s ~s {~n\t~s~n", [Graph#graph.type, Graph#graph.id, get_graph_options(Graph)]).

get_graph_options(Graph) ->
	string:join(Graph#graph.options, "~n").

print_vertices (File, Graph) ->
	[io:format(File, "\t~s;~n", [Vertex]) || Vertex <- Graph#graph.vertices].

print_edges (File, Graph) ->
	SEdges = lists:sort(fun (E1, E2) -> E1#edge.weight < E2#edge.weight end, Graph#graph.edges), 
	[print_edge (File, Graph, Edge) || Edge <- SEdges].

print_edge (File, Graph, Edge) ->
	io:format(File, "\t~s ~s ~s ~s;~n",[Edge#edge.source, Graph#graph.edge_type, Edge#edge.dest, get_edge_options(Edge)]).

get_edge_options (Edge) ->
	Ret = [to_string(Option) ++ "=" ++ to_string(Value) || {Option, Value} <- Edge#edge.options],
	"[" ++ string:join(Ret, " ") ++ "]".

