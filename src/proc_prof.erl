-module (proc_prof).

-include("proc.hrl").

-export([start/0, start/1, stop/1, read_file/2, read_info_file/1]).

-import(utils, [time_to_number/1]).

-export([waitCompletion/3]).

start() ->
	Filename = "Profile." ++ integer_to_list(time_to_number(erlang:now())), 
	start(Filename).

start(Filename) ->
	Now = now(),
	PCount = length(erlang:processes()),
	FileFun = dbg:trace_port(file, Filename), 
	{ok, _TracerPid} = dbg:tracer(port, FileFun),
	{ok, _} = dbg:p(all, [procs, running, send, exiting, timestamp]),
	spawn_link(?MODULE, waitCompletion, [Filename, Now, PCount]).

stop(Pid) ->
	Pid ! stop,
	ok.

read_file (Filename, MessageHandler) -> 
	AccProc = spawn_link(fun () -> readerLoop(self(), MessageHandler) end),
	HandlerFun = fun (Each, _) ->  AccProc ! Each end,
	TracePid = dbg:trace_client(file, Filename, {HandlerFun, nothing}),
	link(TracePid),
	TracePid.

read_info_file(Filename) ->
	NFileName = Filename ++ ".info",
	{ok, Terms} = file:consult(NFileName),
	Terms.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readerLoop(StarterPid, Handler) ->
	receive
		end_of_trace -> 
			%dbg:stop_trace_client(ClientPid),
			Handler({end_of_trace});							
		Log ->
			handleData(Log, Handler),
			readerLoop(StarterPid, Handler)
	end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Processes - procs %%
%%%%%%%%%%%%%%%%%%%%%%%
handleData({trace_ts, Pid, exit, _Info, Timestamp}, Handler) -> %Process exit 
	Handler ({exit, Pid, Timestamp});
handleData({trace_ts, PPid, spawn, Pid, _Info, Timestamp}, Handler) -> %Process spawned
	Handler ({spawn, Pid, PPid, Timestamp});
handleData({trace_ts, _, link, _, _}, _) -> %Process Linked - ignored
	ok;
handleData({trace_ts, _, unlink, _, _}, _) -> %Process Unlinked - ignore
	ok;
handleData({trace_ts, _, getting_linked, _, _}, _) -> %Process getting linked - ignore
	ok;
handleData({trace_ts, _, getting_unlinked, _, _}, _) -> %Process getting ulinked - ignore
	ok;
handleData({trace_ts, _, register,_, _}, _) -> %Process registered - ignore
	ok;
handleData({trace_ts, _, unregister,_, _}, _) -> %Process unregistered - ignore
	ok;
%%%%%%%%%%%%%%%%%%%%%
%% Messages - send %%
%%%%%%%%%%%%%%%%%%%%%
handleData({trace_ts, Pid, send, Msg, To, Timestamp}, Handler) -> %Process sent a message
	Handler({send, Pid, To, Timestamp, byte_size(term_to_binary(Msg))});
handleData({trace_ts, Pid, send_to_non_existing_process, Msg, To, Timestamp}, Handler) -> %Process sent a message nowhere
	Handler({send_to_non_existing_process, Pid, To, Timestamp, byte_size(term_to_binary(Msg))});
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Scheduling - running %%
%%%%%%%%%%%%%%%%%%%%%%%%%%
handleData({trace_ts, Pid, in, _ModuleFuncion, Timestamp}, Handler) -> %Process scheduled for execution
	Handler({in, Pid, Timestamp});
handleData({trace_ts, Pid, out, _ModuleFuncion, Timestamp}, Handler) -> %Process scheduled for execution
	Handler({out, Pid, Timestamp});
%%%%%%%%%%%%%%%%%%%%%
%% Dying - exiting %%
%%%%%%%%%%%%%%%%%%%%%
handleData({trace_ts,Pid, out_exited, _, Timestamp}, Handler) -> %Died!
	Handler ({out_exited, Pid, Timestamp});	
handleData({trace_ts, _, in_exiting, _, _}, _) -> %Becoming zombie
	ok;
handleData({trace_ts, _, out_exiting, _, _}, _) -> %Dying zombie
	ok;
%%%%%%%%%%%%%%%%%%%%%
%% Everything else %%
%%%%%%%%%%%%%%%%%%%%%
handleData(Data, _) -> 
	io:format("Unknown format received: ~p~n", [Data]),
	ok.

waitCompletion (Filename, Start, NProcs) ->
	End = receive
		stop ->
			dbg:flush_trace_port(), %Some events might be lost
			dbg:stop_clear(),
			now()
	end,
	%log info
	Dest = Filename ++ ".info",
	{ok, FileId} = file:open(Dest, [read, write]),
	io:fwrite(FileId, "~p.~n~p.~n~p.~n", [Start, End, NProcs]), 
	file:close(FileId),
	io:format("Profiler stopped. Wrote: ~p~n", [Filename]),
	ok.


