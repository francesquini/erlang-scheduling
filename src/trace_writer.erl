%% Author: emilio
%% Created: 2 oct. 2012
%% Description: TODO: Add description to trace_writer
-module(trace_writer).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([convert/2]).

-import(utils, [to_int/1]).

%%
%% API Functions
%%

convert (InputFileName, OutputFileName) ->
	{ok, InFile} = file:open(InputFileName, [read]),
	{ok, OutFile} = file:open(OutputFileName, [read, write]),
	{MaxScheduler, Events} = processInputFile(InFile),
	writeHeader(OutFile, MaxScheduler),
	writeEvents(OutFile, Events),
	file:close(InFile),
	file:close(OutFile).
	
%%
%% Local Functions
%%
processInputFile(File) ->
	processInputFile(File, [], -1).

processInputFile (File, Acc, MaxScheduler) ->
	case file:read_line(File) of
		{ok, Line} ->
			Event = string:tokens(Line, "\t "),
			NMaxScheduler = max (MaxScheduler, 
				case Event of
					[_, S, _] -> utils:to_int(S);
					[_, _, SFrom, Sto, _] -> max (utils:to_int(SFrom), utils:to_int(Sto))
				end),						
			processInputFile(File, [Event | Acc], NMaxScheduler);
		 eof -> 
			{MaxScheduler, lists:reverse(Acc)}
	end.

writeHeader (File, MaxScheduler) ->
	H1 = [		
		"%EventDef PajeDefineContainerType 1",
		"% Alias string", 
		"% ContainerType string", 
		"% Name string", 
		"%EndEventDef", 
		"%EventDef PajeDefineStateType 2",
		"% Alias string", 
		"% ContainerType string", 
		"% Name string", 
		"%EndEventDef", 
		"%EventDef PajeDefineEntityValue 3",
		"% Alias string",  
		"% EntityType string",  
		"% Name string",  
		"% Color color", 
		"%EndEventDef",  
		"%EventDef PajeCreateContainer 4",
		"% Time date",  
		"% Alias string",  
		"% Type string",  
		"% Container string",  
		"% Name string",  
		"%EndEventDef",  
		"%EventDef PajeSetState 10",
		"% Time date",  
		"% Type string",  
		"% Container string",  
		"% Value string",  
		"%EndEventDef",
		
		"%EventDef PajeDefineEventType		20",
		"% 	  Alias 	string",            
		"% 	  ContainerType string",            
		"% 	  Name 		string",            
		"%EndEventDef",     
		"%EventDef PajeNewEvent			21",
		"% 	  Time          date",              
		"% 	  Type 		string",            
		"% 	  Container 	string",            
		"% 	  Value         string",            
		"%EndEventDef", 
		
		"%EventDef PajeDefineLinkType		41",
		"% 	  Alias               string",      
		"% 	  Name 		      string",      
		"% 	  ContainerType	      string",      
		"% 	  SourceContainerType string",      
		"% 	  DestContainerType   string",      
		"%EndEventDef",
		"%EventDef PajeStartLink	       	42",
		"%	  Time 		  date",            
		"%	  Type 		  string",          
		"%	  Container 	  string",          
		"%	  SourceContainer string",          
		"%	  Value 	  string",          
		"%	  Key 		  string",          
		"%EndEventDef",                             
		"%EventDef PajeEndLink			43",
		"% 	  Time          date",              
		"% 	  Type 		string",            
		"% 	  Container 	string",            
		"% 	  DestContainer string",            
		"% 	  Value 	string",            
		"% 	  Key 		string",            
		"%EndEventDef", 
	
		"1 VM 0 'Erlang VM'",
		"1 S VM 'Scheduler'",
		"2 SS S 'Scheduler State'",
		"3 A SS 'Active'    '1.0 0.5 0.5'",
		"3 I SS 'Inactive'  '0.5 0.5 0.5'",
		"4 0.000000 VM VM 0 'Erlang VM'",
		"20 CB VM 'Check-Balance'", 
		"20 WS S 'Work-Stealing'", 
		"41 Mig Migration VM S S"
	],
	
	[io:fwrite(File, "~s~n", [Line]) || Line <- H1],
	[io:fwrite(File, "4 0.000000 S~p S VM 'Scheduler ~p'~n", [Sched, Sched]) || Sched <- lists:seq(1, MaxScheduler)],
	ok. 

writeEvents(File, Events) ->
	writeEvents(File, Events, 1).

writeEvents(_, [], _) ->
	ok;
%Check-Balance
writeEvents(File, [["CB", Scheduler, When] | Events], Cont) ->
	io:fwrite(File, "21 ~p CB VM ~p~n", [to_int(When)/1000000000, Scheduler]),
	writeEvents(File, Events, Cont);
%Work-Stealing
writeEvents(File, [["WS", Scheduler, When] | Events], Cont) ->
	io:fwrite(File, "21 ~p WS S~s ~p~n", [to_int(When)/1000000000, Scheduler, 0]),
	writeEvents(File, Events, Cont);
%Scheduler Active/Inactive
writeEvents(File, [[State, Scheduler, When] | Events], Cont) -> %State = A or I
	io:fwrite(File, "10 ~p SS S~s ~s~n", [to_int(When)/1000000000, Scheduler, State]),
	writeEvents(File, Events, Cont);
%Process Migration
writeEvents(File, [["M", _, From, To, When] | Events], Cont) ->
	NWhen = to_int(When) / 1000000000,
	Bit = 0.0000000001,
	io:fwrite(File, "42 ~p Mig VM S~s ~p ~p~n", [NWhen, From, Cont, Cont]),
	io:fwrite(File, "43 ~p Mig VM S~s ~p ~p~n", [NWhen + Bit, To, Cont, Cont]),
	writeEvents(File, Events, Cont + 1).
