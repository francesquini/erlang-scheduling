-record(
   proc_info, 
   	{
		pid, 
		ppid, 
		start_ts = undef, 
		end_ts = undef,
		sched_ins=[],
		sched_outs=[],
		sent_msgs=[],
		context_switches = 0,
		%% Control flags
		exited = false,
		out_exited = false		
  	}
).