%%% $Id: fs_exit_handler.erl,v 1.9 2004/07/23 18:49:31 tfee Exp $
%%%-------------------------------------------------------------------
%%% File    : fs_exit_handler_h.erl
%%%
%%% @doc  This module is for monitoring processes and recording failures. 
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(fs_exit_handler).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------
-export ([
	  start_link/0, 
	  stop/0, 
	  get_stats/1, 
	  get_stats/0, 
	  monitor/2, 
	  monitor/3,
          string_format/1
	 ]).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------
% Internal Record
-record (proc_info, {death_count = 0, 
                     start_time, 
                     timestamp = infinity, 
		     reasons = [], 
                     user_fun}).

% External record
-record (proc_stats, {process_name = undefined, 
		      deaths = 0, 
		      start_time = {{0,0,0},{0,0,0}}, 
		      last_death = {{0,0,0},{0,0,0}}, 
		      reasons = []}).

%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%-----------------------------------------------------------------------
%% @doc Returns the stats for a given process name.
%% <pre>
%% Types:
%%  ProcStats = record() #proc_stats(process_name, deaths, start_time, last_death, reasons}
%%   process_name = term()
%%   deaths = integer()
%%   last_death = {date(), time()} | infinity
%%   start_time = {date(), time()}
%%   reasons = [atom()]
%% </pre>
%% @spec get_stats(ProcessName) -> {ok, ProcStats} | {error, no_proc_stats} | exit(Reason)
%% @end
%%-----------------------------------------------------------------------

get_stats (ProcessName) ->
    gen_server:call (?SERVER, {get_stats, ProcessName}).

%%-----------------------------------------------------------------------
%% @doc Returns the stats for a all processes.
%% <pre>
%% Types:
%%  ProcStatsList = [ProcStats]
%%   ProcStats = record() #proc_stats(process_name, deaths, start_time, last_death, reasons}
%%   process_name = term()
%%   deaths = integer()
%%   last_death = {date(), time()} | infinity
%%   start_time = {date(), time()}
%%   reasons = [atom()]
%% </pre>
%% @spec get_stats() -> ProcStatsList | exit(Reason)
%% @end
%%-----------------------------------------------------------------------

get_stats () ->
    gen_server:call (?SERVER, get_stats).

%%-----------------------------------------------------------------------
%% @doc This is a request to monitor a process and execute a fun() on an exit.
%% <pre>
%% Expects:
%%  Pid - The pid of the process to be monitored.
%%  ProcessName - A way to referance the process by a user defined name.
%%  Fun - A fun() to be executed upon an exit of a monitored process.
%%
%% Types:
%%  Pid = pid()
%%  ProcessName = term()
%%  Fun = fun()
%% </pre>
%% @spec monitor(Pid, ProcessName, Fun) -> {ok, Deaths} | {error, Error}
%% @end
%%-----------------------------------------------------------------------

monitor (Pid, ProcessName, Fun) ->
    case catch gen_server:call (?SERVER, {monitor, {Pid, ProcessName, Fun}}) of
        {ok, Deaths} -> {ok, Deaths};
        Error        -> {error, Error}
    end.
   
%% @spec monitor(Pid, ProcessName) -> {ok, Deaths} | {error, Error}
%% @equiv monitor(Pid, ProcessName, undefined)

monitor (Pid, ProcessName) ->
    monitor (Pid, ProcessName, undefined).

%%-----------------------------------------------------------------------
%% @doc Format the response from get_stats the web using tags.
%% @spec string_format(ExitInfo) -> string()
%% @end
%%-----------------------------------------------------------------------

string_format (ExitInfo) ->
    string_format_stats (ExitInfo).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    process_flag (trap_exit, true),
    % The state consists of a dictionary list of "dead_procs" records. Somthing is
    % done with this at some point - I don't feel like thinking about 
    % it now. It is possible to implement a scheme where
    % a process is capable of diferentiating between a start and a
    % restart
    %%error_logger:info_msg ("fs_exit_handler:init/1 ~n"),
    {ok, create_storage ()}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({monitor, {Pid, ProcessName, Fun} = PL}, From, {PidS, NameS} = Storage) ->
    catch link(Pid),
    Deaths = case ets:lookup(NameS, ProcessName) of
                 []                        -> 0;
                 [{ProcessName, ProcInfo}] -> ProcInfo#proc_info.death_count
             end,
    {reply, {ok, Deaths}, monitor_process(Pid, ProcessName, Fun, Storage)};

handle_call({get_stats, ProcessName}, From, {PidS, NameS}) ->
    case ets:lookup (NameS, ProcessName) of
        [Stats] -> {reply, format_stats(Stats), {PidS, NameS}};
        []      -> {reply, {error, no_proc_stats}, {PidS, NameS}}
    end;
handle_call(get_stats, From, {PidS, NameS}) ->
    Records = ets:match (NameS, '$1'),
    Stats = lists:foldl (fun ([Record], Acc) -> [format_stats (Record) | Acc] end, [], Records),
    {reply, Stats, {PidS, NameS}}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info ({'EXIT', Pid, Reason}, Storage) ->
    {noreply, proc_died (Pid, Reason, Storage)}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------


%% Creates a new state storage space
create_storage () ->
    {ets:new (pid_s, [public, set]), 
     ets:new (name_s, [public, set])}.


%% This function updates a record if one is presant for a key or creates
%% a new one if one is not presant.
monitor_process (Pid, ProcessName, Fun, {PidS, NameS}) ->
    ets:insert (PidS, {Pid, ProcessName}),
    is_restart (ets:lookup (NameS, ProcessName), Pid, ProcessName, Fun, {PidS, NameS}).

is_restart ([], Pid, ProcessName, Fun, {PidS, NameS}) -> 
    ets:insert (NameS, {ProcessName, #proc_info{start_time = erlang:localtime (), user_fun = Fun}}),
    {PidS, NameS}; 
is_restart ([{ProcessName, ProcInfo}], Pid, ProcessName, Fun, Storage) ->
    Storage.


%% This function updates the NameInfo in the event that a process died
proc_died (Pid, Reason, {PidS, NameS}) ->
    [{Pid, ProcessName}] = ets:lookup (PidS, Pid),
    ets:delete (PidS, Pid),
    [{ProcessName, ProcInfo}] = ets:lookup (NameS, ProcessName),
    NewDeathCount = ProcInfo#proc_info.death_count + 1,
    NewReason     = add_reasons (Reason, ProcInfo#proc_info.reasons),
    execute_fun (ProcInfo#proc_info.user_fun),
    ets:insert (NameS, {ProcessName, 
			ProcInfo#proc_info{death_count = NewDeathCount, reasons = NewReason, 
					   timestamp = erlang:localtime ()}}),
    {PidS, NameS}.

add_reasons (NewReason, Reasons) when length (Reasons) > 9 ->
    [NewReason | drop_tail (Reasons)];
add_reasons (NewReason, Reasons) ->
    [NewReason | Reasons].

execute_fun (undefined) ->
    ok;
execute_fun (Fun) ->
    Fun ().

%%------------------------------------------------------------------------------
%% This fuction formats the stats on a process for output.
%%------------------------------------------------------------------------------
format_stats ({ProcessName, Stats}) ->
    #proc_info{death_count = Deaths, 
               start_time  = StartTime, 
               timestamp   = LastDeath, 
               reasons      = Reasons} = Stats,

    #proc_stats{process_name = ProcessName, 
                deaths       = Deaths, 
                start_time   = StartTime, 
                last_death   = LastDeath, 
                reasons      = Reasons}.

drop_tail ([]) ->
    [];
drop_tail (List) ->
    [H|T] = lists:reverse (List),
    lists:reverse (T).


date_time_to_list ({{Y, M, D}, {H, Min, S}}) ->
    integer_to_list (Y) ++ "/" ++ integer_to_list (M) ++ "/" ++ 
        integer_to_list (D) ++ " " ++ integer_to_list (H) ++ ":" ++ 
        integer_to_list (Min) ++ ":" ++ integer_to_list (S).


% format a proc_stats record into a string for use with web.
string_format_stats (#proc_stats{process_name = ProcessName, deaths = Deaths, start_time = StartTime, last_death = LastDeath, reasons = Reasons}) -> 
    ["Process Name: " ++ atom_to_list (ProcessName) ++ "\n",
     "Deaths: " ++ integer_to_list (Deaths) ++ "\n",
     "Start Time: " ++ date_time_to_list (StartTime) ++ "\n",
     "Last Death: " ++ case LastDeath of
			   infinity -> " never died";
			   LastDeath -> fs_time:date_time_to_string(LastDeath)
		       end ++ "\n",
     "Reasons: " ++ lists:foldl (fun (Reason, "") -> 
					 atom_to_list (Reason);
				     (Reason, Acc) -> 
					 Acc ++ ", " ++ atom_to_list (Reason) 
				 end, "", Reasons) ++ "\n"].
