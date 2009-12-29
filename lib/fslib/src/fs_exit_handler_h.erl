%%%---------------------------------------------------------------------
%%% $CVS$
%%% File    : exit_handler.erl
%%% Author  : Martin J. Logan <martin@gdubyaerlware>
%%% @doc 
%%%
%%% <p>This module is an event handler that monitors the exit
%%% signals of every process in the system that requests to 
%%% be monitored. </p>
%%%
%%% @end
%%% Created :  8 Jan 2002 by Martin J. Logan <martin@gdubyaerlware>
%%%---------------------------------------------------------------------

-module (fs_exit_handler_h).
-author ('martin@gdubya.botomayo').
-vsn ('2.0').

-behaviour (gen_event).

%%-----------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------
-export ([
	  start_link/1, 
	  start_link/0, 
	  stop/0, 
	  get_stats/1, 
	  get_stats/0, 
	  monitor/2, 
	  monitor/3,
         string_format/1,
	  configuration_spec/1
	 ]).

%%-----------------------------------------------------------------------
%% gen_event callbacks
%%-----------------------------------------------------------------------

-export ([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

%%-----------------------------------------------------------------------
%% Record Definitions
%%-----------------------------------------------------------------------

-record (proc_info, {death_count = 0, start_time, timestamp = infinity, 
		     reason = [], user_fun}).

-record (proc_stats, {process_name = undefined, 
		      deaths = 0, 
		      start_time = {{0,0,0},{0,0,0}}, 
		      last_death = {{0,0,0},{0,0,0}}, 
		      reasons = []}).

%%-----------------------------------------------------------------------
%% Macro Definitions
%%-----------------------------------------------------------------------

-define (MANAGER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc Starts the exit_handler server for gas.
%% @spec start_link(Configuration) -> {ok, Pid}
%% @end
%%-----------------------------------------------------------------------

start_link ([]) ->
    start_link ().

%%-----------------------------------------------------------------------
%% @doc Starts the exit_handler server.
%% @spec start_link() -> {ok, Pid}
%% @end
%%-----------------------------------------------------------------------
start_link () ->
    {ok, Pid} = gen_event:start_link ({local, ?MANAGER}), 
    gen_event:add_handler (?MANAGER, ?MODULE, []),
    {ok, Pid}.

%%-----------------------------------------------------------------------
%% @doc Stops the exit_handler server.
%% @end
%%-----------------------------------------------------------------------

stop () ->
    gen_event:stop (?MANAGER).

%%-----------------------------------------------------------------------
%% @doc Returns the stats for a given process name.
%% <pre>
%% Types:
%%  ExitInfo = record() #proc_stats(process_name, deaths, start_time, last_death, reasons}
%%   process_name = term()
%%   deaths = integer()
%%   last_death = {date(), time()} | infinity
%%   start_time = {date(), time()}
%%   reasons = [atom()]
%% </pre>
%% @spec get_stats(ProcessName) -> ExitInfo | exit(Reason)
%% @end
%%-----------------------------------------------------------------------

get_stats (ProcessName) ->
    gen_event:call (?MANAGER, ?MODULE, {get_stats, ProcessName}).

%%-----------------------------------------------------------------------
%% @doc Returns the stats for a all processes.
%% <pre>
%% Types:
%%  ExitInfoList = [ExitInfo]
%%   ExitInfo = record() #proc_stats(process_name, deaths, start_time, last_death, reasons}
%%   process_name = term()
%%   deaths = integer()
%%   last_death = {date(), time()} | infinity
%%   start_time = {date(), time()}
%%   reasons = [atom()]
%% </pre>
%% @spec get_stats() -> ExitInfoList | exit(Reason)
%% @end
%%-----------------------------------------------------------------------

get_stats () ->
    gen_event:call (?MANAGER, ?MODULE, get_stats).

%%-----------------------------------------------------------------------
%% @doc Request to monitor a process.
%% <pre>
%% Expects:
%%  Pid - The pid of the process to be monitored.
%%  ProcessName - A way to referance the process by a user defined name.
%%
%% Types:
%%  Pid = pid()
%%  ProcessName = term()
%% </pre>
%% @spec monitor(Pid, ProcessName) -> ok | exit(Reason)
%% @end
%%-----------------------------------------------------------------------

monitor (Pid, ProcessName) ->
    monitor (Pid, ProcessName, undefined).

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
%% @spec monitor(Pid, ProcessName, Fun) -> ok | exit(Reason)
%% @end
%%-----------------------------------------------------------------------

monitor (Pid, ProcessName, Fun) ->
    gen_event:notify (?MANAGER, {monitor, {Pid, ProcessName, Fun}}).

%%-----------------------------------------------------------------------
%% @doc Format the response from get_stats the web using tags.
%% @spec string_format(ExitInfo) -> string()
%% @end
%%-----------------------------------------------------------------------

string_format (ExitInfo) ->
    string_format_stats (ExitInfo).

%%--------------------------------------------------------------------
%% @doc Returns the keys, required or optional, used for configuration of this process. 
%% <pre>
%% Returns the configuration_spec for this module. Conforms to the GAS behaviour.
%% 
%% Variables:
%%  Function - The function that configuration_spec pertains to.
%%
%% Types:
%%  ConfigurationSpec = [Spec]
%%   Spec = {optional, ConfToken} | {required, ConfToken}
%%    ConfToken = {App, Key} | Key
%%  Function = atom()
%% </pre>
%% @spec configuration_spec(Function) -> CongifurationSpec
%% @end
%%--------------------------------------------------------------------

configuration_spec (start_link) ->
    [].

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------

init ([]) ->
    process_flag (trap_exit, true),
    % The state consists of a dictionary list of "dead_procs" records. Somthing is
    % done with this at some point - I don't feel like thinking about 
    % it now. It is possible to implement a scheme where
    % a process is capable of diferentiating between a start and a
    % restart
    error_logger:info_msg ("fs_exit_handler_h:init~n"),
    {ok, create_storage ()}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                               
%%----------------------------------------------------------------------

handle_event ({monitor, {Pid, ProcessName, Fun} = PL}, Storage) ->
    %error_logger:info_msg ("fs_exit_handler_h:handle_call monitor ~p~n", [PL]),
    link (Pid),
    Storage2 = monitor_process (Pid, ProcessName, Fun, Storage),
    {ok, Storage2}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------

handle_call ({get_stats, ProcessName}, {PidS, NameS}) ->
    {ok, format_stats (ets:lookup (NameS, ProcessName)), {PidS, NameS}};
handle_call (get_stats, {PidS, NameS}) ->
    Records = ets:match (NameS, '$1'),
    Stats = lists:foldl (fun (Record, Acc) -> [format_stats (Record) | Acc] end, [], Records),
    {ok, Stats, {PidS, NameS}}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------

handle_info ({'EXIT', Pid, Reason}, {PidS, NameS}) ->
    %% Here is where notification of some other event handler takes place
    %% examples would be logging or alarms.
    %% gen_event:notify(error_logger, {non_normal_exit, Pid, Reason}),
    %error_logger:info_msg ("fs_exit_handler_h:handle_info process exited ~p ~p~n", [Pid, Reason]),
    {ok, proc_died (Pid, Reason, {PidS, NameS})}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------

terminate (Reason, State) ->
    % do something with respect to logging the info stored in the PidInfo
    % dict.
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------

code_change (OldVsn, State, Extra) ->
    {ok, State}.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

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
    NewReason     = add_reasons (Reason, ProcInfo#proc_info.reason),
    execute_fun (ProcInfo#proc_info.user_fun),
    ets:insert (NameS, {ProcessName, 
			ProcInfo#proc_info{death_count = NewDeathCount, reason = NewReason, 
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
format_stats ([])                     -> #proc_stats{};
format_stats ([{ProcessName, Stats}]) ->
    #proc_info{death_count = Deaths, 
               start_time  = StartTime, 
               timestamp   = LastDeath, 
               reason      = Reasons} = Stats,

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
			   LastDeath -> date_time_to_list (LastDeath)
		       end ++ "\n",
     "Reasons: " ++ lists:foldl (fun (Reason, "") -> 
					 atom_to_list (Reason);
				     (Reason, Acc) -> 
					 Acc ++ ", " ++ atom_to_list (Reason) 
				 end, "", Reasons) ++ "\n"].
