%%%-------------------------------------------------------------------
%%% File    : fs_test_lib.erl
%%% Author  : Martin J. Logan <martin@mecha.erlware.com>
%%%
%%% @doc Functions for starting and stopping nodes, writing test suits,  
%%%      and managing test fixtures with the Erlang test server. 
%%%      
%%% @end
%%%
%%% Created : 17 Oct 2003 by Martin J. Logan <martin@erlware.com>
%%%-------------------------------------------------------------------
-module(fs_test_lib).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("fs_test_lib.hrl").
-include("macros.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         stop_node/1,
         start_node/2,
         start_node_with_display/2,
         poll_until/4,
         poll_until/3,
         fetch_release_value/2,
         create_base_node_command/1,
         prepend_username/1,
         otp_dir/1,
         linked_private_file/2,
	 pkill_beam/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define (NODE_SHUTDOWN_TIME_SECONDS, 10).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Performs an orderly shutdown on the node. Always succeeds.
%% <pre>
%% Variables:
%%  LongNodeName - The long node name of the node to stop.
%%
%% Types:
%%  LongNodeName = atom() example: 'zubin_parser@localhost'
%% </pre>
%% @spec stop_node (LongNodeName) -> true
%% @end 
%%--------------------------------------------------------------------
stop_node (LongNodeName) ->
    InitialNumber = pgrep_beam(),
    case InitialNumber of
	1 -> true;	% Nothing to kill (Probably because of a prior brutal kill).
	_ ->
	    os:cmd("sync"),
	    catch rpc:call (LongNodeName, init, stop, []),

	    ProcessIsGone = fun()->
		pgrep_beam() < InitialNumber
	    end,
	    poll_until (ProcessIsGone, 60, 500),  % (Condition, Attempts, PauseMilliseconds)

	    Gone = ProcessIsGone(),
	    if Gone -> true;
	       true -> pkill_beam()		  % Note: We were trying to stop a single beam but now, in desperation, we're killing them all (except the test server).
	    end
    end.

pgrep_beam () ->
    Command = "pgrep -u `id -u` beam | xargs ps | sed -e /bash$/d -e /xargs$/d -e /ps$/d -e /sed$/d",
    Beams = string:tokens(os:cmd(Command), "\n"),
    NumberOfBeams = length(Beams) - 1.

%%--------------------------------------------------------------------
%% @doc Kill beam processes owned by user, leaving the one(s) with _tester in the command-line.
%% @spec pkill_beam () -> true
%% @end 
%%--------------------------------------------------------------------
pkill_beam () ->
    %% All the beams owned by the user running the test, except the one with _tester, are brutally killed.
    Command = "pgrep -u `id -u` beam | xargs ps | sed -e 1d -e /bash$/d -e /xargs$/d -e /ps$/d -e /sed$/d -e /_tester/d | awk '{print $1}' | xargs kill -9",
    os:cmd(Command),
    true.


%%--------------------------------------------------------------------
%% @doc Start an erlang node. 
%% Set the TEST_OUTPUT env var to get the output of the node directed to its own xterm.
%% <pre>
%% Variables:
%%  NodeName - What to name the node the release resides in.
%%  Command - A command to start a node with erl.
%%
%% Types:
%%  NodeName = atom() example: zubin_galaxy_parser
%%  Command = string() example: "erl -name martin"
%%  LongNodeName = atom() example: 'zubin_galaxy_parser@localhost'
%% </pre>
%% @spec start_node(NodeName, Command) -> {ok, LongNodeName}
%% @end 
%%--------------------------------------------------------------------
start_node (NodeName, Command) ->
    io:format ("fs_test_lib:start_node name ~p command ~p~n", [NodeName, Command]),
    case os:getenv ("TEST_OUTPUT") of
	false ->
            spawn_it_detached (NodeName, Command);

	EnvVar ->
            case start_node_with_display (NodeName, Command) of
                {error, Reason}    -> spawn_it_detached (NodeName, Command);
                {ok, LongNodeName} -> {ok, LongNodeName}
            end
    end.

spawn_it_detached (NodeName, Command) ->
    spawn_link (fun () -> shell (NodeName, Command ++ " -detached") end),
    {ok, create_long_node_name (NodeName)}.

    

%%--------------------------------------------------------------------
%% @doc Start an erlang node with output going to its own xterm.
%% <pre>
%% Note* -If the DISPLAY env var is not set this will fail.
%%       -If the DEBUG env variable is set and contains the long node name 
%%        then the debugger is started on that node.
%%       -If the TEST_OUTPUT env var is set to "hold" the -hold option will 
%%        be applied to the xterm.
%%
%% Variables:
%%  NodeName - What to name the node the release resides in.
%%  Command - A command to start a node with erl.
%%
%% Types:
%%  NodeName = atom() example: zubin_galaxy_parser
%%  Command = string() example: "erl -name martin"
%%  LongNodeName = atom() example: 'zubin_galaxy_parser@localhost'
%% </pre>
%% @spec start_node_with_display(NodeNode, Command) -> {ok, LongNodeName} | {error, Reason}
%% @end 
%%--------------------------------------------------------------------
start_node_with_display(NodeName, Command) ->
    have_xterm(NodeName, Command).

have_xterm(NodeName, Command) ->
    case os:find_executable ("xterm") of
        false -> {error, enoxterm};
        Xterm -> have_display(NodeName, Command)
    end.

have_display(NodeName, Command) ->
    case os:getenv ("DISPLAY") of
        false   -> {error, nodisplay};
        Display -> do_start_node_with_display(NodeName, Command)
    end.

do_start_node_with_display(NodeName, Command) ->
    %% Atom is used for node name to preserve continuity.
    NodeNameString = atom_to_list(NodeName),

    %% Debug the node?
    Mode = case lists:member (NodeNameString, debug_targets ()) of
               true  -> " -s debugger start -err_log_tty true";
               false -> " -err_log_tty true"
           end,

    case os:getenv ("TEST_OUTPUT") of
        "hold" -> spawn_link(fun() -> shell(NodeName, "xterm -hold -e " ++ Command ++ Mode) end);
        _      -> spawn_link(fun() -> shell(NodeName, "xterm -e " ++ Command ++ Mode) end)
    end,
                  
    LongNodeName = create_long_node_name(NodeName),
    {ok, LongNodeName}.


    

%%--------------------------------------------------------------------
%% @doc Create a basic node command. This means that it creates a default
%% command line structure  for starting a node. All the user of this function
%% has to do as add various exra arguments to the returned string.
%% <pre>
%% Note* the assumption is made that the script and bood files and config files reside in the same dir.
%%
%% Variables:
%%  Release - This is a record that is defined in fs_test_lib.hrl see the include dir.
%% 
%% Types:
%%  Release = record()
%%  BaseCommand = string()
%%
%% Example: currently base command looks like the following:
%%  cd ScriptAndBootDir; erl -name SomeName -boot SomeReleaseName -config SomeReleaseName.config - sasl sasl_error_logger {file, SaslLogDir -gas err_log ErrLogDir 
%% </pre> 
%% @spec create_base_node_command(Release) -> BaseCommand
%% @end
%%--------------------------------------------------------------------
create_base_node_command(#release{name = RN, node_name = NN, sasl_log = SL, err_log = EL, contact_node = CN, script_and_boot_dir = SB, config_file = CF}) ->  
    BaseCommand = ["erl -name ", atom_to_list(NN), " -boot ", SB ++ "/" ++ atom_to_list(RN),
                   " -config ", SB ++ "/" ++ atom_to_list(CF), ".config",
                   " -contact_node ", atom_to_list (CN),
                   " -sasl sasl_error_logger \\{file\\,\\\"", SL, "\\\"\\}",
                   " -gas err_log \\'", EL, "\\' "],
    lists:flatten(BaseCommand).

%%--------------------------------------------------------------------
%% @doc Fetch a value from a #release record. This is nessisary because of the non
%% dynamic nature of erlang records. Booo hooo.
%% <pre>
%% Note* This function must be kept in sync with the release record in include/fs_test_lib.hrl
%%
%% Types:
%%  Key = name     | node_name    | sasl_log            | long_node_name |
%%        err_log  | contact_node | script_and_boot_dir | 
%% </pre>
%% @spec fetch_release_value(Key, Release) -> Value | exit()
%% @end
%%--------------------------------------------------------------------
fetch_release_value(name,                #release{name = Value})                -> Value;
fetch_release_value(node_name,           #release{node_name = Value})           -> Value;
fetch_release_value(sasl_log,            #release{sasl_log = Value})            -> Value;
fetch_release_value(err_log,             #release{err_log = Value})             -> Value;
fetch_release_value(contact_node,        #release{contact_node = Value})        -> Value;
fetch_release_value(config_file,         #release{config_file = Value})         -> Value;
fetch_release_value(script_and_boot_dir, #release{script_and_boot_dir = Value}) -> Value;
fetch_release_value(long_node_name,      #release{long_node_name = Value})      -> Value.

%%--------------------------------------------------------------------
%% @doc This is a higher order function that allows for Iterations number of executions of Fun until Result is returned pausing for PauseMS after each execution.
%% <pre>
%% Variables:
%%  Fun - A fun to execute per iteration.
%%  Reply - If the fun returns this we return tr
%%  Iterations - The maximum number of iterations to try getting Reply out of Fun.  
%%  PauseMS - The number of miliseconds to wait inbetween each iteration.
%%  Return - What ever the fun returns.
%%
%% Types:
%%  Fun = fun()
%%  Reply = term()
%%  PauseMS = Iterations = integer()
%%  Return = term()
%% </pre>
%% @spec poll_until(Fun, Reply, Iterations, PauseMS) -> Return
%% @end
%%--------------------------------------------------------------------
poll_until(Fun, Reply, 0, PauseMS) ->
    Fun();
poll_until(Fun, Reply, Iterations, PauseMS) ->
    case Fun() of
        Reply -> 
            Reply;
        NotReply -> 
            timer:sleep(PauseMS),
            case Iterations of 
                infinity   -> poll_until(Fun, Reply, Iterations, PauseMS);
                Iterations -> poll_until(Fun, Reply, Iterations - 1, PauseMS)
            end
    end.
            
%% @spec poll_until(Fun, Iterations, PauseMS) -> Return
%% @equiv poll_until(Fun, true, Iterations, PauseMS)
poll_until (Fun, Iterations, PauseMS) ->
    poll_until (Fun, true, Iterations, PauseMS).


%%--------------------------------------------------------------------
%% @doc Appends the current user name to the atom or string passed in.
%% <pre>
%% Variables:
%%  Token - The string or atom to have an underscore and the current username prepended to.
%%
%% Types:
%%  Token = atom() | string()
%% 
%% example: prepend_username(gq) -> martin_gq
%% </pre>
%% @spec prepend_username(Token) -> atom() | string() | exit()
%% @end
%%--------------------------------------------------------------------
prepend_username(Token) when is_atom(Token) ->
    list_to_atom(prepend_username(atom_to_list(Token)));
prepend_username(Token) ->
    os:getenv("USER") ++ "_" ++ Token.
    
%%----------------------------------------------------------
%% @doc Takes test server Config and returns a path to otp directory
%% <pre>
%% Variables:
%%  Config - per test case configuration generated by test server
%%
%% Types:
%%  Config = list()
%% 
%% example:
%%  otp_dir(Config) -> "/home/Username/otp"
%% </pre>
%% @spec otp_dir(Config) -> string()
%% @end
%%----------------------------------------------------------
otp_dir (Config) ->
    {value, {_, DataDir}} = lists:keysearch (data_dir, 1, Config),
    strip_after (lists:reverse (DataDir)).

strip_after ([$p, $t, $o, $/ | T] = Text) ->
    lists:reverse (T) ++ "/otp";
strip_after ([_ | T]) ->
    strip_after (T).

%%----------------------------------------------------------
%% @doc Writes a hyper text link to a user supplied file into the w3m last_test.html test results
%% <pre>
%% Variables:
%% Filename - user supplied file name 
%% Config - per test case configuration generated by test server
%%
%% Types:
%%  Filename = list()
%%  Config = list()
%%
%% example:
%%  linked_private_file(Filename, Config) -> PrivDir/FileName
%% </pre>
%% @spec linked_private_file(Filename, Config) -> string()
%% @end
%%----------------------------------------------------------
linked_private_file (Filename, Config) when is_list (Filename) ->
    linked_private_file (Filename, Filename, Config).

linked_private_file (Filename, Description, Config) when is_list (Filename), is_list (Description) ->
    PrivateFile = priv_dir (Config) ++ "/" ++ Filename,
        io:fwrite ("<a href=\"~s\">" ++ Description ++ "</a>", [PrivateFile]),
        PrivateFile.


%%%====================================================================
%%% Internal functions
%%%====================================================================

% A function used to manage a the starting of a node.
shell (NodeName, ShellCommand) ->
    process_flag (trap_exit, true),
    Port = open_port ({spawn, ShellCommand}, [stream, in, eof]),
    io:fwrite ("fs_test_lib:shell command: ~s~n", [ShellCommand]),
    manage_port (NodeName, Port).

%% Translate the port to a node name for book keeping purposes.
%% Nice technique credit to Eric Newhuis.
manage_port (NodeName, Port) ->
    LongNodeName = create_long_node_name (NodeName),
    case poll_until (fun () -> pong == net_adm:ping (LongNodeName) end, 40, 250) of
	true -> ok;
	false -> exit ({node_down, {cant_ping, NodeName}})
    end,
    case catch monitor_node (LongNodeName, true) of
        {'EXIT', Reason} -> exit ({node_down, {Reason, {monitor_failed, NodeName}}});
        _                -> watch_node_and_port (NodeName, LongNodeName, Port)
    end.
                
watch_node_and_port (NodeName, LongNodeName, Port) ->
    receive
	{Port, {data, Bytes}} ->
	    error_logger:info_report (atom_to_list (NodeName) ++ ": " ++ Bytes),
	    watch_node_and_port (NodeName, LongNodeName, Port);

        {nodedown, LongNodeName} ->
            port_close (Port),
            exit ({node_down, {monitor_trap, NodeName}});

	{Port, eof} ->
            watch_node_and_port (NodeName, LongNodeName, Port);

        {'EXIT', Port, Reason} -> 
            exit ({node_down, {regular_exit, NodeName}});

        {'EXIT', Other, Reason} -> 
            io:format ("fs_test_lib:manage_port ~p had exited with reason ~p~n", [Other, Reason]),
	    watch_node_and_port (NodeName, LongNodeName, Port)

    end.

create_long_node_name (NodeName) ->
    list_to_atom (atom_to_list (NodeName) ++ "@" ++  net_adm:localhost ()).

%% Returns atoms representing node names of debug targets.
%% Debug targets want interactive graphical debuggers started.
%% Returns: [atom()]
debug_targets () ->
    case os:getenv ("DEBUG") of
	false        -> [];
	DebugTargets -> string:tokens (DebugTargets, " ,:;\t\n")
    end.

%% Extracts a string representation of the location
%% of the test server "priv dir" from the test server Config.
priv_dir (Config) ->
    
        {value, {_, PrivDir}} = lists:keysearch (priv_dir, 1, Config),
            PrivDir.


