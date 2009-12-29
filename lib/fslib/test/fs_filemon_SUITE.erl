%%%-------------------------------------------------------------------
%%% File    : galaxy_filemon_test.erl
%%%
%%% @doc  Test for fs_filemon.erl
%%% @end
%%%-------------------------------------------------------------------

-module (fs_filemon_SUITE).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include ("macros.hrl").
-include ("kernel/include/file.hrl").

%%--------------------------------------------------------------------
%% Macro Definitions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Test Suite Exports
%%--------------------------------------------------------------------

-export ([
	  all/1,
	  init_per_testcase/2,
	  fin_per_testcase/2
	 ]).

%%--------------------------------------------------------------------
%% Export All Test Cases
%%--------------------------------------------------------------------

-export ([
	  test_filemon/1
         ]).

%% "Internal" Exports

-export ([
	  file_mod/3
	 ]).

%%====================================================================
%% Test Server Functions
%%====================================================================

all (doc)   -> ["fs_lists tests"];
all (suite) -> [
		test_filemon
	       ].

init_per_testcase (Case, Config) ->
    Config.

fin_per_testcase (Case, Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_filemon (doc)   -> [];
test_filemon (suite) -> [];
test_filemon (Config) when list (Config) ->
    error_logger:tty (false),

    TestFile1 = "test1.txt",
    TestFile2 = "test2.txt",

    create_file (TestFile1),
    create_file (TestFile2),

    {ok, Pid} = fs_filemon:start_link ([], 100),

    ok = fs_filemon:add_subscription (Pid, TestFile1, {?MODULE, file_mod, [self ()]}),
    ok = fs_filemon:add_subscription (Pid, TestFile2, {?MODULE, file_mod, [self ()]}),

    modify_file (TestFile1),
    modify_file (TestFile1),
    modify_file (TestFile1),
    modify_file (TestFile1),
    modify_file (TestFile1),
    modify_file (TestFile1),
    expect_notification (TestFile1),

    modify_file (TestFile2),
    expect_notification (TestFile2),

    fs_filemon:stop (Pid),
    file:delete (TestFile1),
    file:delete (TestFile2),
    ok.
    
%%====================================================================
%% Internal functions
%%====================================================================

file_mod (FileName, FileInfo, State) ->
    case State of
	[Pid] when pid (Pid) ->
	    Pid ! {file_mod, FileName, FileInfo};
	Error ->
	    (?FAIL (Error))
    end,
    {ok, State}.

expect_notification (FileName) ->
    receive
	{file_mod, FileName, {modtime, _}} ->
	    ok;

	Error ->
	    (?FAIL (Error))
    after
	2000 ->
	    (?FAIL (timeout))
    end.

create_file (FileName) ->
    {ok, IODevice} = file:open (FileName, [write]),
    file:write (IODevice, "created\n"),
    file:close (IODevice),
    %% Fake the last mod time so we can run the test quickly.
    %% Otherwise we'd have to actually wait at least one second and that is far
    %% too long for us impatient developers to wait.
    {ok, FileInfo} = file:read_file_info (FileName),
    {{Y,M,D},Time} = FileInfo#file_info.mtime,
    ModifiedTime = {{Y - 1, M, D}, Time},
    ok = file:write_file_info (FileName, FileInfo#file_info{mtime = ModifiedTime}).

modify_file (FileName) ->
    {ok, IODevice} = file:open (FileName, [append]),
    file:write (IODevice, "modified\n"),
    file:close (IODevice).
