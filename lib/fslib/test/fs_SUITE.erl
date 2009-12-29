%%% File    : fs_SUITE.erl
%%%
%%% @doc
%%% @end
%%%
%%%-------------------------------------------------------------------

-module (fs_SUITE).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

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
	  fs_exit_handler_h_test/1,
	  fs_exit_handler_test/1,
	  fs_weighted_distribution_test/1,
	  fs_event_tracker_h_test/1,
	  fs_file_test/1
         ]).

%%====================================================================
%% Test Server Functions
%%====================================================================

all (doc)   -> ["FSLIB Tests"];
all (suite) -> [
		fs_exit_handler_h_test,
		%fs_exit_handler_test,
		fs_weighted_distribution_test,
		fs_event_tracker_h_test,
		fs_file_test
	       ].

init_per_testcase (Case, Config) ->
    Config.

fin_per_testcase (Case, Config) ->
    ok.

%%====================================================================
%% Individual Test Cases
%%====================================================================

-define (CASE (M), M (doc) -> []; M (suite) -> []; M (Config) when list (Config) -> M:all (Config)).

?CASE (fs_exit_handler_h_test).
?CASE (fs_exit_handler_test).
?CASE (fs_weighted_distribution_test).
?CASE (fs_event_tracker_h_test).
?CASE (fs_file_test).

%%====================================================================
%% Internal Functions
%%====================================================================
