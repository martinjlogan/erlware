%%% File    : fs_lists_test.erl
%%%
%%% @doc fs_lists unit test.
%%% @end
%%%
%%%-------------------------------------------------------------------

-module (fs_lists_SUITE).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include ("macros.hrl").

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
	  test_do_until/1,
	  test_separate/1,
	  test_find_smallest/1,
	  test_find_all_smallest/1,
	  test_find_largest/1,
	  test_find_all_largest/1,
	  test_index_tokens/1,
	  test_flatten_term/1
         ]).

%%====================================================================
%% Test Server Functions
%%====================================================================

all (doc)   -> ["fs_lists tests"];
all (suite) -> [
		test_do_until,
		test_separate,
		test_find_smallest,
		test_find_all_smallest,
		test_find_largest,
		test_find_all_largest,
		test_index_tokens,
		test_flatten_term
	       ].

init_per_testcase (Case, Config) ->
    Config.

fin_per_testcase (Case, Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_do_until (doc)   -> [];
test_do_until (suite) -> [];
test_do_until (Config) when list (Config) ->
    Fun = fun (done)  -> ok;
	      (three) -> exit (do_until);
	      (_)     -> err
          end,
    ok = fs_lists:do_until (Fun, ok, [one, two, done, three]),
    ok.

test_separate (doc)   -> [];
test_separate (suite) -> [];
test_separate (Config) when list (Config) ->
    Fun = fun (a) -> true;
              (b) -> false
          end,
    {[a], [b]} = fs_lists:separate (Fun, [a, b]),
    ok.
                  
test_find_smallest (doc)   -> [];
test_find_smallest (suite) -> [];
test_find_smallest (Config) when list (Config) ->
    {1,1} = fs_lists:find_smallest ([{1,3},{1,2},{1,1},{1,5},{1,3}], 2),
    ok.

test_find_largest (doc)   -> [];
test_find_largest (suite) -> [];
test_find_largest (Config) when list (Config) ->
    {1,5} = fs_lists:find_largest ([{1,3},{1,2},{1,1},{1,5},{1,3}], 2),
    ok.

test_find_all_smallest (doc)   -> [];
test_find_all_smallest (suite) -> [];
test_find_all_smallest (Config) when list (Config) ->
    [{1,1}, {1,1}] = fs_lists:find_all_smallest ([{1,1},{1,2},{1,1},{1,5},{1,3}], 2),
    ok.

test_find_all_largest (doc)   -> [];
test_find_all_largest (suite) -> [];
test_find_all_largest (Config) when list (Config) ->
    [{1,5}, {1,5}] = fs_lists:find_all_largest ([{1,5},{1,2},{1,1},{1,5},{1,3}], 2),
    ok.

test_index_tokens (doc)   -> [];
test_index_tokens (suite) -> [];
test_index_tokens (Config) when list (Config) ->
    {ok, {["martin", "was"], Rest}} = fs_lists:index_tokens ("martin was,and is here", 2, " ,"),
    ok.
                  
test_flatten_term (doc)   -> [];
test_flatten_term (suite) -> [];
test_flatten_term (Config) when list (Config) ->
    % Test atom(), list(), integer(), tuple(), string()
    "{hello, [1, 2, \"3\"]}" = fs_lists:flatten_term ({hello, [1,2,"3"]}),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================








