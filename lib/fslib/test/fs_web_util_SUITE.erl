%%% File    : fs_lists_test.erl
%%%
%%% @doc fs_web_util unit test.
%%% @end
%%%
%%%-------------------------------------------------------------------

-module (fs_web_util_SUITE).

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
	  test_single_replace/1,
	  test_block_replace/1,
	  test_parse_final/1
         ]).

%%====================================================================
%% Test Server Functions
%%====================================================================

all (doc)   -> ["fs_web_util tests"];
all (suite) -> [
	  	test_single_replace,
	  	test_block_replace,
		test_parse_final
	       ].

init_per_testcase (Case, Config) ->
    Config.

fin_per_testcase (Case, Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_single_replace(doc)   -> [];
test_single_replace(suite) -> [];
test_single_replace(Config) when list (Config) ->
    HTML = "<head>{VAR}</head>",
    {ok, "<head>Martin's Home Page</head>", 1} = fs_web_util:replace(HTML, "VAR", "Martin's Home Page"), 
    ok.

test_block_replace(doc)   -> [];
test_block_replace(suite) -> [];
test_block_replace(Config) when list (Config) ->
    HTML = "<hello>Title</hello><!-- BEGIN B1 -->{VAR}<!-- END B1 --><bye></bye>More good stuff<!-- BEGIN B2 -->{VAR}<!-- END B2 --></tail>",
    {ok, B} = fs_web_util:set_current_block(HTML, "B2"),
    {ok, B2, _} = fs_web_util:replace(B, "VAR", " 1-"),
    {ok, B3} = fs_web_util:set_current_block(B2, "B2"),
    {ok, B4, _} = fs_web_util:replace(B3, "VAR", "2-"),
    {ok, B5} = fs_web_util:set_current_block(B4, "B2"),
    {ok, B6, _} = fs_web_util:replace(B5, "VAR", "3"),
    {ok, NewHTML} = fs_web_util:parse_current_block(B6),
    NewHTML = "<hello>Title</hello><!-- BEGIN BLOCK -->{VAR}<!-- END BLOCK --><bye></bye>More good stuff 1-2-3</tail>",
    ok.

test_parse_final(doc)   -> [];
test_parse_final(suite) -> [];
test_parse_final(Config) when list (Config) ->
	HTML = "<hello>martin</hello><!-- BEGIN BLOCK -->{TEMPLATE}<!-- END BLOCK --><bye></bye>More good stuff<!-- BEGIN B2 -->{TEMPLATE2}<!-- END B2 --></tail>",
	{ok, NewHTML} = fs_web_util:parse_final(HTML),
	NewHTML = "<hello>martin</hello><bye></bye>More good stuff</tail>",
	ok.




