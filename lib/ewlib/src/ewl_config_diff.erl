%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@sixfoe>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%   This module is for use in diffing config files. Config files are essentialy lists of two tuples or AVL's. 
%%%   
%%% @end
%%% Created : 16 Mar 2008 by Martin Logan <martinjlogan@sixfoe>
%%%-------------------------------------------------------------------
-module(ewl_config_diff).

%% API
-export([config_files/2, diff_avl/2]).

-include("eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Diffs two config files, basically diffs an AVL list down to the leaves. 
%%      The leaves are defined as values, as in {Key, Val}, which are not themselved AVL's.  
%% @spec config_files(AVL1, AVL2) -> DiffList
%% where
%%  DiffList = [Diff]
%%   Diff = term() | {added, term()} | {removed, term()}
%% @end
%%--------------------------------------------------------------------
config_files(FilePath1, FilePath2) ->
    {ok, [AVL1]} = file:consult(FilePath1),
    {ok, [AVL2]} = file:consult(FilePath2),
    diff_avl(AVL1, AVL2).

%%--------------------------------------------------------------------
%% @doc Diffs an AVL list down to the leaves. The leaves are defined as 
%%      values, as in {Key, Val}, which are not themselved AVL's.  
%% @spec diff_avl(AVL1, AVL2) -> DiffList
%% where
%%  DiffList = [Diff]
%%   Diff = term() | {added, term()} | {removed, term()}
%% @end
%%--------------------------------------------------------------------
diff_avl([{Key, Val}|T], AVL2) ->
    Value = case lists:keysearch(Key, 1, AVL2) of
		{value, {Key, Val}} ->
		    [];
		{value, {Key, Val2}} ->
		    case {is_avl(Val), is_avl(Val2)} of
			{true, true} -> [{Key, diff_avl(Val, Val2)}];
			{_, _}       -> [{added, {Key, Val}}, {removed, {Key, Val2}}]
		    end;
		false ->
		    [{added, {Key, Val}}]
	    end,
    lists:append(Value, diff_avl(T, lists:keydelete(Key, 1, AVL2)));
diff_avl([], AVL2) ->
    lists:map(fun(Term) -> {removed, Term} end, AVL2).
    

%%====================================================================
%% Internal functions
%%====================================================================

is_avl([H|T]) when is_tuple(H), size(H) == 2 ->
    is_avl(T);
is_avl([_|_]) ->
    false;
is_avl([]) ->
    true;
is_avl(_) ->
    false.

%%====================================================================
%% Test functions
%%====================================================================
diff_avl_test() ->
    Term1A = [{gas, [{tty, true}, {list, [{a,[a,b]}]}]}],
    Term1B = [{gas, [{tty, true}, {list, [{a,b}, {b,c}]}]}],
    Resp1  = [{gas,[{list,[{added,{a,[a,b]}},{removed,{a,b}},{removed,{b,c}}]}]}],
    ?assertMatch(Resp1, diff_avl(Term1A, Term1B)),

    Term2A = [{gas, [{tty, true}, {list, [{a,b}]}]}],
    Term2B = [{gas, [{tty, true}, {list, [{a,b}, {b,c}]}]}],
    Resp2  = [{gas,[{list,[{removed,{b,c}}]}]}],
    ?assertMatch(Resp2, diff_avl(Term2A, Term2B)).

