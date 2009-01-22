%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  This module contains the gas transform syntax functions.
%%% @end
%%% Created : 20 Jan 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gas_tranform).

%% API
-export([transform_term/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc transform a term into another term
%% @spec transform_term(Term, TransformationSpec) -> term()
%% @end
%%--------------------------------------------------------------------
transform_term(Tupel, [${|T]) when is_tuple(Tupel) ->
    extract_terms(Tupel, T).

extract_terms(Tupel, Spec) when is_tuple(Tupel) ->
    extract_terms(tuple_to_list(Tupel), Spec).
extract_terms([H|T], Spec) ->
    case pull_spec(Spec) of
	{Num



%%%===================================================================
%%% Internal functions
%%%===================================================================
