%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  This module contains the gas transform syntax functions.
%%% @end
%%% Created : 20 Jan 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gas_transform).

%% API
-export([transform_term/2]).

-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Transform a term into another term. Will throw an exception on failure.
%% Example: Term ={hello, dolly} 
%%          TransformationSpec = "{H, D}:{ok, {D, H}}
%%          Output = {ok, {dolly, hello}}
%% Tranformation specs are separated into two parts separated with a colon.
%% The first part is the match and bind where as the second part is the
%% ouput using what has been bound.
%% @spec transform_term(Term, TransformationSpec) -> term()
%% @end
%%--------------------------------------------------------------------
transform_term(Terms, Spec)  ->
    error_logger:info_msg("gas_transform:transform_term(~p, ~p)~n", [Terms, Spec]),
    [InSpec, OutSpec] = string:tokens(Spec, ":"),
    Dict = extract_terms(Terms, InSpec),
    build_output_term(Dict, OutSpec).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc take an outspec and produce a term corresponding to it.
%%      bindings.
%% Example: build_output_term(dict(), "[Var, {Var2}]") -> [a, {b}] if Var in the dict is a and Var2 is b
%% @spec (VarStore, Spec) -> {Var, RestOfSpec} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
build_output_term(VarStore, [H|_] = RawSpec) when H == ${ ->
    Spec = pull_struct_markers(RawSpec),
    list_to_tuple(pull_from_spec(VarStore, Spec));
build_output_term(VarStore, [H|_] = RawSpec) when H == $[ ->
    Spec = pull_struct_markers(RawSpec),
    pull_from_spec(VarStore, Spec);
build_output_term(VarStore, Spec) ->
    dict:fetch(Spec, VarStore).

pull_from_spec(_VarStore, "") ->
    [];
pull_from_spec(VarStore, Spec) when is_list(Spec) ->
    case pull_from_spec(Spec) of
	{error, Reason} ->
	    {error, Reason};
	{struct, {Struct, RestOfSpec}} ->
	    [build_output_term(VarStore, Struct)|pull_from_spec(VarStore, RestOfSpec)];
	{var, {Var, RestOfSpec}} ->
	    case dict:find(Var, VarStore) of
		error ->
		    [convert_string_to_terms(Var)|pull_from_spec(VarStore, RestOfSpec)];
		{ok, Value} ->
		    [Value|pull_from_spec(VarStore, RestOfSpec)]
	    end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc take an inspec and produce a dictionary containing the correct
%%      bindings.
%% Example: extract_terms({a, b}, "{Var, Var2}") -> will bind Var to a and Var2 to b in the dictionary.
%% @spec (Terms, InSpec) -> {Var, RestOfSpec} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
extract_terms(Terms, InSpec) ->
    extract_terms(Terms, InSpec, dict:new()).

extract_terms(Term, [H|_] = Spec, VarStore) when H == ${; H == $[ ->
    {Struct, []} = pull_struct(Spec), % Sanity check, assertion
    case is_tuple(Term) of
	true  -> process_list(tuple_to_list(Term), Struct, VarStore);
	false -> process_list(Term, Struct, VarStore)
    end;
extract_terms(Term, Spec, VarStore) when is_list(Spec) ->
    process_single(Term, Spec, VarStore).

process_single(Term, Var, VarStore) when is_list(Var) ->
    case pull_from_spec(Var) of
	{error, Reason} ->
	    {error, Reason};
	{var, {Var, ""}} ->
	    dict:store(Var, Term, VarStore);
	_Badmatch ->
	    %error_logger:info_msg("process_single/3 badmatch ~p~n", [_Badmatch]),
	    {error, badmatch}
    end.

process_list(Tuple, Spec, VarStore) when is_tuple(Tuple) ->
    process_list(tuple_to_list(Tuple), Spec, VarStore);
process_list([H|T], RawSpec, VarStore) ->
    case pull_from_spec(pull_struct_markers(RawSpec)) of
	{error, Reason} ->
	    {error, Reason};
	{var, {Var, RestOfSpec}} ->
	    %error_logger:info_msg("process_list/3 resulted in ~p~n", [{var, {Var, RestOfSpec}}]),
	    process_list(T, RestOfSpec, dict:store(Var, H, VarStore));
	{struct, {Struct, RestOfSpec}} ->
	    error_logger:info_msg("process_list/3 resulted in ~p on term ~p~n", [{struct, {Struct, RestOfSpec}}, H]),
	    process_list(T, RestOfSpec, extract_terms(H, Struct, VarStore))
    end;
process_list([], "", VarStore) ->
    VarStore;
process_list(_, _, _) ->
    %error_logger:info_msg("process_list/3 badmatch~n"),
    {error, badmatch}.

%%--------------------------------------------------------------------
%% @private
%% @doc pull an element from a spec.
%% Example: pull_from_spec("{Var, Var2}") -> {"Var", "Var2"}
%% @spec (Spec) -> {Var, RestOfSpec} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
pull_from_spec(Spec) when is_list(Spec) ->
    case pull_var(Spec) of
	{error, Reason} ->
	    case pull_struct(Spec) of
		{error, Reason}      -> {error, bad_spec};
		{Struct, RestOfSpec} -> {struct, {Struct, RestOfSpec}}
	    end;
	{Var, RestOfSpec} ->
	    {var, {Var, RestOfSpec}}
    end.
    

%%--------------------------------------------------------------------
%% @private
%% @doc pull the struct markers off as in "{a, b}" -> "a, b"
%% @spec (StructString) -> string()
%% @end
%%--------------------------------------------------------------------
pull_struct_markers([H|StructString]) when H == $[; H == ${ ->
    [_|T] = lists:reverse(StructString),
    lists:reverse(T);
pull_struct_markers(NotStruct) ->
    NotStruct.

%%--------------------------------------------------------------------
%% @private
%% @doc pull a structure like [a, b] or {c, d} from a spec string.
%% @spec (Spec) -> {StructList, RestOfSpec} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
pull_struct(Spec) when is_list(Spec) ->
    %error_logger:info_msg("pull_struct/1 pulling from ~p~n", [Spec]),
    Struct = pull_struct(Spec, [], 0),
    %error_logger:info_msg("pull_struct/1 resulted in ~p~n", [Struct]),
    Struct.

pull_struct([H|T], Acc, 0) when H == $[; H == ${ ->
    pull_struct_body(T, [H|Acc], 1);
pull_struct(Spec, _Acc, 0) ->
    {error, {no_struct, Spec}}.


pull_struct_body([H|T], Acc, Count) when H == $[; H == ${ ->
    pull_struct_body(T, [H|Acc], Count + 1);
pull_struct_body([H|T], Acc, 1) when H == $]; H == $} ->
    {lists:reverse([H|Acc]), tear_comma_space(T)};
pull_struct_body([H|T], Acc, Count) when H == $]; H == $} ->
    pull_struct_body(T, [H|Acc], Count - 1);
pull_struct_body([H|T], Acc, Count) ->
    pull_struct_body(T, [H|Acc], Count).

%%--------------------------------------------------------------------
%% @private
%% @doc pull a variable like _Hello, or _ or Var1 from a spec string.
%% @spec (Spec) -> {Var, RestOfSpec} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
pull_var(Spec) when is_list(Spec) ->
    %error_logger:info_msg("pull_var/1 pulling from ~p~n", [Spec]),
    Var = pull_var(Spec, []),
    %error_logger:info_msg("pull_var/1 resulted in ~p~n", [Var]),
    Var.

pull_var([H|T], Acc) when H == $"; H == $\ ->
    pull_var_body(T, [H|Acc]);
pull_var([H|T], Acc) when H == $_ ->
    pull_var_body(T, [H|Acc]);
pull_var([H|T], Acc) when H >= $a, H =< $z ->
    pull_var_body(T, [H|Acc]);
pull_var([H|T], Acc) when H >= $A, H =< $Z ->
    pull_var_body(T, [H|Acc]);
pull_var(Spec, _Acc) ->
    {error, {no_var, Spec}}.

pull_var_body([H|T], Acc) when H == $_; H == $"; H == $\ ->
    pull_var_body(T, [H|Acc]);
pull_var_body([H|T], Acc) when H >= $0, H =< $9 ->
    pull_var_body(T, [H|Acc]);
pull_var_body([H|T], Acc) when H >= $A, H =< $Z ->
    pull_var_body(T, [H|Acc]);
pull_var_body([H|T], Acc) when H >= $a, H =< $z ->
    pull_var_body(T, [H|Acc]);
pull_var_body([H|_] = Spec, Acc) when H == $,; $  ->
    {lists:reverse(Acc), tear_comma_space(Spec)};
pull_var_body([], Acc) ->
    {lists:reverse(Acc), []}.
    
tear_comma_space([$,|T]) ->
    tear_comma_space(T);
tear_comma_space([$ |T]) ->
    tear_comma_space(T);
tear_comma_space(Str) ->
    Str.

%%%===================================================================
%%% Internal functions
%%%===================================================================
convert_string_to_terms(ArgString) ->
    error_logger:info_msg("arg string ~p~n", [ArgString]),
    case regexp:match(ArgString, "^[0-9]+\.[0-9]+$") of
	{match, _, _} -> 
	    ArgString;
	_  ->
	    ScanableArg = ArgString ++ ". ", 
	    error_logger:info_msg("scanning and parsing ~p~n", [ScanableArg]),
	    {ok, Toks, _Line} = erl_scan:string(ScanableArg, 1),
	    case catch erl_parse:parse_term(Toks) of
		{ok, Term} -> 
		    Term;
		Error ->
                    convert_string_to_terms(special_case(Error, ArgString))
	    end
    end.

special_case(Msg, ArgString) ->
    error_logger:info_msg("Special case discovered for ~p converting input arg to string and reprocessing~n", [Msg]),
    "\"" ++ ArgString ++ "\"".

%%%===================================================================
%%% Test functions
%%%===================================================================
pull_var_test() ->
    ?assertMatch({error, {no_var, "{hello}"}}, pull_var("{hello}")),
    ?assertMatch({"Hello", []},          pull_var("Hello")),
    ?assertMatch({"hello_hello", []},    pull_var("hello_hello")),
    ?assertMatch({"_Hello", []},         pull_var("_Hello")),
    ?assertMatch({"Hello9", []},         pull_var("Hello9")),
    ?assertMatch({"hello", []},          pull_var("hello")),
    ?assertMatch({"hello", "blah}"},   pull_var("hello, blah}")).

pull_struct_test() ->
    ?assertMatch({error, {no_struct, "hello"}}, pull_struct("hello")),
    ?assertMatch({"{hello, blah}", []},   pull_struct("{hello, blah}")),
    ?assertMatch({"{hello, {blah}}", []},   pull_struct("{hello, {blah}}")),
    ?assertMatch({"{hello, [blah]}", []},   pull_struct("{hello, [blah]}")),
    ?assertMatch({"{hello, [blah, {blay}]}", []},   pull_struct("{hello, [blah, {blay}]}")).

extract_terms_test() ->
    ?assertMatch([{"D", dolly}], dict:to_list(extract_terms({dolly}, "{D}"))),
    ?assertMatch([{"Foo", foo}, {"Bar", [a,b]}], dict:to_list(extract_terms([foo, [a, b]], "[Foo, Bar]"))),
    ?assertMatch([{"Hello", hello}], dict:to_list(extract_terms(hello, "Hello"))),
    ?assertMatch([{"Hello", []}],    dict:to_list(extract_terms([], "Hello"))),
    ?assertMatch([{"Hello", {}}],    dict:to_list(extract_terms({}, "Hello"))).

transform_term_test() ->
    ?assertMatch(hello, transform_term(hello, "H:H")),

    ?assertMatch([hello], transform_term([hello], "H:H")),
    ?assertMatch({hello}, transform_term({hello}, "H:H")),
    ?assertMatch({hello}, transform_term([hello], "[H]:{H}")),
    ?assertMatch([hello], transform_term({hello}, "{H}:[H]")),

    ?assertMatch(["dolly", hello], transform_term({hello}, "{H}:[\"dolly\", H]")),
    ?assertMatch([dolly, hello], transform_term({hello}, "{H}:[dolly, H]")),
    ?assertMatch({ok, {dolly, hello}}, transform_term({hello}, "{H}:{ok, {dolly, H}}")),

    ?assertMatch([dolly, hello], transform_term({hello, dolly}, "{H, D}:[D, H]")),
    ?assertMatch({dolly, hello}, transform_term([hello, dolly], "[H, D]:{D, H}")),
    ?assertMatch({dolly, {hello}}, transform_term([{hello}, dolly], "[H, D]:{D, H}")),
    ?assertMatch({dolly, hello}, transform_term([{hello}, dolly], "[{H}, D]:{D, H}")),
    ?assertMatch({dolly, {hello}}, transform_term([{{hello}}, dolly], "[{H}, D]:{D, H}")).
    
