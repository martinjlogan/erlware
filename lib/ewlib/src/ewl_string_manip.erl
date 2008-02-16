%%%-------------------------------------------------------------------
%%% File    : ewl_string_manip.erl
%%% Author  : Martin J. Logan 
%%%
%%% @doc
%%%  Functions to manipulate strings.
%%% @end
%%%-------------------------------------------------------------------
-module(ewl_string_manip).

%%--------------------------------------------------------------------
%% Include files 
%%--------------------------------------------------------------------
-include("eunit.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         n_tokens/3, 
         separate_by_token/2,
         separate_by_token/3,
         is_string/1 
        ]).

%%====================================================================
%% External functions
%%====================================================================

%%------------------------------------------------------------------------------
%% @doc Grabs n number of words from a string.
%% <pre>
%% Tokens is a list of charachters i.e " |@". This function traverses a
%% string accumulating words delimeted by one of the tokens for
%% a number of iterations specified by index.
%%
%% Example:
%%  n_tokens("app-1.2.3-alpha", 1, "-") -> {ok, {["app"], "1.2.3-alpha"}}
%%
%% Expects:
%%  String - The string to be separated.
%%  Index - The number of words to grab.
%%  Tokens - A list of tokens to be separated on.
%%
%% Types:
%%  String = string()
%%  Index - integer()
%%  Tokens = list()
%% </pre>
%% @spec n_tokens(String, Index, Tokens) -> {ok, {[Token], Rest}}
%% @end
%%------------------------------------------------------------------------------
n_tokens(String, Index, Tokens) ->
    n_tokens(String, Index, Tokens, []).
                                                                                   
n_tokens(String, 0, _Tokens, Tokenized) ->
    {ok, {lists:reverse(Tokenized), String}};
n_tokens(String, Index, Tokens, Tokenized) ->
    case separate_by_token(String, Tokens) of
        {ok, {[], Rest}} ->
            {ok, {Tokenized, Rest}};
        {ok, {NewTokenized, Rest}} ->
            n_tokens(Rest, Index -1, Tokens, [NewTokenized|Tokenized])
    end.
                                                                                   

%%-----------------------------------------------------------------------------
%% @doc Grabs the first word from a string that is delimeted by a Token.
%% <pre>
%% Tokens is a list of charachters i.e " |@". This function traverses a
%% string accumulating elements until it reaches one of the tokens.
%% Upon reaching a token it returns the accumulated elements and
%% the rest of the string.
%%
%% <pre>
%%  separate_by_token("foo-bar", "-") -> {ok, {"foo", "bar"}}
%%  separate_by_token("foo-bar", "-", true) -> {ok, {"foo", "-bar"}}
%% </pre>
%%
%% Expects:
%%  String - The string to be separated.
%%  Tokens - A list of tokens to be separated on.
%%  KeepToken - indicates whether the string Rest returned with or without the Token.
%%
%% Types:
%%  String = string()
%%  Tokens = list()
%% </pre>
%% @spec separate_by_token(String, Tokens, KeepToken::bool()) -> {ok, {Token, Rest}}
%% @end
%%-----------------------------------------------------------------------------
separate_by_token(String, Tokens, KeepToken) ->
    separate_by_token(String, Tokens, [], KeepToken).
                                                                                   
%% @spec separate_by_token(String, Tokens) -> {ok, {Token, Rest}}
%% @equiv separate_by_token(String, Tokens, false)
separate_by_token(String, Tokens) ->
    separate_by_token(String, Tokens, false).

separate_by_token([H|T], Tokens, Word, KeepToken) ->
    case is_token(H, Tokens) of
        true when KeepToken == false -> {ok, {lists:reverse(Word), T}};
        true                         -> {ok, {lists:reverse(Word), [H|T]}};
        false                        -> separate_by_token(T, Tokens, [H|Word], KeepToken)
    end;
separate_by_token([], _Tokens, Word, _KeepToken) ->
    {ok, {[], lists:reverse(Word)}}.
                                                                                   

%%----------------------------------------------------------------------------
%% @doc Checks to see if a list is a string.
%% @spec is_string(List) -> bool()
%% @end
%%----------------------------------------------------------------------------
is_string([])   -> false;
is_string(Term) -> lists:any(fun(Element) -> string_p1(Element) end, Term).
                                                                                   
string_p1(H) when integer(H), H >= $\s, H < 255 -> true;
string_p1($\n)                                  -> true;
string_p1($\r)                                  -> true;
string_p1($\t)                                  -> true;
string_p1($\v)                                  -> true;
string_p1($\b)                                  -> true;
string_p1($\f)                                  -> true;
string_p1($\e)                                  -> true;
string_p1(_)                                    -> false.

%%====================================================================
%% Internal functions
%%====================================================================

is_token(Target, [Target|_]) -> true;
is_token(Target, [_|T])      -> is_token(Target, T);
is_token(_Target, [])        -> false.

%%====================================================================
%% Test functions
%%====================================================================
separate_by_token_test() ->
    ?assertMatch({ok, {"foo", "bar"}}, separate_by_token("foo-bar", "-")),
    ?assertMatch({ok, {"foo", "-bar"}}, separate_by_token("foo-bar", "-", true)).

n_tokens_test() ->
    ?assertMatch({ok, {["app"], "1.2.3-alpha"}},  n_tokens("app-1.2.3-alpha", 1, "-")).

