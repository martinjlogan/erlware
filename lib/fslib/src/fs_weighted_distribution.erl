%%% $Id: fs_weighted_distribution.erl,v 1.4 2003/09/05 19:54:22 mlogan Exp $
%%%-------------------------------------------------------------------
%%% File    : fs_weighted_distribution.erl
%%% Author  : Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%
%%% @doc  Reads tuples consisting of weight, term() from a file. These term()s are distributed
%%% according to the weight associated with them in a random fashion. 
%%% <pre>
%%% File Format:
%%%  {integer(), term()}.
%%% Example:
%%%  {30, {martin, [1,2,3]}.
%%%  {50, {eric, [4]}.
%%%  {70, {jeff, []}.
%%%  {10, {tom, [1,2,3,4,5]}.
%%% </pre> 
%%% @end
%%%
%%% Created :  4 Sep 2003 by Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%-------------------------------------------------------------------
-module(fs_weighted_distribution).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         output_term/1,
         read_file/1
        ]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------
-record(state, {terms = undefined, range = undefined}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Read the file specified by FileName.
%% <pre>
%% Types:
%%  FileName = string()
%%  State = wd()
%% </pre>
%% @spec read_file(FileName) -> State | exit(Reason)
%% @end
%%--------------------------------------------------------------------
read_file(FileName) -> 
    {ok, {Range, Terms}} = store_file(FileName),
    #state{terms = Terms, range = Range}.

%%--------------------------------------------------------------------
%% @doc Returns a term.
%% <pre>
%% Types:
%%  State = wd()
%% </pre>
%% @spec output_term(State) -> term()
%% @end
%%--------------------------------------------------------------------
output_term(State) -> 
    select_term(random:uniform(State#state.range), State#state.terms).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Takes a list of terms of the format {integer(), term()} and returns one where the frequencies are sumed in order.
%% Types:
%%  Range = integer()
%%  TermList = [Term]
%%   Terms = {integer(), term()}
%%
%% Returns: 
%%  {ok, {Range, TermList}}
%%
%% Example:
%%  [{50, martin}, {50, eric}] returns: {ok, {100, [{50, martin}, {100, eric}]}}
store_file(FileName) ->
    case file:consult(FileName) of
        {ok, TermList}  -> {ok, parse_terms(TermList)};
        {error, Reason} -> {error, Reason}
    end.

parse_terms(TermList) -> 
    ParsedTermList = parse_terms(TermList, []),
    {element(1, hd(ParsedTermList)), lists:reverse(ParsedTermList)}.

parse_terms([{Frequency, Term}|T], []) when is_integer(Frequency) -> 
    parse_terms(T, [{Frequency, Term}]);
parse_terms([{Frequency, Term}|T], [{LF, _}|TA] = Acc) when is_integer(Frequency) -> 
    parse_terms(T, [{Frequency + LF, Term}|Acc]);
parse_terms([], Acc) -> 
    Acc;
parse_terms(_, Acc) -> 
    {error, format}.


% Select the term whose range limit is the closest greater number than the integer passed in. 
select_term(Integer, [{Range, Term}|T]) when Range >= Integer -> Term;
select_term(Integer, [H|T])                                   -> select_term(Integer, T).
