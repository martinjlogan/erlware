%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Provides a methods to parse a list of args. Will also parse a string.
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 21 Oct 2007 by Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(ewl_get_opt).

%% API
-export([parse_opts/2, parse_string_opts/2]).

-include("eunit.hrl").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec parse_opts(Args::list(), OptDesc::list()) -> {Res::assoc_list(), Rest}
%% @doc
%%  Parse the options into something more interesting.
%%  {ShortDesc, LongDesc, required|maybe|none}.
%% @end
%%--------------------------------------------------------------------
parse_opts(Args, OptDesc) when is_list(Args) ->
    parse_opts(Args, OptDesc, []).

%%--------------------------------------------------------------------
%% @spec parse_string_opts(Args::string(), OptDesc::list()) -> {Res::assoc_list(), Rest}
%%
%% @doc
%%  parse a string that represents an option list. "--arg1 some_arg -a" into
%% a list of options.
%% @end
%%--------------------------------------------------------------------
parse_string_opts(Args, OptDesc) ->
    parse_opts(parse_monolithic(Args, []), OptDesc).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec parse_opts(ArgList, OptDesc, Acc) -> {Res::assoc_list(), Rest}
%%
%% @doc
%%  Parse the options into something more interesting.
%%  {ShortDesc, LongDesc, required|optional|none}.
%% @end
%%--------------------------------------------------------------------
parse_opts([], _, Acc) ->
    {lists:reverse(Acc), []};
parse_opts([[$-, $-] | Tail], _, Acc) ->
    {lists:reverse(Acc), Tail};
parse_opts([[$-, $- | LongDesc] | Tail], OptDesc, Acc) ->
    {LongDesc, HasArgs} = long_desc(LongDesc, OptDesc),
    parse_opt_body(LongDesc, HasArgs, Tail, OptDesc, Acc);
parse_opts([[$- | ShortDesc] | Tail], OptDesc, Acc) ->
    {LongDesc, HasArgs} = short_desc(ShortDesc, OptDesc),
    parse_opt_body(LongDesc, HasArgs, Tail, OptDesc, Acc).


%%--------------------------------------------------------------------
%% @spec parse_opt_body(LongDesc, HasArgs, Tail, OptDesc, Acc) -> ok
%%
%% @doc
%%  This doesn't return. It tail calls parse_opts and parse_opts returns.
%% @end
%%--------------------------------------------------------------------
parse_opt_body(LongDesc, HasArgs, Tail, OptDesc, Acc) ->
    case HasArgs of
        required ->
            {NewTail, Arg} = get_arg(LongDesc, Tail, required),
            parse_opts(NewTail, OptDesc, [{LongDesc, Arg} | Acc]);
        optional ->
            {NewTail, Arg} = get_arg(LongDesc, Tail, optional),
            parse_opts(NewTail, OptDesc, [{LongDesc, Arg} | Acc]);
        _ ->
            parse_opts(Tail, OptDesc, [{LongDesc, true} | Acc])
    end.


%%--------------------------------------------------------------------
%% @spec get_arg(LongDesc::string(), OptList::list(),  Type::atom()) -> {Rest::list(), Arg}
%%
%% @doc
%%  Got the argument and the rest of the args.
%% @end
%%--------------------------------------------------------------------
get_arg(_, Opts = [[$- | _] | _], optional) ->
    {Opts, true};
get_arg(LongDesc, [[$- | _] | _], required) ->
    throw({args_required, LongDesc});
get_arg(_, [Arg | Rest], _) ->
    {Rest, Arg};
get_arg(LongDesc, [], required) ->
    throw({args_required, LongDesc});
get_arg(_, [], optional) ->
    {[], true}.


%%--------------------------------------------------------------------
%% @spec short_desc(ShortDesc::string(), OptDesc::list()) -> {LongDesc::string(), ArgumentReq::atom()}
%%
%% @doc
%%  get the longdesc and requriedargument field by the short desc.
%% @end
%%--------------------------------------------------------------------
short_desc(ShortDesc, [{ShortDesc, LongDesc, Bool} | _]) ->
    {LongDesc, Bool};
short_desc(ShortDesc, [_ | Tail]) ->
    short_desc(ShortDesc, Tail);
short_desc(ShortDesc, []) ->
    throw({unknown_argument, ShortDesc}).

%%--------------------------------------------------------------------
%% @spec long_desc(LongDesc::string, OptDesc::list) -> {LongDesc::string(), ArgumentReq::atom()}
%%
%% @doc
%%  Get the longdesc and requiredargument field by the long desc.
%% @end
%%--------------------------------------------------------------------
long_desc(LongDesc, [{_, LongDesc, Bool} | _]) ->
    {LongDesc, Bool};
long_desc(LongDesc, [_ | Tail]) ->
    long_desc(LongDesc, Tail);
long_desc(LongDesc, []) ->
    throw({unknown_argument, LongDesc}).

%%--------------------------------------------------------------------
%% @spec parse_monolithic(Args::string, Acc::list) -> Args::list()
%%
%% @doc
%%  Given a string parse out the args.
%% @end
%%--------------------------------------------------------------------
parse_monolithic([$\" | Tail], Acc) ->
    {NewTail, NewAcc} = parse_string($\", Tail, []),
    parse_monolithic(NewTail, [NewAcc | Acc]);
parse_monolithic([$\' | Tail], Acc) ->
    {NewTail, NewAcc} = parse_string($\', Tail, []),
    parse_monolithic(NewTail,[NewAcc | Acc]);
parse_monolithic(All = [_ | _], Acc) ->
    {NewTail, NewAcc} = parse_next_arg(eat_space(All), []),
    parse_monolithic(NewTail, [NewAcc | Acc]);
parse_monolithic([], Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @spec parse_string(Char::char(), StringBody::string(), Acc::list) -> {Rest::string(), This::string}
%%
%% @doc
%%  parse a string delimited by Char
%% @end
%%--------------------------------------------------------------------
parse_string(Char, [$\\, $r | Tail], Acc) ->
    parse_string(Char, Tail, [$\r | Acc]);
parse_string(Char, [$\\, $n | Tail], Acc) ->
    parse_string(Char, Tail, [$\n | Acc]);
parse_string(Char, [$\\, Char | Tail], Acc) ->
    parse_string(Char, Tail, [Char | Acc]);
parse_string(Char, [Char | Tail], Acc) ->
    {Tail, lists:reverse(Acc)};
parse_string(Char, [Val | Tail], Acc) ->
    parse_string(Char, Tail, [Val | Acc]);
parse_string(_, [], _) ->
    throw(unterminated_string).

%%--------------------------------------------------------------------
%% @spec parse_next_arg(ArgBody::string(), Acc::list()) -> {Rest::string(), Arg}
%%
%% @doc
%%  parse the next arg, basically parse to the next space.
%% @end
%%--------------------------------------------------------------------
parse_next_arg([$\n | Tail], Acc) ->
    {Tail, lists:reverse(Acc)};
parse_next_arg([$\r | Tail], Acc) ->
    {Tail, lists:reverse(Acc)};
parse_next_arg([$\t | Tail], Acc) ->
    {Tail, lists:reverse(Acc)};
parse_next_arg([$\  | Tail], Acc) ->
    {Tail, lists:reverse(Acc)};
parse_next_arg([Char | Tail], Acc) ->
    parse_next_arg(Tail, [Char | Acc]);
parse_next_arg([], Acc) ->
    {[], lists:reverse(Acc)}.


%%--------------------------------------------------------------------
%% @spec eat_space(Body::string()) -> Rest::string()
%%
%% @doc
%%  Eat any and all spaces until you get to a non-space char.
%% @end
%%--------------------------------------------------------------------
eat_space([$\n | Tail]) ->
    eat_space(Tail);
eat_space([$\r | Tail]) ->
    eat_space(Tail);
eat_space([$\t | Tail]) ->
    eat_space(Tail);
eat_space([$\  | Tail]) ->
    eat_space(Tail);
eat_space(Rest) ->
    Rest.

%%====================================================================
%% Tests
%%====================================================================
parse_opts_test() ->
    OptDesc = [{"b", "blah", required},
               {"z", "zag", optional},
               {"s", "slah", none}],
    {OptList, []}  =  parse_opts(["-b", "argu", "--zag", "-s"], OptDesc),
    {value, {"blah", "argu"}} = lists:keysearch("blah", 1, OptList),
    {value, {"zag", true}} = lists:keysearch("zag", 1, OptList),
    {value, {"slah", true}} = lists:keysearch("slah", 1, OptList).

parse_string_opts_0_test() ->
    OptDesc = [{"b", "blah", required},
               {"z", "zag", optional},
               {"s", "slah", none}],
    {OptList, []}  =  parse_string_opts("-b argu --zag -s", OptDesc),
    {value, {"blah", "argu"}} = lists:keysearch("blah", 1, OptList),
    {value, {"zag", true}} = lists:keysearch("zag", 1, OptList),
    {value, {"slah", true}} = lists:keysearch("slah", 1, OptList).

parse_string_opts_1_test() ->
    OptDesc = [{"b", "blah", required},
               {"z", "zag", optional},
               {"s", "slah", none}],
    {OptList, []}  =  parse_string_opts("-b 'argu hoodo' --zag -s", OptDesc),
    {value, {"blah", "argu hoodo"}} = lists:keysearch("blah", 1, OptList),
    {value, {"zag", true}} = lists:keysearch("zag", 1, OptList),
    {value, {"slah", true}} = lists:keysearch("slah", 1, OptList).

parse_string_opts_2_test() ->
    OptDesc = [{"b", "blah", required},
               {"z", "zag", optional},
               {"s", "slah", none}],
    {OptList, []}  =  parse_string_opts("-b \"argu hoodo\" --zag -s", OptDesc),
    {value, {"blah", "argu hoodo"}} = lists:keysearch("blah", 1, OptList),
    {value, {"zag", true}} = lists:keysearch("zag", 1, OptList),
    {value, {"slah", true}} = lists:keysearch("slah", 1, OptList).


