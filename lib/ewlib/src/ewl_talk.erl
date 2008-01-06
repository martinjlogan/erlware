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
%%% @author Eric Merritt
%%% @doc
%%%  Provides the ability to ask questions of the user and 
%%%  get a response.
%%% @end
%%% @copyright 2006 
%%%---------------------------------------------------------------------------
-module(ewl_talk).

%% API
-export([ask/1, ask/2, ask/4, say/1, say/2]).

-include("eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------
%% @spec say(Str::string()) -> ok
%% @doc
%%  Outputs the line to the screen
%% @end
%%-------------------------------------------------------------------
say(Say) ->
    error_logger:info_msg(lists:flatten([Say, "~n"])).

say(Say, Args) when is_list(Args) ->
    error_logger:info_msg(lists:flatten([Say, "~n"]), Args);
say(Say, Args) ->
    error_logger:info_msg(lists:flatten([Say, "~n"]), [Args]).

%%-------------------------------------------------------------------
%% @spec ask(Str::string()) -> Resp::string()
%% @doc
%%  Asks the user for a response to the specified prompt.
%% @end
%%-------------------------------------------------------------------
ask(Prompt) ->
    io:format(lists:flatten(Prompt)),
    Line = io:get_line('? '),
    case Line of
        eof ->
            io:format("I didn't catch that!~n"),
            ask(Prompt);
        _ ->
            string:strip(string:strip(Line), both, $\n)
    end.

%%-------------------------------------------------------------------
%% @spec ask(Str::string(), Type::atom()) -> Resp
%% @doc
%%  Asks the user to respond to the prompt. Trys to return the value
%%  in the format specified by 'Type'.
%% @end
%%-------------------------------------------------------------------
ask(Prompt, boolean) ->
    ask(Prompt, fun get_boolean/1, boolean);
ask(Prompt, number) ->
    ask(Prompt, fun get_integer/1, number).

%%-------------------------------------------------------------------
%% @spec ask(Str::string(), Type::atom, Min::number(), Max::number()) -> Resp
%% @doc
%%  Asks the user to respond to the number prompt with a value between 
%%  min and max.
%% @end
%%-------------------------------------------------------------------
ask(Prompt, number, Min, Max) ->
    Res = ask(Prompt, fun get_integer/1, number),
    case (Res >= Min andalso Res =< Max) of
        true ->
            Res;
        false ->
            say(["Your answer must be between ~w and ~w!"], [Min, Max]),
            ask(Prompt, number, Min, Max)
    end.
            
%%====================================================================
%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @spec ask(Str::string(), TransFun::atom(), Type::atom) -> Resp
%% @doc
%%  Actually does the work of asking, checking result and translating
%% result into the requested format.
%% @end
%% @private
%%-------------------------------------------------------------------
ask(Prompt, TransFun, Type) ->
    Ret = TransFun(ask(Prompt)),
    case Ret of
        no_clue ->
            io:format(["I didn't get that. This ", Type, 
                       " kind of question.~n"]),
            ask(Prompt, Type);
        _ ->
            Ret
    end.

%%-------------------------------------------------------------------
%% @spec get_boolean(Str::string()) -> true | false | no_clue
%% @doc
%%  Trys to translate the result into a boolean
%% @end
%% @private
%%-------------------------------------------------------------------
get_boolean([$T | _]) ->
    true;
get_boolean([$t | _]) ->
    true;
get_boolean("ok") ->
    true;
get_boolean("OK") ->
    true;
get_boolean([$Y | _]) ->
    true;
get_boolean([$y | _]) ->
    true;
get_boolean([$f | _]) ->
    false;
get_boolean([$F | _]) ->
    false;
get_boolean([$n | _]) ->
    false;
get_boolean([$N | _]) ->
    false;
get_boolean(_) ->
    no_clue.

%%-------------------------------------------------------------------
%% @spec get_integer(Str::string()) -> Number::integer() | no_clue
%% @doc
%%  Trys to translate the result into an integer
%% @end
%% @private
%%-------------------------------------------------------------------
get_integer(String) ->
    case (catch list_to_integer(String)) of
        {'Exit', _} ->
            no_clue;
        Integer ->
            Integer
    end.



%%%====================================================================
%%% tests
%%%====================================================================
general_test_() ->
    [?_test(42 == get_integer("42")),
     ?_test(true == get_boolean("true")),
     ?_test(false == get_boolean("false")),
     ?_test(true == get_boolean("Ok")),
     ?_test(true == get_boolean("ok")),
     ?_test(true == get_boolean("Y")),
     ?_test(true == get_boolean("y")),
     ?_test(false == get_boolean("False")),
     ?_test(false == get_boolean("No")),
     ?_test(false == get_boolean("no"))].
