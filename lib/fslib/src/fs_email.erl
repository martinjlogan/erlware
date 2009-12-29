%% Copyright (C) 1997, Ericsson Telecom AB
%% File:    fs_email.erl
%% Author:  Joe Armstrong (joe@cslab.ericsson.se)
%% Modified:  Martin Logan - 7/5/2001
%%
%% @doc
%% Send and receive Erlang terms by e-mail + other e-mail utilities.
%% This is specifically for use with UNIX and variants.
%% @end

-module(fs_email).
-vsn('1.0').

-export([test/1, start/0, internal/0]).

-export([send_async/1, send_async/4, send/1, send/3, send/4, send_term/4]).
-export([parse_email/1, parse_email_file/1, parse_mbox/1]).

-ifndef(SENDMAIL).
-define(SENDMAIL, os:find_executable("sendmail")).
-endif.

-import(lists, [reverse/1, reverse/2]).


%% new send function additions done 8/16/01 by Martin Logan
%% sagan was here: -t option in sendmail

%%-----------------------------------------------------------------------------
%% @doc Sends mail async.
%% <pre>
%%
%% Types:
%%  From = To = Subject = Data = string()
%%
%% </pre>
%% @spec send_async(From, To, Subject, Data) -> void()
%% @end
%%-----------------------------------------------------------------------------
send_async(From, To, Subject, Data) ->
    spawn(fs_email, send, [From, To, Subject, Data]).

%%-----------------------------------------------------------------------------
%% @doc Sends mail async from a source file.
%% <pre>
%%
%% Types:
%%  FileName = string()
%%
%% </pre>
%% @spec send_async(FileName) -> void()
%% @end
%%-----------------------------------------------------------------------------
send_async(FileName) ->
    spawn(fs_email, send, [FileName]).

%%-----------------------------------------------------------------------------
%% @doc Sends mail blocking until mail is sent.
%% <pre>
%%
%% Types:
%%  To = Subject = Data = string()
%%
%% </pre>
%% @spec send(To, Subject, Data) -> {ok, sent}
%% @end
%%-----------------------------------------------------------------------------
send(To, Subject, Data) ->
    {TmpFile, S} = open_tmp_file("/tmp", ".mail"),
    io:format(S, "To: <~s>~n", [To]),
    io:format(S, "Subject: ~s~n~n", [Subject]),
    io:format(S, "~s~n",[Data]),
    io:format(S, ".~nquit~n", []),
    file:close(S),
    io:format("sending .....~n", []),
    os:cmd(?SENDMAIL ++ " -t < " ++ TmpFile),
    file:delete(TmpFile),
    {ok, sent}.	

%%------------------------------------------------------------------------------
%% @doc Sends mail blocking until mail is sent.
%% <pre>
%%
%% Types:
%%  From = To = Subject = Data = string()
%%
%% </pre>
%% @spec send(From, To, Subject, Data) -> {ok, sent}
%% @end
%%------------------------------------------------------------------------------
send(From, To, Subject, Data) ->
    {TmpFile, S} = open_tmp_file("/tmp", ".mail"),
    io:format(S, "From: <~s>~n", [From]),
    io:format(S, "To: <~s>~n", [To]),
    io:format(S, "Subject: ~s~n~n", [Subject]),
    io:format(S, "~s~n",[Data]),
    io:format(S, ".~nquit~n", []),
    file:close(S),
    io:format("sending ....~n", []),
    os:cmd(?SENDMAIL ++ " -t < " ++ TmpFile),
    file:delete(TmpFile),
    {ok, sent}.

%%------------------------------------------------------------------------------
%% @doc Sends mail from a source file blocking until mail is sent.
%% <pre>
%%
%% Types:
%%  FileName = string()
%%
%% </pre>
%% @spec send(FileName) -> void()
%% @end
%%------------------------------------------------------------------------------
send(File) ->
	io:format("sending ...~n", []),
    	os:cmd(?SENDMAIL ++ " -t < " ++ File),
	{ok, sent}.

%%------------------------------------------------------------------------------
%% @doc Sends a term blocking until mail is sent.
%% @spec send_term(To, Subject, Module, Term) -> ok | exit()
%% @end
%%------------------------------------------------------------------------------
send_term(To, Subject, Module, Term) ->
    send(To, Subject ++ [$ |Module], term_to_uue(Term)).

%% @doc a test.
test(1) -> 
    send("joe","one line","A single line");
test(2) -> 
    send("joe","three lines", "one\ntwo\nthree\n");
test(3) -> 
    send_term("joe", "erlmagicstring", "email_handler1", {a,b});
test(4) ->
    Bin = term_to_binary({hi,"joe"}),
    send_term("joe", "erlmagicstring", "email_handler1", {b, Bin}).

%% @hidden 
%% start is launched from procmail.
start() ->
    spawn(?MODULE, internal, []).

%% @hidden 
internal() ->
    L = read_stdin([]),
    case (catch parse_email(L)) of
	{From, Dict, Content} ->
	    Str = get_arg("Subject", Dict),
	    case string:tokens(Str, " ") of
		[_,Mod] ->
		    Term = uue_to_term(Content),
		    call_term_handler(list_to_atom(Mod),
				      From, Dict, Term);
		_ ->
		    io:format("error:~p~n", [Str])
	    end;
	_ ->
	    io:format("Cannot parse:~p~n", [L])
    end,
    init:stop().

call_term_handler(Mod, From, Dict, Term) ->
    case (catch apply(Mod, handle_email, [From, Dict, Term])) of
	{'EXIT', Why} ->
	    io:format("Error in module handler:~p ~p~n", [Mod, Why]);
	Other ->
	    true
    end.

%% @hidden 
parse_email_file(File) ->
    {ok, Bin} = file:read_file(File),
    catch parse_email(binary_to_list(Bin)).

%% @hidden 
parse_mbox(File) ->
    {ok, Bin} = file:read_file(File),
    case catch parse_mbox1(binary_to_list(Bin)) of
	{L, []} -> L;
	_ -> exit(format)
    end.

%% @hidden 
parse_email([$F,$r,$o,$m,$ |T]) ->
    {Address, T1} = get_email_address(T, []),
    {Dict, Content}    = parse_headers(T1),
    {Address,Dict,Content};
parse_email(_) ->
    exit(bad_email_header).

parse_mbox1([$F,$r,$o,$m,$ |T]) ->
    {Address, T1} = get_email_address(T, []),
    {Dict, T2}    = parse_headers(T1),
    {Content, T3} = parse_mbox_content(T2),
    {Emails, T4}  = parse_mbox1(T3),
    {[{Address,Dict,Content}|Emails], T4};
parse_mbox1([]) ->
    {[], []}.
    
get_email_address([$ |T], L) ->
    {reverse(L), skip_to_nl(T)};
get_email_address([H|T], L) ->
    get_email_address(T, [H|L]);
get_email_address([], _) ->
    exit(no_header).

skip_to_nl([$\n|T]) -> T;
skip_to_nl([_|T]) -> skip_to_nl(T);
skip_to_nl([]) -> exit(no_nl).

parse_headers([$\n|T]) ->
    {[], T};
parse_headers(Data) ->
    {Header, Rest} = parser_header_keyword(Data),
    {Body, Rest1}  = parse_header_value(Rest),
    {Headers, T}   = parse_headers(Rest1),
    {[{Header,Body}|Headers], T}.

parser_header_keyword(Data) -> parser_header_keyword(Data, []).

parser_header_keyword([$:|T], L) -> {reverse(L), T};
parser_header_keyword([H|T], L) ->
    case legal_rfc822_header_char(H) of
	true ->
	    parser_header_keyword(T, [H|L]);
	false ->
	    exit({bad_character_in_header, H})
    end;
parser_header_keyword([], L) ->
    exit(bad_header).

parse_header_value(Str) -> parse_header_value(Str, []).

parse_header_value([$\n,$   |T], L) -> parse_header_value(T, L);
parse_header_value([$\n,$\t |T], L) -> parse_header_value(T, L);
parse_header_value([$\n|T],      L) -> {reverse(L), T};
parse_header_value([H|T],        L) -> parse_header_value(T, [H|L]);
parse_header_value([],           L) -> exit(bad_body).

legal_rfc822_header_char(X) when X =< 0, X =< 32 -> false;
legal_rfc822_header_char(127)                    -> false;
legal_rfc822_header_char(_)                      -> true.

parse_mbox_content(Str) -> parse_mbox_content(Str, []).

parse_mbox_content([$\n,$F,$r,$o,$m,$ |T], L) ->
    {reverse(L), [$F,$r,$o,$m,$ |T]};
parse_mbox_content([], L) ->
    {reverse(L), []};
parse_mbox_content([H|T], L) ->
    parse_mbox_content(T, [H|L]).

read_stdin(L) ->
    case io:get_line('') of
	eof -> reverse(L);
	Cs  -> read_stdin(reverse(Cs, L))
    end.
    
%% these are inverses

term_to_uue(T) -> uue:list_to_uue(binary_to_list(term_to_binary(T))).
uue_to_term(L) -> binary_to_term(list_to_binary(uue:uue_to_list(L))).

open_tmp_file(RootName, Suffix) ->
    open_tmp_file(10, RootName, Suffix).

open_tmp_file(0, _, Suffix) ->
    exit({cannot_open_a_temporay_file, Suffix});
open_tmp_file(N, RootName, Suffix) ->
    {_,_,M} = erlang:now(),
    FileName = RootName ++ "/" ++ integer_to_list(M) ++ Suffix,
    %% io:format("trying to open:~p~n", [FileName]),
    case file:open(FileName, write) of
	{ok, Stream} ->
	    {FileName, Stream};
	{error, _} ->
	    open_tmp_file(N-1, RootName, Suffix)
    end.

get_arg(Key, [{Key,Val}|_]) -> Val;
get_arg(Key, [_|T])         -> get_arg(Key, T);
get_arg(Key, [])            -> exit({bad_key,Key}).

