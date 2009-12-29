%%%-------------------------------------------------------------------
%%% File    : fs_file.erl
%%% Author  : Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%
%%% @doc  Functions for reading different types of files.
%%% @end
%%%
%%% Created :  1 Aug 2003 by Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%-------------------------------------------------------------------
-module(fs_file).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         read_file/3,
         read_csv/1,
         read_csv/2
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Reads a file with the Delimeter separated value format.
%% <pre>
%% Note * Ignore come in the form of # unless otherwise specified.
%%        Blank lines are ignored.
%%
%% Variables:
%%  Ignore - The parser ignores any line that begins with this string.
%%
%% Types:
%%  FileName = string() | atom()
%%  Ignore = string()
%%  Delimeter = string()
%%  Row = [string()]
%% </pre>
%% @spec read_file(FileName, Ignore, Delimeter) -> [Row] | {error, Reason}
%% @end 
%%--------------------------------------------------------------------
read_file (FileName, Ignore, Delimeter) ->
    case file:read_file(FileName) of
	{ok, Binary}    -> 
            case catch parse (string:tokens (binary_to_list (Binary), "\n"), Ignore, Delimeter) of
                {'EXIT', Reason} -> {error, Reason};
                ParsedFile       -> ParsedFile
            end;
	{error, Reason} -> {error, Reason}
    end.

parse ([Line|Rest], Ignore, Delimeter) -> 
    case is_ignore (Line, Ignore) of
        true  -> parse (Rest, Ignore, Delimeter);
        false -> [string:tokens (Line, Delimeter) | parse (Rest, Ignore, Delimeter)]
    end;
parse ([], Ignore, Delimeter) -> 
    []. 
            
% Simple recursive equality check.
is_ignore ([H|T], [H|T2]) -> is_ignore (T, T2);
is_ignore (_, [])         -> true;
is_ignore ([], _)         -> false;
is_ignore (_, _)          -> false.

%%--------------------------------------------------------------------
%% @doc Reads a file with the comma separated value format.
%% <pre>
%% Note * Ignore come in the form of # unless otherwise specified.
%%        Blank lines are ignored.
%%
%% Variables:
%%  Ignore - The parser ignores any line that begins with this string.
%%
%% Types:
%%  FileName = string() | atom()
%%  Ignore = string()
%%  Row = [string()]
%% </pre>
%% @spec read_csv(FileName, Ignore) -> [Row] | {error, Reason}
%% @end 
%%--------------------------------------------------------------------
read_csv (FileName, Ignore) ->
    read_file (FileName, Ignore, ",").

%% @spec read_csv (FileName) -> [Row] | {error, Reason}
%% @equiv read_csv (FileName, "#")
read_csv (FileName) ->
    read_csv (FileName, "#").
