%%%-------------------------------------------------------------------
%%% File    : fs_syslog_h.erl
%%% Author  : Martin J. Logan <martin@berimbauerlware>
%%% @doc
%%% <p>This module implements a loggger. Logfiles are wrapped 
%%% in the manner that newsyslog wraps syslog generated log files.</p>
%%% @end
%%%
%%% Created : 26 Mar 2002 by Martin J. Logan <martin@berimbauerlware>
%%%-------------------------------------------------------------------
-module(fs_syslog_h).
-vsn('3.0').

-behaviour(gen_event).

-export([
         init/6, 
         init/5, 
         init/4,
         add_handler/7, 
         add_handler/6, 
         add_handler/5,
         log/2, 
         to_file/2,
         to_tty/2,
         config/2
        ]).

-export([init/1, handle_event/2, handle_info/2, terminate/2]).
-export([handle_call/2, code_change/3]). 

-record(state, {dir, 
                filename, 
                maxB, 
                maxF, 
                curF, 
                cur_fd, 
                index = [], 
                pred, 
                opts}).

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------

%%------------------------------------------------------------------
%% @spec add_handler(Manager, Dir, FileName, MaxB, MaxF) -> 
%% ok | {'EXIT', Reason} | term()
%% @equiv add_handler(Manager, Dir, FileName, MaxB, MaxF, Pred, Options)
%% @end
%%------------------------------------------------------------------
add_handler(Manager, Dir, FileName, MaxB, MaxF) -> 
    gen_event:add_handler(Manager,
			  ?MODULE,
			  ?MODULE:init(Dir, FileName, MaxB, MaxF)).

%%------------------------------------------------------------------
%% @spec add_handler(Manager, Dir, FileName, MaxB, MaxF, Options)-> 
%% ok | {'EXIT', Reason} | term()
%% @equiv add_handler(Manager, Dir, FileName, MaxB, MaxF, Pred, Options)
%% @end
%%------------------------------------------------------------------
add_handler(Manager, Dir, FileName, MaxB, MaxF, Options) when list(Options) -> 
    gen_event:add_handler(Manager,
			  ?MODULE,
			  ?MODULE:init(Dir, FileName, MaxB, MaxF, Options));

%%------------------------------------------------------------------
%% @spec add_handler(Manager, Dir, FileName, MaxB, MaxF, Pred) -> 
%% ok | {'EXIT', Reason} | term()
%% @equiv add_handler(Manager, Dir, FileName, MaxB, MaxF, Pred, Options)
%% @end
%%------------------------------------------------------------------
add_handler(Manager, Dir, FileName, MaxB, MaxF, Pred) -> 
    gen_event:add_handler(Manager,
			  ?MODULE,
			  ?MODULE:init(Dir, FileName, MaxB, MaxF, Pred)).

%%------------------------------------------------------------------
%% @doc Adds fs_syslog_h to a manager.
%% <pre>
%% Types: 
%%  Manager = atom()
%%  Dir  = string()
%%  FileName  = string()
%%  MaxB = integer() 
%%  MaxF = byte()
%%  Pred = fun(Event) -> boolean()
%%  Options = flatten | to_tty.  list() of atom()
%% </pre>
%% @spec add_handler(Manager, Dir, FileName, MaxB, MaxF, Pred, Options) -> 
%% ok | {'EXIT', Reason} | term()
%% @end
%%------------------------------------------------------------------
add_handler(Manager, Dir, FileName, MaxB, MaxF, Pred, Options) when list(Options) -> 
    gen_event:add_handler(Manager,
			  ?MODULE,
			  ?MODULE:init(Dir, FileName, MaxB, MaxF, Pred, Options)).
    

%%------------------------------------------------------------------
%% @doc Logs terms.
%% <pre>
%% Expects: 
%%  Manager - The handle of the event manager to send to.
%%  Term - The term to be logged. 
%% </pre>
%% @spec log(Manager, Term) -> void()
%% @end
%%------------------------------------------------------------------
log(Manager, Term) ->
    gen_event:notify(Manager, {?MODULE, Term}).

%%------------------------------------------------------------------
%% @doc Alters configuration of the logger on the fly.
%% <pre>
%%
%% Expects:
%%  Manager - The handle of the event manager to send to.
%%  ConfigList - A list of configuration options.
%% 
%% Types:
%%  ConfigList = [Option]
%%   Option = {Key, bool()}
%%    Key = to_tty | flatten | to_file
%%
%% </pre>
%% @spec config(Manager, ConfigList) -> void()
%% @end
%%------------------------------------------------------------------
config(Manager, ConfigList) ->
    gen_event:notify(Manager, {config, ConfigList}).

%%------------------------------------------------------------------
%% @doc Toggles logging to the screen.
%% <pre>
%% 
%% Expects:
%%  Manager - The handle of the event manager to send to.
%%  Value - the true or false value for whether to log to the terminal.
%%
%% </pre>
%% @spec to_tty(Manager, Value) -> void()
%% @end
%%------------------------------------------------------------------
to_tty(Manager, Value) ->
    config(Manager, [{to_tty, Value}]).

%%------------------------------------------------------------------
%% @doc Toggles logging to a file.
%% <pre>
%% 
%% Expects:
%%  Manager - The handle of the event manager to send to.
%%  Value - the true or false value for whether to log to a file.
%%
%% </pre>
%% @spec to_file(Manager, Value) -> void()
%% @end
%%------------------------------------------------------------------
to_file(Manager, Value) ->
    config(Manager, [{to_file, Value}]).

%%%-----------------------------------------------------------------
%%% This module implements an event handler that writes events
%%% to multiple files (configureable).
%%%-----------------------------------------------------------------
%% @hidden
%% Func: init/3, init/4
%% Args: EventMgr = pid() | atom()
%%       Dir  = string()
%%       FileName  = string()
%%       MaxB = integer() 
%%       MaxF = byte()
%%       Pred = fun(Event) -> boolean()
%% Purpose: An event handler. This writes plain text to a file that is
%%          Rotated in the same manner as newsyslog manages files.
%% Returns: Args = term()
%%          The Args term should be used in a call to
%%          gen_event:add_handler(EventMgr, fs_syslog_h, Args).
%%-----------------------------------------------------------------
init(Dir, FileName, MaxB, MaxF) -> {Dir, FileName, MaxB, MaxF, fun(_) -> true end, []}.

%% @hidden
init(Dir, FileName, MaxB, MaxF, Options) -> {Dir, FileName, MaxB, MaxF, fun(_) -> true end, Options};
init(Dir, FileName, MaxB, MaxF, Pred)    -> {Dir, FileName, MaxB, MaxF, Pred, []}.

%% @hidden
init(Dir, FileName, MaxB, MaxF, Pred, Options) -> {Dir, FileName, MaxB, MaxF, Pred, Options}.

%%-----------------------------------------------------------------
%% Call-back functions from gen_event
%%-----------------------------------------------------------------
init({Dir, FileName, MaxB, MaxF, Pred, Options}) when MaxF > 0, MaxF < 256 -> 
    FormattedOptions = format_options(Options, [{to_file, true}]),
    error_logger:info_msg("~nOptions ~p~n", [FormattedOptions]),
    timer:sleep(500),
    CurF = num_files(Dir, FileName),
    catch shift_files(MaxF, CurF, FileName, Dir),
    case catch file_open(Dir, FileName) of
	{ok, Fd} -> 
	    {ok, #state{dir = Dir, filename = FileName, 
			maxB = MaxB, maxF = MaxF,
			pred = Pred, curF = CurF, 
			cur_fd = Fd, opts = FormattedOptions}};
	Error -> Error
    end.

%%-----------------------------------------------------------------
%% This function may crash!  In this case, this
%% handler is removed by gen_event from the event handlers.
%% Fails: 'file_open' if file:open failed for a log file.
%%        'write_index_file' if file:write_file failed for the
%%            index file.
%%        {file_exit, Reason} if the current Fd crashes. 
%%-----------------------------------------------------------------
handle_event({?MODULE, Term}, State) ->
    handle_logging(Term, State#state.opts, State);
handle_event({config, OptList}, State) ->
    {ok, State#state{opts = format_options(OptList, State#state.opts)}};
handle_event(_, State) -> {ok, State}.


    
handle_info({emulator, GL, Chars}, State) ->
    handle_event({emulator, GL, Chars}, State);
handle_info(_, State) ->
    {ok, State}.

terminate(_, {Record, Options} = State) ->
    file:close(Record#state.cur_fd),
    State.

handle_call(null, State) ->
    {ok, null, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% handle_logging. Determine the user options and take the appropriate options.
handle_logging(Term, [{to_tty, true}|T], State) ->
    io:format("~p - ~p~n", [time(), Term]),
    handle_logging(Term, T, State);
handle_logging(Term, [{flatten, true}|T], State) ->
    handle_logging(fs_utility:flatten_term(Term), T, State);
handle_logging(Term, [{to_file, true}], State) ->
    handle_syslog(Term, State).
    

%% This handles the logging and the rotation of the files. 
%% Returns:
%%  {ok, State}. term()
handle_syslog(Event, State) ->
    #state{maxB = MaxB, curF = CurF, maxF = MaxF, filename = FileName, 
	   dir = Dir, cur_fd = CurFd, pred = Pred} = State,
    case catch Pred(Event) of
	true -> 
	    case file:read_file_info(Dir ++ "/" ++ FileName) of
		{ok, FileInfo} ->
		    Size = element(2, FileInfo),
		    NewRecord = check_size(Size, MaxB, State),
		    case catch io:fwrite(NewRecord#state.cur_fd,"~p - ~p~n", [erlang:localtime(), Event]) of
			ok    -> {ok, NewRecord};
			Error -> {ok, reopen_file(NewRecord)}
		    end;
		{error, Reason} ->
		    NewRecord = reopen_file(State),
		    catch io:fwrite(NewRecord#state.cur_fd,"~p - ~p~n", [erlang:localtime(), Event]),
		    {ok, NewRecord}
	    end;
	_ ->
	    {ok, State}
    end.

file_open(Dir, FileName) ->
    PathString = Dir ++ "/" ++  FileName,
    file:open(PathString, [write]).


%% This finds the number of logfiles.
num_files(Dir, FileName) ->
    {ok, Files} = file:list_dir(Dir),
    num_files(Files, FileName, 0).

num_files([H|T], FileName, Num) ->
    case regexp:match(H, FileName ++ "\.*[0-9]*") of
	{match, Start, Length} ->
	    num_files(T, FileName, Num + 1);
	nomatch ->
	    num_files(T, FileName, Num);
	{error, Reason} ->
	    num_files(T, FileName, Num)
    end;
num_files([], FileName, Num) ->
    Num.


%% Shift all files one over. Returns: ok | {error, Reason}.
shift_files(MaxFiles, 0, FileName, Dir) ->
    ok;
shift_files(MaxFiles, 1, FileName, Dir) ->
    catch file:rename(Dir ++ "/" ++ FileName, 
		Dir ++ "/" ++ FileName ++ ".0"); 
shift_files(MaxFiles, CurrentCount, FileName, Dir) when CurrentCount >= MaxFiles ->
    shift_files(MaxFiles, MaxFiles - 1, FileName, Dir);
shift_files(MaxFiles, CurrentCount, FileName, Dir) ->
    catch file:rename(Dir ++ "/" ++ FileName ++ "." ++ integer_to_list((CurrentCount - 2)), 
		      Dir ++ "/" ++ FileName ++ "." ++ integer_to_list((CurrentCount - 1))), 
    shift_files(MaxFiles, CurrentCount - 1, FileName, Dir).

    
%% Makes sure that the max count is not exeeded.
new_count(MaxF, CurF) when CurF >= MaxF->
    MaxF;
new_count(MaxF, CurF) ->
    CurF + 1.


%% Reopens the file after a problem
reopen_file(Record) ->
    #state{maxB = MaxB, curF = CurF, maxF = MaxF, filename = FileName, 
	   dir = Dir, cur_fd = CurFd} = Record,
    catch file:close(CurFd),
    file:delete(Dir ++ "/" ++ FileName),
    {ok, NewFd} = file_open(Dir, FileName),
    Record#state{cur_fd = NewFd}.


%% Checks the size of the file and determines whether it should be rotated.
check_size(Size, MaxB, State) when Size > MaxB ->
    #state{curF = CurF, maxF = MaxF, filename = FileName, 
	   dir = Dir, cur_fd = CurFd} = State,
    shift_files(MaxF, new_count(MaxF, CurF), FileName, Dir),
    {ok, NewFd} = file_open(Dir, FileName),
    catch file:close(CurFd),
    State#state{cur_fd = NewFd, curF = new_count(MaxF, CurF)};
check_size(Size, MaxB,  State) ->
    State.

%% Formats the options for the loop. Make sure that to_file is the last position in the 
%% options list.
%% Returns:
%%  tuple() | exit({error, bad_options})
%% XXX: Non tuple format for backwards compatability
format_options([{flatten, true}|T], Opts) -> 
    format_options(T, [{flatten, true}|lists:keydelete(flatten, 1, Opts)]);
format_options([{flatten, Value}|T], Opts) -> 
    format_options(T, Opts);

format_options([{to_tty, true}|T], Opts) -> 
    format_options(T, [{to_tty, true}|lists:keydelete(to_tty, 1, Opts)]);
format_options([{to_tty, Value}|T], Opts) -> 
    format_options(T, Opts);

format_options([{to_file, false}|T],Opts) -> 
    format_options(T, lists:keydelete(to_file, 1, Opts));
format_options([{to_file, true}|T],Opts) -> 
    format_options(T, lists:keydelete(to_file, 1, Opts) ++ [{to_file, true}]);

format_options([flatten|T], Opts) -> 
    format_options(T, [{flatten, true}|lists:keydelete(flatten, 1, Opts)]);
format_options([to_tty|T], Opts) -> 
    format_options(T, [{to_tty, true}|lists:keydelete(to_tty, 1, Opts)]);
format_options([to_file|T],Opts) -> 
    format_options(T, lists:keydelete(to_file, 1, Opts) ++ [{to_file, true}]);
format_options([], Opts) -> 
    Opts;
format_options(_, Opts) -> 
    exit({error, bad_options}).











