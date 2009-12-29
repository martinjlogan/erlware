%%% $Id: fs_exit_handler_h_test.erl,v 1.4 2003/09/23 15:22:46 enewhuis Exp $
%%%-------------------------------------------------------------------
%%% File    : fs_exit_handler_h_test.erl
%%% Author  : Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%
%%% @doc  Guess
%%% @end
%%%
%%% Created :  3 Sep 2003 by Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%-------------------------------------------------------------------

-module (fs_exit_handler_h_test).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export ([
	  all/1
	 ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

-export ([
	 ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

-define (SERVER, ?MODULE).
-define (PROC1, proc1).
-define (PROC2, proc2).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc All the starting point for the test functions.
%% @spec all() -> ok | exit(Reason)
%% @end 
%%--------------------------------------------------------------------

all (Config) ->
    error_logger:tty (false),
    fs_exit_handler_h:start_link (),
    monitor_two (),
    fs_exit_handler_h:stop (),
    fs_exit_handler_h:start_link (),
    monitor_three (),
    ok.

    
%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc test the monitor/2 function
%% @end 
%%--------------------------------------------------------------------

monitor_two () ->
    spawn_link (fun() -> live_and_die () end),

    % Wait for 'EXIT' signal to propagate.
    timer:sleep (1000),

    [Stats] = fs_exit_handler_h:get_stats (),
    ?PROC1  = element (2, Stats),
    1       = element (3, Stats).

%%--------------------------------------------------------------------
%% @doc test the monitor/3 function
%% @end 
%%--------------------------------------------------------------------

monitor_three () ->
    register (?SERVER, self ()),
    spawn_link (fun() ->
			fun_live_and_die ()
		end),

    % Wait for 'EXIT' signal to propagate.
    ok = receive
	     fun_called ->
		 ok
	 after
	     1000 ->
		 error
	 end,

    [Stats] = fs_exit_handler_h:get_stats (),
    ?PROC2  = element (2, Stats),
    1       = element (3, Stats).


%%====================================================================
%% Internal functions
%%====================================================================

% Order of ops here matters.
live_and_die () ->
    fs_exit_handler_h:monitor (self (), ?PROC1).

fun_live_and_die () ->
    fs_exit_handler_h:monitor (self (), ?PROC2, fun () ->
							?SERVER ! fun_called
						end).
    

