%%% $Id: fs_event_tracker_h_test.erl,v 1.7 2003/09/30 20:32:48 mlogan Exp $
%%%-------------------------------------------------------------------
%%% File    : fs_event_tracker_h_test.erl
%%% Author  : Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%
%%% @doc  
%%% @end
%%%
%%% Created :  8 Sep 2003 by Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%-------------------------------------------------------------------
-module (fs_event_tracker_h_test).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export ([
	  all/1,
	  got_it/2
	 ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export ([
	 ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define (EVENT, my_event).

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
    fs_event_tracker_h:start_link (),
    fs_event_tracker_h:subscribe (?EVENT, ?MODULE, got_it, [self()]),
    fs_event_tracker_h:send_event (?EVENT),
    receive
	?EVENT ->
	    ok
    after
	4000 ->
	    exit (bad_subscription)
    end,
    {ok, 1}        = fs_event_tracker_h:check_event (?EVENT),
    {ok, [?EVENT]} = fs_event_tracker_h:get_events(),
    fs_event_tracker_h:clear(),
    {ok, 0} = fs_event_tracker_h:check_event (?EVENT),
    {ok, 0} = fs_event_tracker_h:check_event (no_event),
    ok.

got_it (Event, [Caller]) -> 
    error_logger:info_msg ("~p:got_it ~p~n", [?MODULE, Event]),
    Caller ! Event.

%%====================================================================
%% Internal functions
%%====================================================================
