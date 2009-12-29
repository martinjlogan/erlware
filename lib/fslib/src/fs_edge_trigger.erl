%%%-------------------------------------------------------------------
%%% Created : 25 Apr 2003 by Martin J. Logan <martin@localhost.localdomain>
%%% File    : fs_edge_trigger.erl
%%% @author Martin J. Logan 
%%%
%%% @doc  
%%% This module uses a dict to store all types of events and there status with
%%% respect to being on the edge of that status changing.
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(fs_edge_trigger).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         new/0,
         edge/2,
         clear/2,
         reset/2
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
%% @doc Creates a new edge_trigger structure.
%% @spec new() -> Struct
%% @end 
%%--------------------------------------------------------------------
new() ->
    dict:new().

%%--------------------------------------------------------------------
%% @doc Enters an event into the struct and returns true if its on an edge.
%% @spec edge(Event, Struct) -> {bool(), NewStruct}
%% @end 
%%--------------------------------------------------------------------
edge(Event, Struct) ->
    case dict:find(Event, Struct) of
        error       -> {true, dict:store(Event, false, Struct)};
        {ok, true}  -> {true, dict:store(Event, false, Struct)};
        {ok, false} -> {false, Struct}
    end.
            
%%--------------------------------------------------------------------
%% @doc Resets an event that may or may not have been  previously triggered.
%% <pre> 
%% A little slower than reset but will not exit if the event
%% is not present in the struct.
%% </pre>
%% @spec clear(Event, Struct) -> NewStruct
%% @end 
%%--------------------------------------------------------------------
clear(Event, Struct) ->
    case dict:find(Event, Struct) of
        error       -> dict:store(Event, true, Struct);
        {ok, true}  -> Struct;
        {ok, false} -> dict:store(Event, true, Struct)
    end.
            
%%--------------------------------------------------------------------
%% @doc Resets an event that was previously triggered.
%% @spec reset(Event, Struct) -> NewStruct | exit(enoevent)
%% @end 
%%--------------------------------------------------------------------
reset(Event, Struct) ->
    case dict:fetch(Event, Struct) of
        true  -> Struct;
        false -> dict:store(Event, true, Struct)
    end.
            

%%====================================================================
%% Internal functions
%%====================================================================
