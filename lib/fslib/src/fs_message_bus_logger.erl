%%%-------------------------------------------------------------------
%%% File    : fs_message_bus_logger.erl
%%%
%%% @doc Dumps message bus traffic in ergonomic format out to stdout.
%%% @end
%%%-------------------------------------------------------------------

-module (fs_message_bus_logger).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export ([
          start/2,
          start/1
	 ]).

%%====================================================================
%% External functions
%%====================================================================

%% @doc Starts a logger for the monitoring namespace witha port to listen on and the default domain of monitoring.
%% @spec start (Port) -> void ()

start (Port) ->
	start (Port, monitoring).

%% @doc Starts a logger for a specific message bus domain on a specific port.
%% @spec start (Port, Domain) -> void ()

start (Port, Domain) ->
    fs_message_bus:start (),
    fs_message_bus:subscribe (Port, Domain, '_', self ()),
    loop ().

%%====================================================================
%% Internal functions
%%====================================================================

loop () ->
    receive
	{message_bus, Domain, Payload} ->
	    format (Domain, Payload),
	    loop ()
    end.

%% format for printing. The first clause is system status message specific.
format (monitoring, {system_status, Priority, Now, Node, Application, Attributes, ID, Payload}) ->
    case lists:member(causal, Attributes) of
        true ->
            io:format ("~n~n----------------------~n"),
            io:format ("Node Name:   ~p~n" 
                       "Application: ~p~n"
                       "ID:          ~p~n"
                       "Payload:     ~p~n", [Node, Application, ID, Payload]);
        false ->
            ok
    end;

format (Domain, Payload) ->
    io:format ("~n~n----------------------~n"),
    io:format ("Domain: ~p  Payload: ~p~n", [Domain, Payload]).













