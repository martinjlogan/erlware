%%% $Id: fs_process_data.erl,v 1.5 2004/01/14 22:46:26 mlogan Exp $
%%%-------------------------------------------------------------------
%%% File    : fs_process_data.erl
%%% Author  : Martin J. Logan <martin@mecha.erlware.com>
%%%
%%% @doc  fetch the queue lengths of individual processes on a node.
%%% @end
%%%
%%% Created : 18 Dec 2003 by Martin J. Logan <martin@mecha.erlware.com>
%%%-------------------------------------------------------------------
-module(fs_process_data).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("macros.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/1,
         configuration_spec/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([init/2]).


%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {nodes, frequency, names}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc The starting point.
%% <pre>
%% Types:
%%  Options = [Option] see configuration_spec/1
%% </pre>
%% @spec start_link(Options) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    proc_lib:start_link(?MODULE, init, [self(), Options]).

%%--------------------------------------------------------------------
%% @doc Returns the keys, required or optional, used for configuration of this process.
%% <pre>
%% Conforms to the GAS behaviour.
%% Variables:
%%  Function - The function that the config spec pertains to.
%%
%% Types:
%%  Function = atom()
%%  ConfigurationSpec = [Spec]
%%   Spec = {optional, ConfToken} | {required, ConfToken}
%%    ConfToken = {App, Key} | Key 
%%     App = Key = atom()
%%
%% Configuration:
%%  nodes - The nodes to poll. This can either equal a list of nodes or the atom 'node()' for the local node.
%%  frequency - The frequency of polling.
%%  names - the registered names of the processes to monitor.
%%
%% </pre>
%% @spec configuration_spec(Function) -> ConfigurationSpec
%% @end
%%--------------------------------------------------------------------
configuration_spec(start_link) ->
    [{required, frequency},
     {required, nodes}].

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Initializes this server.
%% Variables:
%%  From - The pid of the parent process.
%% @hidden
%%--------------------------------------------------------------------
init(From, Options) ->
    register(?SERVER, self()),
    State = options_to_state(Options),
    proc_lib:init_ack(From, {ok, self()}),
    loop(State).

%%====================================================================
%% Internal functions
%%====================================================================
loop(State) ->
    lists:foreach(
      fun(Node) -> 
              case rpc:call(Node, erlang, processes, []) of
                  {badrpc, Reason} -> 
		      ok;
                  Processes ->
                      lists:foreach(fun(Pid) -> 
                                            QueueLen       = rpc:call(Node, erlang, process_info, [Pid, message_queue_len]),
                                            Reductions     = rpc:call(Node, erlang, process_info, [Pid, reductions]),
                                            RegisteredName = 
	 					case rpc:call(Node, erlang, process_info, [Pid, registered_name]) of
	 					    {_, RegisteredName_} -> RegisteredName_;
 						    X                    -> X
					        end,
					    system_status:publish(6, {queue_len, RegisteredName}, {Pid, QueueLen}),
					    system_status:publish(6, {reductions, RegisteredName}, {Pid, Reductions})
                                    end, Processes) 
              end
      end, State#state.nodes),
    timer:sleep(State#state.frequency),
    loop(State).


options_to_state(Options) ->
    options_to_state(Options, #state{nodes = undefined, names = undefined, frequency = undefined}).

options_to_state(Options, State) ->
    lists:foldl(fun(Option, State_) -> option_to_state(Option, State_) end, State, Options).
                       
option_to_state({nodes, 'node()'}, State) ->
    State#state{nodes = [node()]};
option_to_state({nodes, Nodes}, State) ->
    State#state{nodes = Nodes};
option_to_state({frequency, Frequency}, State) ->
    State#state{frequency = Frequency}.
