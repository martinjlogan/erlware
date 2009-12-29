%%%-------------------------------------------------------------------
%%% File    : fs_net.erl
%%%
%%% @doc  Functions for doing common network things..
%%% @end
%%%-------------------------------------------------------------------

-module (fs_net).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export ([
	  ping/3,
	  sync_ping/2,
	  sync_ping/1
	 ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

-export ([
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
%% @doc Pings a node up to a number of seconds looking for a specific
%%      result, be it pong or pang.
%%
%% @spec ping (Node, Seconds, Result) -> pang | pong
%% @end
%%--------------------------------------------------------------------

ping (Node, 0, Result) -> 
    net_adm:ping (Node);
ping (Node, Seconds, Result) ->
    case net_adm:ping (Node) of
	Result -> 
	    Result;
	
	_Other ->
	    timer:sleep (1000),
	    ping (Node, Seconds - 1, Result)
    end.


%%--------------------------------------------------------------------
%% @doc Pings a node and returns only after the net kernal distributes the nodes.
%% This function will return pang after 10 seconds if the Node is not found in nodes()
%%
%% @spec sync_ping (Node, Timeout) -> pang | pong
%% @end
%%--------------------------------------------------------------------
sync_ping(Node, Timeout) ->
    case net_adm:ping(Node) of
        pong ->
            case fs_time:poll_until(fun() -> lists:member(Node,[node()|nodes()]) end, 500, Timeout / 500) of
                true  -> pong;
                false -> pang
            end;
        pang ->
            pang
    end.

%% @spec sync_ping (Node) -> pang | pong
%% @equiv sync_ping (Node, 10000)
sync_ping(Node) ->
    sync_ping(Node, 10000).


                                       






