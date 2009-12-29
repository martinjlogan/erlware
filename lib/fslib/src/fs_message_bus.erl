%%%-------------------------------------------------------------------
%%% File    : fs_message_bus.erl
%%%
%%% @doc Sends and receives messages on a sender-receiver decoupled
%%%      message bus.
%%% @end
%%%-------------------------------------------------------------------

-module (fs_message_bus).

-behaviour (gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include ("macros.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export([
	 start/1,
	 start/0,
         start_link/1,
         start_link/0,
         stop/0,
         publish/3,
         subscribe/4,
	 unsubscribe/1
        ]).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------

-record (state, {
	   mcast_socket,
	   mcast_addr,
	   recv_connection_list = [],
	   subscribers = []
	  }).

-record(recv_connection, {addr, port, socket}).
 
%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------

-define (SERVER, ?MODULE).
-define (PROTOCOL_HEADER, "QBUS").
-define (INADDR_ANY_PORT, 0).
-define (FS_MESSAGE_BUS_IP, {224,2,1,1}).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start(MulticastAddr::tuple()) -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start (MulticastAddr) ->
    gen_server:start ({local, ?SERVER}, ?MODULE, [MulticastAddr], []).

%% @spec start() -> {ok, pid()} | {error, Reason}
%% @equiv start(DefaultMulticastAddr) 
start () ->
    start(?FS_MESSAGE_BUS_IP).
%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(MulticastAddr::tuple()) -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link (MulticastAddr) ->
    gen_server:start_link ({local, ?SERVER}, ?MODULE, [MulticastAddr], []).

%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @equiv start_link(DefaultMulticastAddr) 
start_link () ->
    start_link(?FS_MESSAGE_BUS_IP).
%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------

stop () ->
    gen_server:cast (?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc Publishes a message to all subscribers.
%%
%% @spec publish (MultiCastPort, Domain::atom(), Payload::term()) -> ok
%% @end
%%--------------------------------------------------------------------

publish (MultiCastPort, Domain, Payload) ->
    gen_server:cast (?SERVER, {publish, {MultiCastPort, Domain, Payload}}).

%%--------------------------------------------------------------------
%% @doc Subscribes to messags that match a pattern.
%% The message that comes back to Pid is {message_bus, Domain, Payload};
%% @spec subscribe (MultiCastPort, Domain::atom(), Pattern, Pid) -> ok
%% @end
%%--------------------------------------------------------------------

subscribe (MultiCastPort, Domain, '_', Pid) ->
    gen_server:call (?SERVER, {subscribe, {MultiCastPort, Domain, '_', Pid}});
subscribe (MultiCastPort, Domain, Pattern, Pid) ->
    {error, not_yet_implemented}.

%%--------------------------------------------------------------------
%% @doc Unsubscribes from all messages previously subscribed.
%%
%% Implementation note:  The intention is to eventually have
%% unsubscription by pattern so the Pid can sub/unsub at will
%% per pattern and not all at once.  But this at least handles
%% graceful termination of subscribers when they want to quickly
%% punt the subscriptions.
%%
%% @spec unsubscribe (Pid) -> ok
%% @end
%%--------------------------------------------------------------------

unsubscribe (Pid) ->
    gen_server:call (?SERVER, {unsubscribe, Pid}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------

init ([MultiCastAddr]) ->
    %% Trap subscriber exits.
    process_flag (trap_exit, true),
    %% Socket for broadcasting local messages.
    {ok, Socket} = gen_udp:open(?INADDR_ANY_PORT),
    {ok, #state{mcast_socket = Socket, mcast_addr = MultiCastAddr}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call ({subscribe, {MultiCastPort, Domain, Pattern, Pid}}, From, #state{recv_connection_list = RCs, mcast_addr = MultiCastAddr} = State) ->
    link (Pid),

    case [Port || #recv_connection{port = Port} <- RCs, Port == MultiCastPort] of
        [] -> 
            RC = setup_multicast(MultiCastAddr, MultiCastPort),
            {reply, ok, State#state{subscribers = [{Domain, Pid} | State#state.subscribers], recv_connection_list = [RC|RCs]}};
        AlreadyConnected ->
            link (Pid),
            {reply, ok, State#state{subscribers = [{Domain, Pid} | State#state.subscribers]}}
    end;

handle_call ({unsubscribe, Pid}, From, State) ->
    unlink (Pid),
    {reply, ok, State#state{subscribers = lists:keydelete (Pid, 2, State#state.subscribers)}}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast ({publish, {MultiCastPort, Domain, Payload}}, State) ->
    multicast (State#state.mcast_socket, Domain, Payload, State#state.mcast_addr, MultiCastPort),
    {noreply, State};

handle_cast (stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_info ({udp, _, Address, Port, MessageBinary}, State) ->
    case catch binary_to_term (MessageBinary) of
	{'EXIT', _} ->
	    {noreply, State};

	{?PROTOCOL_HEADER, Domain, Payload} ->
	    lists:foreach (fun ({Domain_, Pid}) -> 
	    		       case Domain_ of
			            Domain_ when Domain == Domain_ -> Pid ! {message_bus, Domain, Payload};
				    Domain_                        -> ok
			       end
			   end,
			   State#state.subscribers),
	    {noreply, State};

	_ ->
	    %% Unknown protocol, must be new revision I don't understand.
	    %% Simply ignore it.  Maybe I'll be upgraded some day.
	    {noreply, State}
    end;

handle_info ({'EXIT', Pid, Reason}, State) ->
    {noreply, State#state{subscribers = lists:keydelete (Pid, 2, State#state.subscribers)}};

handle_info (_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------

terminate (Reason, State) ->
    gen_udp:close (State#state.mcast_socket),
    case State#state.recv_connection_list of
	[]  -> ok;
	RCs -> lists:foreach(fun(#recv_connection{socket = Socket}) -> gen_udp:close (Socket) end, RCs)
    end,
    case State#state.subscribers of
	[] -> 
            ok;
	_  ->
	    ?ERROR_MSG ("terminating when subscribers still active: ~p~n", [State#state.subscribers]),
	    lists:foreach (fun ({Domain, Pid}) -> unlink (Pid) end, State#state.subscribers)
    end.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------

code_change (OldVsn, State, Extra) ->
    {ok, State}.

%%%=========================================================
%%% Internal functions
%%%=========================================================

multicast (Socket, Domain, Payload, MulticastAddr, MulticastPort) ->
    Message = term_to_binary ({?PROTOCOL_HEADER, Domain, Payload}),
    gen_udp:send (Socket, MulticastAddr, MulticastPort, Message).


%% Returns: RC::record()
setup_multicast(MultiCastAddr, MultiCastPort) ->
    %% Setup for multicast.
    {ok, Hostname}                     = inet:gethostname(),
    {ok, {_, _, _, _, _, [LocalIP|_]}} = inet:gethostbyname(Hostname ++ ".erlware.com"),
    {ok, Socket} = gen_udp:open(MultiCastPort, [binary,
						{reuseaddr,true},
						{ip,MultiCastAddr},
						{multicast_loop,true},
						{add_membership,{MultiCastAddr, {0,0,0,0}}}]),
    #recv_connection{addr = MultiCastAddr, port = MultiCastPort, socket = Socket}.

