
%%%-------------------------------------------------------------------
%%% File    : fs_tcp_gateway.erl
%%% Author  : Martin J. Logan 
%%%
%%% @doc 
%%% <p>Listens for connections over tcp and calls for the spawning of a gateway process.</p>
%%% <pre>
%%% Protocal Examples
%%% =================
%%% Requests:
%%% "ID handoff M F Arg1|Arg2\n"  ---------> "ID OK\n"
%%% "ID async M F Arg1|Arg2\n"    ---------> no reply 
%%% "ID sync M F Arg1|Arg2\n"     ---------> "ID UserSpecificReply\n"
%%% </pre>
%%% @end
%%%
%%% Created : 22 Apr 2002 by Martin J. Logan <martin@berimbauerlware>
%%%-------------------------------------------------------------------
-module(fs_tcp_gateway).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_gateway/1, 
         start_link/2, 
         setopts/2, 
         init_listener/3, 
         stop/2, 
         sync_reply/3, 
         sync_reply/4,
         cast/2,
         async_reply/3
        ]).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
-define(EOR_MARKER, "\n").
-define(REQUEST_DELIMITER, " ").
-define(ARG_DELIMETER, "|").
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the gateway server.
%% <pre>
%%
%%  There is to be one gateway server per incoming tcp stream. 
%%  This function  is to be called with the name of a callback module
%%  exhibiting the tcp_gateway behaviour as one of its parameters.
%%
%% Expects:
%%  CallBackModule - The user defined callback module. 
%%
%% </pre>
%%
%% @spec 
%%  start_gateway(CallBackModule) -> {ok, Pid}
%%   CallBackModule = atom()
%%   Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_gateway(CallBackModule) ->
    gen_server:start_link(?MODULE, [CallBackModule], []).

%%--------------------------------------------------------------------
%% @doc 
%%  Starts the server that listens on the well known tcp port
%%  for incoming requests.
%%
%% <pre>
%% Expects:
%%  CallBackModule - The user defined callback module.
%%  TCPPort - the port to listen on.
%%
%% Returns:
%%   Pid = pid()
%%
%% </pre>
%%
%% @spec 
%%  start_link(CallBackModule, TCPPort) -> {ok, Pid}
%%   CallBackModule = atom() 
%%   TCPPort = integer()
%%   Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(CallBackModule, TCPPort) ->
    proc_lib:start_link(?MODULE, init_listener, [self(), CallBackModule, TCPPort]).

%%--------------------------------------------------------------------
%% @doc This stops a gateway.
%% <pre>
%%
%% Expects:
%%  Pid - The pid() of the gen_server to stop.
%%  Reason - The reason for stopping.
%%
%% </pre>
%%
%% @spec stop(Pid, Reason) -> ok 
%% @end
%%--------------------------------------------------------------------
stop(Pid, Reason) ->
    gen_server:cast(Pid, {stop, Reason}).

%%--------------------------------------------------------------------
%% @doc This sends a message back out over a stream managed by Server. 
%% <pre>
%%
%% NOTE: The easiest way to get Server is to collect 
%% it with self() in one of the callbacks.
%%
%% Expects: 
%%  Server - The identifier of the server that manages the stream.
%%  Msg - The message to be sent.
%%
%% Types:
%%  Server = {Name, Node} | pid()
%%
%% </pre>
%%
%% @spec cast(Server, Msg) -> ok
%% @end
%%--------------------------------------------------------------------
cast(Server, Msg) -> 
    gen_server:cast(Server, {cast, Msg}).

%%--------------------------------------------------------------------
%% @doc: This sends a response to a gateway
%% <pre>
%% Variables:
%%  Pid - The pid() of the gen_server to stop.
%%  ID - The request id = string()
%%  Reply - a string() to send back over tcp.
%%  Timeout - The length in miliseconds to wait for a return.
%%
%% Types:
%%  Pid = pid()
%%  ID = string()
%%  Reply string()
%%  Timeout = integer()
%% </pre>
%% @spec sync_reply(Pid, ID, Reply, Timeout) -> ok | exit(Reason)
%% @end
%%--------------------------------------------------------------------
sync_reply(Pid, ID, Reply, Timeout) ->
    gen_server:call(Pid, {sync_reply, ID, Reply}, Timeout).

%% @spec sync_reply(Pid, ID, Reply) -> ok | exit(Reason)
%% @equiv sync_reply(Pid, ID, Reply, infinity)
sync_reply(Pid, ID, Reply) ->
    gen_server:call(Pid, {sync_reply, ID, Reply}).

%%--------------------------------------------------------------------
%% @doc This stops a gateway.
%% <pre>
%% Variables:
%%  Pid - The pid() of the gen_server to stop.
%%  ID - The request id = string()
%%  Reply - string()
%% </pre>
%% @spec async_reply(Pid, ID, Reply) -> ok
%% @end
%%--------------------------------------------------------------------
async_reply(Pid, ID, Reply) ->
    gen_server:cast(Pid, {async_reply, ID, Reply}).

%%--------------------------------------------------------------------
%% @doc Sets the options on the socket for active = true.
%% <pre>
%% Variables:
%%  Pid - The pid of the gateway to send to.
%%  ClientSocket - The socket to set opts on.
%% Side Effects: 
%%  This function changes the controlling process of the client socket
%%  from the caller to the gateway for whom the pid() belongs.
%% </pre>
%% @spec setopts(Pid, ClientSocket) -> ok
%% @end
%%--------------------------------------------------------------------
setopts(Pid, ClientSocket) ->
    ok = gen_tcp:controlling_process(ClientSocket, Pid),
    gen_server:cast(Pid, {setopts, ClientSocket}).

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
%%         State = {Data, CallBackModule, UserState, Socket}
%%--------------------------------------------------------------------
init([CallBackModule]) ->
    process_flag(trap_exit, true),
    {ok, UserState} = CallBackModule:init(),
    {ok, {[], CallBackModule, UserState, nill}}.


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
handle_call({sync_reply, ID, Reply}, From, {[], CallBackModule, UserState, Socket}) ->
    reply(Socket, Reply, ID),
    {noreply, {[], CallBackModule, UserState, Socket}}.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({async_reply, ID, Reply}, {[], CallBackModule, UserState, Socket}) ->
    reply(Socket, Reply, ID),
    {noreply, {[], CallBackModule, UserState, Socket}};
handle_cast({cast, Msg}, {_, _, _, Socket} = State) ->
    reply(Socket, Msg, "-1"),
    {noreply, State};
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};
handle_cast({setopts, ClientSocket}, {[], CallBackModule, UserState, _}) ->
    ok = inet:setopts(ClientSocket, [{active, true}]),
    {noreply, {[], CallBackModule, UserState, ClientSocket}}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({tcp, Socket, BinaryData}, {Data, CallBackModule, UserState, Socket}) ->
    Buffer = Data ++ binary_to_list(BinaryData),
    case string:rstr(Buffer, "\r") of
        0 ->
            case string:rstr(Buffer, "\n") of
                0 -> {noreply, {Buffer, CallBackModule, UserState, Socket}};
                Index ->
                    NewUserState = 
                        service_request(parse_request(string:sub_string(Buffer, 1 , Index - 1)), 
                                        Socket, CallBackModule, UserState),
                    {noreply, {[], CallBackModule, NewUserState, Socket}}
            end;
        Index ->
            NewUserState = 
                service_request(parse_request(string:sub_string(Buffer, 1 , Index - 1)), 
                                Socket, CallBackModule, UserState),
            {noreply, {[], CallBackModule, NewUserState, Socket}}
    end;
handle_info({tcp_closed, Socket}, State) ->
    {stop, {error, tcp_closed}, State};   
handle_info({tcp_error, Socket, Reason}, State) ->
    {stop, Reason, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, {_, CallBackModule, UserState, Socket}) ->
    CallBackModule:terminate(Reason, UserState),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%===========================================
%%% Internal functions
%%%===========================================

%% <dt>Expects:</dt> 
%%  Request -  Format "ID sync|async|handoff mod func args(pipe delimeted)"
%% Returns:
%%  {SyncFlag, RequestId, M, F, ArgList}.
parse_request(Request) ->
    {ok, [RequestId, SyncFlag, M, F], Args} = io_lib:fread("~s~a~a~a ", Request),
    ArgList = string:tokens(Args, ?ARG_DELIMETER),
    {SyncFlag, RequestId, M, F, ArgList}.


%% service_request/3
%% <dt>Expects:</dt> 
%%  Request - a gateway formatted request.
%%  Socket - the tcp socket
%%  CallBackModule - The module defineing the users callbacks.
%% Returns: void()
service_request({handoff, ID, M, F, ArgList}, Socket, CallBackModule, UserState) -> 
    reply(Socket, "OK", ID),
    return_request_async(CallBackModule:async_request(ID, M, F, ArgList, UserState));
service_request({async, ID, M, F, ArgList}, Socket, CallBackModule, UserState) -> 
    return_request_async(CallBackModule:async_request(ID, M, F, ArgList, UserState));
service_request({sync, ID, M, F, ArgList}, Socket, CallBackModule, UserState) ->
    return_request_sync(CallBackModule:sync_request(ID, M, F, ArgList, UserState), Socket, ID).

% Helper for service request
return_request_sync({reply, Reply, State}, Socket, ID) -> 
    reply(Socket, Reply, ID), 
    State;
return_request_sync({noreply, State}, Socket, ID) -> 
    State;
return_request_sync({stop, Reply, Reason, State}, Socket, ID) -> 
    reply(Socket, Reply, ID), 
    stop(self(), Reason),
    State.

return_request_async({noreply, State}) ->
    State;
return_request_async({stop, Reason, State}) ->
    stop(self(), Reason),
    State.
    

%% reply/3: Formats an ID and a string into a properly formatted reply for tcp
%% <dt>Expects:</dt>
%%  Reply - string()
%%  ID - string()
%% Returns:
%%  Response - see gen_tcp
reply(Socket, Reply, ID) -> gen_tcp:send(Socket, format_reply(Reply, ID)).

format_reply(Reply, ID) when integer(ID) -> integer_to_list(ID) ++ " " ++ Reply ++ "\n";
format_reply(Reply, ID) -> ID ++ " " ++ Reply ++ "\n".


%%%===========================================
%%% Code to Listen on TCP socket and spawn Gateway procs.
%%%===========================================

%%--------------------------------------------------------------------
%% @hidden
%% Function: init/1
%% Description: Initiates the server
%% <dt>Expects:</dt>
%%  Parent - The pid of the parent process. = pid()
%%  CallBackModule - The user defined callback module.  = atom()
%%  TCPPort - the port to listen on.  = integer()
%% Returns: {ok, Pid}
%% @end
%%--------------------------------------------------------------------
init_listener(Parent, CallBackModule, TCPPort) ->
    process_flag(trap_exit, true),

    TCPOptions = [binary, {packet, raw}, {active, false}, {reuseaddr, true}],
    {ok, ListenSocket} = gen_tcp:listen(TCPPort, TCPOptions),

    proc_lib:init_ack(Parent, {ok, self()}),
    accept(ListenSocket, CallBackModule).


%% Function: listen/2
%% Description: listens for incoming connections
accept(ListenSocket, CallBackModule) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    {ok, Pid}          = setup_handler(ClientSocket, CallBackModule),
    accept(ListenSocket, CallBackModule).

    
%% Function: setup_handler/2
%% Description: Sets up the handler process so that it can read from the 
%% socket..
setup_handler(ClientSocket, CallBackModule) ->
    {ok, Pid} = ?MODULE:start_gateway(CallBackModule),
    ok        = ?MODULE:setopts(Pid, ClientSocket),
    {ok, Pid}.



