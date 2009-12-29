%%% $CVS$
%%%-------------------------------------------------------------------
%%% File    : fs_gen_tcp_recv.erl
%%% Author  : Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%
%%% @doc  
%%% <p>A generic tcp gateway behaviour. fs_gen_tcp_recv accepts and services 
%%% tcp connections on a particular port.</p>
%%%
%% <pre>
%%% ts_gen_tcp module           Callback module
%%% -----------------           ---------------
%%% fs_gen_tcp_recv:start       ------>  CallbackModule:init/1 
%%% fs_gen_tcp_recv:start_link  ------>  CallbackModule:init/1 
%%%
%%% fs_gen_tcp_recv:call        ------>  CallbackModule:handle_call/4 
%%% 
%%% Erlang msg | timeout        ------>  CallbackModule:handle_info/3 
%%%
%%% Data from socket            ------>  CallbackModule:packet/3
%%%
%%% Socket closed               ------>  CallbackModule:terminate/2
%%% 
%%% CallBack Functions
%%% ------------------
%%%
%%% </pre>
%%% <strong>CallbackModule:init(InitArgs) -> {ok, State} | {ok, NewState, Timeout}
%%% </strong><pre>
%%% Types:
%%%  InitArgs = term()
%%%  Timeout = integer()
%%%
%%% </pre>
%%% <strong>CallbackModule:packet(Socket, Packet, State) -> >
%%% </strong><pre>
%%%  {noreply, NewState} | {noreply, NewState, Timeout}
%%%  {reply, Reply, NewState, Timeout} | {reply, Reply, NewState} |  
%%%  {stop, Reason, NewState} | {stop, Reason, Reply, NewState}
%%% Variables:
%%%  Reply - A reply sent back over the socket to the sender of Packet.
%%% 
%%% Types:
%%%  Socket = socket()
%%%  Reply = Packet = binary() by default. This can be altered via 
%%%                   TCPOptions(see ?MODULE:start/5)
%%%  State = NewState = term()
%%%  Timeout = integer()
%%%
%%% </pre>
%%% <strong>CallbackModule:handle_call(Socket, From, Msg, State) -> 
%%%  {noreply, NewState} | {noreply, NewState, Timeout}
%%%  {reply, Reply, NewState, Timeout} | {reply, Reply, NewState} |  
%%%  {stop, Reason, NewState} | {stop, Reason, Reply, NewState}
%%% </strong><pre>
%%% Variables:
%%%  Reply - A reply sent back to the sender of Msg.
%%% 
%%% Types:
%%%  Socket = socket()
%%%  Msg = term() | timeout
%%%  Reply = term()
%%%  State = NewState = term()
%%%  Timeout = integer()
%%%
%%% </pre>
%%% <strong>CallbackModule:handle_info(Socket, Msg, State) -> >
%%%  {noreply, NewState} | {noreply, NewState, Timeout} | {stop, Reason, NewState}
%%% </strong><pre>
%%% Types:
%%%  Socket = socket()
%%%  Msg = term() | timeout
%%%  State = NewState = term()
%%%  Timeout = integer()
%%%
%%% </pre>
%%% <strong>CallbackModule:terminate(Socket, State) ->> term()</strong>
%%% <pre> module does not look at return value
%%% Types:
%%%  Socket = socket()
%%%  State = NewState = term()
%%% </pre>
%%% @end
%%%
%%% Created :  5 Jun 2003 by Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%-------------------------------------------------------------------
-module(fs_gen_tcp_recv).

%%-------------------------------------------------------------------
%% External Exports
%%-------------------------------------------------------------------
-export([
         behaviour_info/1,
         start/5,
         start/4,
         start_link/5,
         start_link/4,
         reply/2,
         call/2
        ]).

%%-------------------------------------------------------------------
%% Macro Definitions
%%-------------------------------------------------------------------
-define(TAG, ?MODULE).

%%-------------------------------------------------------------------
%% Internal Exports
%%-------------------------------------------------------------------
-export([
         init_it/6,
         listener/6
        ]).

%%------------------------------------------------------------------------------
%% @doc Contains the functions that the gen_query_handler behaviour should.
%% @spec behaviour_info(Tag) -> list() | undefined
%% @end
%%------------------------------------------------------------------------------
behaviour_info(callbacks) -> [{packet,3},{init,1},{terminate,2}];
behaviour_info(_Other)    -> undefined.


%%------------------------------------------------------------------------------
%% @doc Starts the fs_gen_tcp_recv server.
%% <pre>
%% Variables: 
%%  CallbackModule - User defined callback module.
%%  TCPport - The port to listen on.
%%  TCPOptions - The options to gen_tcp:listen.
%%  InitArgs - Arguments to be passed to CallbackModule:init/1
%%  Options - See module gen.
%%
%% Types:
%%  CallbackModule = atom()
%%  TCPport = integer()
%%  TCPOptions = [TCPOption] 
%%  Options = [Option] 
%%
%% Note *
%%  The default setting for TCPOptions is
%%    TCPOptions = [binary, {packet, raw}, {active, false}, {reuseaddr, true}],
%% </pre>
%% @spec start(CallbackModule, TCPPort, TCPOptions, InitArgs, Options) -> {ok,pid()}
%% @end
%%------------------------------------------------------------------------------
start(CallbackModule, TCPPort, TCPOptions, InitArgs, Options) ->
    Args = [self(), CallbackModule, TCPPort, TCPOptions, InitArgs, Options],
    proc_lib:start(?MODULE, listener, Args).

%% @spec start(CallbackModule, TCPPort, TCPOptions, InitArgs) -> {ok, pid()}
%% @equiv start_link(CallbackModule, TCPPort, TCPOptions, InitArgs, [])
start(CallbackModule, TCPPort, TCPOptions, InitArgs) ->
    Args = [self(), CallbackModule, TCPPort, TCPOptions, InitArgs, []],
    proc_lib:start(?MODULE, listener, Args).


%%------------------------------------------------------------------------------
%% @doc Starts the fs_gen_tcp_recv server.
%% <pre>
%% Variables: 
%%  CallbackModule - User defined callback module.
%%  TCPport - The port to listen on.
%%  TCPOptions - The options to gen_tcp:listen.
%%  InitArgs - Arguments to be passed to CallbackModule:init/1
%%  Options - See module gen.
%%
%% Types:
%%  CallbackModule = atom()
%%  TCPport = integer()
%%  TCPOptions = [TCPOption] 
%%  Options = [Option] 
%%
%% Note *
%%  The default setting for TCPOptions is
%%    TCPOptions = [binary, {packet, raw}, {active, false}, {reuseaddr, true}],
%% </pre>
%% @spec start_link(CallbackModule, TCPPort, TCPOptions, InitArgs, Options) -> {ok,pid()}
%% @end
%%------------------------------------------------------------------------------
start_link(CallbackModule, TCPPort, TCPOptions, InitArgs, Options) ->
    Args = [self(), CallbackModule, TCPPort, TCPOptions, InitArgs, Options],
    proc_lib:start_link(?MODULE, listener, Args).

%% @equiv start_link(CallbackModule, TCPPort, TCPOptions, InitArgs, [])
%% @spec start_link(CallbackModule, TCPPort, TCPOptions, InitArgs) -> {ok,pid()}
start_link(CallbackModule, TCPPort, TCPOptions, InitArgs) ->
    Args = [self(), CallbackModule, TCPPort, TCPOptions, InitArgs, []],
    proc_lib:start_link(?MODULE, listener, Args).

%%------------------------------------------------------------------------------
%% @doc Sync call to a tcp server.
%% <pre>
%% Variables: 
%%  Server - The indentifier for the server to message.
%%  Msg - The message to be delivered.
%%
%% Types:
%%  Msg = term()
%%
%% Note *
%%  Calls the server callback handle_call.
%% </pre>
%% @spec call(Server, Msg) -> Reply | exit()
%% @end
%%------------------------------------------------------------------------------
call(Server, Msg) -> 
    gen:call(Server, ?TAG, Msg).

%%------------------------------------------------------------------------------
%% @doc Reply to a sync call.
%% <pre>
%% Variables: 
%%  From - The From received in a handle_call/4 function.
%%  Reply - The reply to be delivered.
%%
%% Types:
%%  reply = term()
%%
%% </pre>
%% @spec reply(From, Reply) -> Reply | exit()
%% @end
%%------------------------------------------------------------------------------
reply(From, Reply) -> 
    gen:reply(From, Reply).


%%------------------------------------------------------------------------------
%% @hidden
%% Initiate the new process.
%% Register the name using the Rfunc function
%% Calls the Mod:init/Args function.
%% Finally an acknowledge is sent to Parent and the main
%% loop is entered.
%% 
%% Variables:
%%  Parent - The pid of the fs_gen_tcp_recv process.
%%  CallbackModule - The user defined callback module.
%%  TCPport - The port to listen on.
%%  TCPOptions - The options to gen_tcp:listen.
%%  InitArgs - Arguments to the fs_gen_tcp_recv module.
%%  Options - OTP gen specific debugging options. (almost depricated at this point)
%% @end
%%------------------------------------------------------------------------------
listener(Parent, CallbackModule, TCPPort, TCPOptions, InitArgs, Options) ->
    {ok, ListenSocket} = listen(TCPPort, TCPOptions),
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    top_loop(ListenSocket, CallbackModule, InitArgs, Options).

top_loop(ListenSocket, CallbackModule, InitArgs, Options) ->
    receive
        {'EXIT', Pid, normal} -> 
            error_logger:error_msg("fs_gen_tcp_recv:tcp_loop normal ~n"),
            top_loop(ListenSocket, CallbackModule, InitArgs, Options);
        {'EXIT', Pid, Reason} -> 
            error_logger:error_msg("fs_gen_tcp_recv:tcp_loop abnormal exit ~p~n", [Reason]),
            top_loop(ListenSocket, CallbackModule, InitArgs, Options)
    after
        0 -> 
            gen:start(fs_gen_tcp_recv, link, CallbackModule, {ListenSocket, InitArgs}, Options),
            top_loop(ListenSocket, CallbackModule, InitArgs, Options)
    end.
            

%%------------------------------------------------------------------------------
%% @hidden
%% Initiate the new process to accept on a socket.
%% Register the name using the Rfunc function
%% Calls the Mod:init/Args function.
%% Finally an acknowledge is sent to Parent and the main
%% loop is entered.
%% 
%% Variables:
%%  Starter - The original calling process.
%%  Parent - The pid of the fs_gen_tcp_recv process.
%%  Name - 
%%  Mod - The user defined callback module.
%%  Args - Arguments to the fs_gen_tcp_recv module.
%%  Options - OTP gen specific debugging options. (almost depricated at this point)
%%
%% Types:
%%  Mod = atom()
%%  Args  =  {ListenSocket, Args}
%% @end
%%------------------------------------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name, Mod, {ListenSocket, InitArgs}, Options) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    proc_lib:init_ack(Starter, {ok, self()}),
    case catch apply(Mod, init, [InitArgs]) of
	{ok, UserState} ->
 	    loop(Socket, UserState, Mod, infinity);
	{ok, UserState, Timeout} ->
 	    loop(Socket, UserState, Mod, Timeout);
	{stop, Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
            error_logger:info_msg("fs_gen_tcp_recv:init_it 'EXIT'"), 
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
            error_logger:info_msg("fs_gen_tcp_recv:init_it bad return value ~p", [Else]), 
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

%%%============================================================
%%% Internal Functions
%%%-============================================================

%%------------------------------------------------------------------------------
%% Set up a listen socket.
%% Returns: {ok, ListenSocket} | {error, Reason}
%%------------------------------------------------------------------------------
listen (TCPPort, []) -> 
    TCPOptions = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
    gen_tcp:listen (TCPPort, TCPOptions);

listen (TCPPort, TCPOptions) ->
    % Set the active option to true.
    NewTCPOptions = [{active, true} | lists:delete ({active, false}, TCPOptions)],
    gen_tcp:listen (TCPPort, NewTCPOptions).


%%------------------------------------------------------------------------------
%% Main control loop for procecess manageing an accepted socket.
%%------------------------------------------------------------------------------
loop(Socket, UserState, CallbackModule, Timeout) ->
    LoopStartTime = now(),
    receive
        {tcp, Socket, Data} ->
            case CallbackModule:handle_packet(Socket, Data, UserState) of
                {reply, Reply, NewUserState} -> 
                    gen_tcp:send(Socket, Reply),
                    loop(Socket, NewUserState, CallbackModule, infinity);
                {reply, Reply, NewUserState, NewTimeout} -> 
                    gen_tcp:send(Socket, Reply),
                    loop(Socket, NewUserState, CallbackModule, NewTimeout);
                {noreply, NewUserState} -> 
                    loop(Socket, NewUserState, CallbackModule, infinity);
                {noreply, NewUserState, NewTimeout} -> 
                    loop(Socket, NewUserState, CallbackModule, NewTimeout);
                {stop, Reason, NewUserState} -> 
                    CallbackModule:terminate(Socket, NewUserState),
                    catch gen_tcp:close(Socket),
                    exit(Reason)
            end;
        {tcp_closed, Socket} ->
            error_logger:info_msg("fs_gen_tcp_recv:loop tcp_closed~n"),
            CallbackModule:terminate(Socket, UserState);
        {tcp_error, Socket, Reason} ->
            error_logger:error_msg("fs_gen_tcp_recv:loop tcp_error ~p~n", [Reason]),
            NewTimeout = up_to_zero(Timeout - (fs_time:epoch_elapsed(LoopStartTime, now()) / 1000)),
            loop(Socket, UserState, CallbackModule, NewTimeout);
        {?TAG, From, Msg} ->
            case CallbackModule:handle_call(Socket, From, Msg, UserState) of
                {reply, Reply, NewUserState} -> 
                    gen:reply(From, Reply),
                    loop(Socket, NewUserState, CallbackModule, infinity);
                {reply, Reply, NewUserState, NewTimeout} -> 
                    gen:reply(From, Reply),
                    loop(Socket, NewUserState, CallbackModule, NewTimeout);
                {noreply, NewUserState} -> 
                    loop(Socket, NewUserState, CallbackModule, infinity);
                {noreply, NewUserState, NewTimeout} -> 
                    loop(Socket, NewUserState, CallbackModule, NewTimeout);
                {stop, Reason, NewUserState} -> 
                    CallbackModule:terminate(Socket, NewUserState),
                    catch gen_tcp:close(Socket),
                    exit(Reason);
                {stop, Reason, Reply, NewUserState} -> 
                    gen:reply(From, Reply),
                    CallbackModule:terminate(Socket, NewUserState),
                    catch gen_tcp:close(Socket),
                    exit(Reason)
            end;
        Other ->
            case CallbackModule:handle_info(Socket, Other, UserState) of
                {noreply, NewUserState} -> 
                    loop(Socket, NewUserState, CallbackModule, infinity);
                {noreply, NewUserState, NewTimeout} -> 
                    loop(Socket, NewUserState, CallbackModule, NewTimeout);
                {stop, Reason, NewUserState} -> 
                    CallbackModule:terminate(Socket, NewUserState),
                    catch gen_tcp:close(Socket),
                    exit(Reason)
            end
    after
        Timeout -> 
            case CallbackModule:handle_info(Socket, timeout, UserState) of
                {noreply, NewUserState} -> 
                    loop(Socket, NewUserState, CallbackModule, infinity);
                {noreply, NewUserState, NewTimeout} -> 
                    loop(Socket, NewUserState, CallbackModule, NewTimeout);
                {stop, Reason, NewUserState} -> 
                    catch gen_tcp:close(Socket),
                    CallbackModule:terminate(Socket, NewUserState),
                    exit(Reason)
                                                   
            end
    end.

up_to_zero(Number) when Number < 0 -> 0;
up_to_zero(Number)                 -> Number.












