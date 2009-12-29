%%%-------------------------------------------------------------------
%%% @author Martin Logan and Victoria Kulikou 
%%% 
%%%
%%% @doc  Monitors files on a time interval.
%%% <pre>
%%% Callback Functions:
%%%  These functions should be placed in a call back module. Filemon is really
%%%  an informal behaviour. 
%%%
%%%  M:init(Filename, State) returns: {ok, NewState} is optional it is to 
%%%  initialize a particular subscription within the process space of filemon. 
%%%  M and State come from the initial subscription (see start_link or
%%%  add_subscription)
%%%
%%%  M:F(FileName, FileInfo, State) callback function as specified 
%%%  in a subscription should return {ok, NewState} where NewState = term().
%%% </pre>
%%% @end
%%%-------------------------------------------------------------------

-module (fs_filemon).

-behaviour (gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include ("macros.hrl").
%-include ("kernel/include/file.hrl").
-include ("file.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export([
         start/1, 
         start/2, 
         start/3, 
         start_link/1, 
         start_link/2, 
         start_link/3, 
         stop/1,
         subscriber/3,
         subscriber/2,
         add_subscription/3,
         delete_subscription/2
        ]).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------

-record (state, {}).

%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define (TIMEOUT, 10000).	% Default filestat probe interval.

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% <pre>
%% Variables:
%%  Server - The locally registered name of the server to be started.
%%  Subscriptions - A list of filenames and pids
%%  Timeout - The time inbetween checking target files. In miliseconds. 
%%            Defaults to one minute.
%%  F - Function Name. It must have the following argument signature 
%%      M:F(FileName, FileStats)
%%       FileStats =  {modtime, NewFileStats} | {error, Reason}
%%        NewFileStats = {Year, Month, Day, Hour, Minute, Second}
%%  State - User state.
%% 
%% Types:
%%  Server = atom()
%%  Subscriptions = [Subscription]
%%   Subscription = {FileName, Subscribers}
%%    FileName = string() | atom()
%%    Subscribers = {M, F, State} | [{M, F, State}]
%%     M = F = atom()
%%     State = term()
%%  Timeout = integer()
%% </pre>   
%% @spec start_link(Server, Subscriptions, Timeout) -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link (Server, Subscriptions, Timeout) ->
    gen_server:start_link ({local, Server}, ?MODULE, [Subscriptions, Timeout], []).

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% <pre>
%% Variables:
%%  Subscriptions - A list of filenames and pids
%%  Timeout - The time inbetween checking target files. In miliseconds. 
%%            Defaults to one minute.
%%  F - Function Name. It must have the following argument signature 
%%      M:F(FileName, FileStats)
%%       FileStats =  {modtime, NewFileStats} | {error, Reason}
%%        NewFileStats = {Year, Month, Day, Hour, Minute, Second}
%%  State - User state.
%% 
%% Types:
%%  Subscriptions = [Subscription]
%%   Subscription = {FileName, Subscribers}
%%    FileName = string() | atom()
%%    Subscribers = {M, F, State} | [{M, F, State}]
%%     M = F = atom()
%%     State = term()
%%  Timeout = integer()
%% </pre>   
%% @spec start_link(Subscriptions, Timeout) -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link (Subscriptions, Timeout) ->
    gen_server:start_link (?MODULE, [Subscriptions, Timeout], []).

%% @spec start_link(Subscriptions) -> {ok, pid()} | {error, Reason}
%% @equiv start_link(Subscriptions, infinity)

start_link (Subscriptions) ->
    gen_server:start_link (?MODULE, [Subscriptions, ?TIMEOUT], []).

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% <pre>
%% Variables:
%%  Server - The locally registered name of the server to be started.
%%  Subscriptions - A list of filenames and pids
%%  Timeout - The time inbetween checking target files. In miliseconds. 
%%            Defaults to one minute.
%%  F - Function Name. It must have the following argument signature 
%%      M:F(FileName, FileStats)
%%       FileStats =  {modtime, NewFileStats} | {error, Reason}
%%        NewFileStats = {Year, Month, Day, Hour, Minute, Second}
%%  State - User state.
%% 
%% Types:
%%  Server = atom()
%%  Subscriptions = [Subscription]
%%   Subscription = {FileName, Subscribers}
%%    FileName = string() | atom()
%%    Subscribers = {M, F, State} | [{M, F, State}]
%%     M = F = atom()
%%     State = term()
%%  Timeout = integer()
%% </pre>   
%% @spec start(Server, Subscriptions, Timeout) -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------

start(Server, Subscriptions, Timeout) ->
    gen_server:start_link ({local, Server}, ?MODULE, [Subscriptions, Timeout], []).

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% <pre>
%% Variables:
%%  Subscriptions - A list of filenames and pids
%%  Timeout - The time inbetween checking target files. In miliseconds. 
%%            Defaults to one minute.
%%  F - Function Name. It must have the following argument signature 
%%      M:F(FileName, FileStats)
%%       FileStats =  {modtime, NewFileStats} | {error, Reason}
%%        NewFileStats = {Year, Month, Day, Hour, Minute, Second}
%%  State - User state.
%% 
%% Types:
%%  Subscriptions = [Subscription]
%%   Subscription = {FileName, Subscribers}
%%    FileName = string() | atom()
%%    Subscribers = {M, F, State} | [{M, F, State}]
%%     M = F = atom()
%%     State = term()
%%  Timeout = integer()
%% </pre>   
%% @spec start(Subscriptions, Timeout) -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------

start(Subscriptions, Timeout) ->
    gen_server:start_link (?MODULE, [Subscriptions, Timeout], []).

%% @spec start(Subscriptions) -> {ok, pid()} | {error, Reason}
%% @equiv start(Subscriptions, infinity)

start(Subscriptions) ->
    gen_server:start_link (?MODULE, [Subscriptions, ?TIMEOUT], []).

%%--------------------------------------------------------------------
%% @doc Add a subscription.
%% <pre>
%% Variables:
%%  Server - The token referencing this filemon process.
%%  FileName - The file name to monitor.
%%  Subscribers - The processes that wish to receive notification.
%%  M - The module for the callback
%%  F - Function Name. It must have the following argument signature 
%%      M:F(FileName, FileStats)
%%       FileStats =  {modtime, NewFileStats} | {error, Reason}
%%        NewFileStats = {Year, Month, Day, Hour, Minute, Second}
%%  State - User state
%% 
%% Types:
%%  FileName = string() | atom()
%%    Subscribers = {M, F, State} | [{M, F, State}]
%%     M = F = atom()
%%     State = term()
%% </pre>   
%% @spec add_subscription(Server, FileName, Subscribers) -> ok
%% @end
%%--------------------------------------------------------------------
add_subscription (Server, FileName, Subscribers) ->
    gen_server:call (Server, {add_subscription, FileName, Subscribers}).

    
%%--------------------------------------------------------------------
%% @doc Delete a subscription.
%% <pre>
%% Variables:
%%  Server - The token referencing this filemon process.
%%  FileName - The file name for the subscription.
%% 
%% Types:
%%  FileName = string() | atom()
%% </pre>   
%% @spec delete_subscription(Server, FileName) -> ok
%% @end
%%--------------------------------------------------------------------
delete_subscription (Server, FileName) ->
    gen_server:cast (Server, {delete_subscription, FileName}).
    

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% <pre>
%% Variables:
%%  Server - The token referencing this filemon process.
%% 
%% Types:
%%  FileName = string() | atom()
%% </pre>   
%% @spec stop(Server) -> ok
%% @end
%%--------------------------------------------------------------------
stop (Server) ->
    gen_server:cast (Server, stop).

%%--------------------------------------------------------------------
%% @doc Makes a new subscriber.
%% <pre>
%% Variables:
%%  Timeout - The time inbetween checking target files. In miliseconds. 
%%            Defaults to one minute.
%%  F - Function Name. It must have the following argument signature i
%%      M:F(FileName, FileStats)
%%       FileStats =  {modtime, NewFileStats} | {error, Reason}
%%        NewFileStats = {Year, Month, Day, Hour, Minute, Second}
%%  State - User state.
%% 
%% Types:
%%  FileName = string() | atom()
%%  Subscribers = {M, F, State} | [{M, F, State}]
%%   M = F = atom()
%%   State = term()
%%  Timeout = integer()
%% </pre>   
%% @spec subscriber(M, F, State) -> Subscription
%% @end
%%--------------------------------------------------------------------

subscriber (M, F, State) -> {M, F, State}.

%% @spec subscriber(M, F) -> Subscription
%% @equiv subscriber(M, F, [])

subscriber (M, F) -> {M, F, []}.

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

init ([Subscriptions, Timeout]) ->
    %%error_logger:info_msg ("fs_filemon:init ~p~n", [Subscriptions]),
    FileSpecList = get_initial_time (init_subscribers(Subscriptions)),
    {ok, {FileSpecList, Timeout}, Timeout}.

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

handle_call ({add_subscription, FileName, RawSubscribers}, From, {FileSpecList, Timeout}) ->
    Subscribers = init_subscribers(FileName, RawSubscribers),
    ModTime     = get_mod_time (FileName),
    %%error_logger:info_msg ("fs_filemon:handle_call add subscription ~p ~p~n", [FileName, ModTime]),
    {reply, ok, {[{FileName, ModTime, Subscribers} | FileSpecList], Timeout}, Timeout}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast ({delete_subscription, FileName}, {FileSpecList, Timeout}) ->
    {noreply, {lists:keydelete (FileName, 1, FileSpecList), Timeout}, Timeout};

handle_cast (stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_info (timeout, {FileSpecList, Timeout}) ->
    NewFileSpecList = check_mod (FileSpecList),
    %%error_logger:info_msg ("fs_filemon:handle_info timeout ~p~n", [NewFileSpecList]),
    {noreply, {NewFileSpecList, Timeout}, Timeout}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------

terminate (Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change (OldVsn, State, Extra) ->
    {ok, State}.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Populates a list with {FileName, FileStats, Subscribers} tuples.
%%--------------------------------------------------------------------

get_initial_time ([{FileName, Subscribers}|T]) ->
    [{FileName, get_mod_time (FileName), Subscribers} | get_initial_time (T)];
get_initial_time ([{FileName, FileStats, Subscribers} = Subscription | T]) ->
    [Subscription | get_initial_time (T)];
get_initial_time ([]) -> 
    [].

%%--------------------------------------------------------------------
%% Initialises subscriptions if the calback module has exported init/2
%%--------------------------------------------------------------------

init_subscribers ([{FileName, Subscribers}|T]) ->
    [{FileName, init_subscribers(FileName, Subscribers)} | init_subscribers (T)];
init_subscribers ([{FileName, FileStats, Subscribers} = Subscription | T]) ->
    [Subscription | init_subscribers (T)];
init_subscribers ([]) -> 
    [].


init_subscribers(FileName, [{M, F, State}|T]) ->
    [init_subscribers(FileName, {M, F, State}) |init_subscribers(FileName, T)];
init_subscribers(FileName, []) ->
    [];

init_subscribers(FileName, {M, F, State} = Subscriber) ->
    case fs_lists:get_val(init, fs_lists:get_val(exports, M:module_info())) of
	2 ->
	    case catch M:init(FileName, State) of
		{ok, NewState} -> 
		    {M, F, NewState};
		Error -> 
		    error_logger:error_msg("fs_filemon:init_subscribers failed to init ~p because ~p~n", [Subscriber,Error]),
		    Subscriber
	    end;
	_ ->
	    Subscriber
    end.
            

%%--------------------------------------------------------------------
%% Retreives a files modification time.
%%--------------------------------------------------------------------

get_mod_time (FileName) ->
    % read_file_info returns: {Size,Type,Access,AccessTime,ModifyTime,UnUsed1,UnUsed2}
    case file:read_file_info (FileName) of
        {ok, FileInfo}  -> {modtime, FileInfo#file_info.mtime};
        {error, Reason} -> {error, Reason}
    end.
    
%%--------------------------------------------------------------------
%% check_mod checks to see if a files modification time has been
%% altered. If true the subscribers associated with this file are notified.
%% ** Side Effect **
%% Messages are send directly from this function!!
%%--------------------------------------------------------------------

check_mod ([{FileName, FileStats, Subscribers} = FileSpec | T]) ->
    NewFileStats = 
        case file:read_file_info (FileName) of
            {ok, FileInfo}  -> {modtime, FileInfo#file_info.mtime};
            {error, Reason} -> {error, Reason}
        end,

    case NewFileStats of
        FileStats -> 
            [FileSpec | check_mod (T)];

        _ ->
	    %%error_logger:info_msg ("fs_filemon file changed, file: ~p  stats: ~p~n", [FileName, NewFileStats]),
            NewSubscribers = notify_subscribers (FileName, NewFileStats, Subscribers),
            [{FileName, NewFileStats, NewSubscribers} | check_mod (T)]
    end;
            
check_mod ([]) -> 
    [].
        
            
%%--------------------------------------------------------------------
%% Notifies all subscribers of a file modification.
%%--------------------------------------------------------------------

notify_subscribers (FileName, NewFileStats, [Subscriber | Subscribers]) ->
    [notify (Subscriber, FileName, NewFileStats) | notify_subscribers (FileName, NewFileStats, Subscribers)];
notify_subscribers (FileName, NewFileStats, []) ->
    [];

notify_subscribers (FileName, NewFileStats, Subscriber) ->
    notify (Subscriber, FileName, NewFileStats).

notify ({M, F, State}, FileName, NewFileStats) ->
    case catch M:F (FileName, NewFileStats, State) of
        {ok, NewState} ->
	    {M, F, NewState};

        Error ->
	    error_logger:error_msg ("fs_filemon notify could not notify subscriber: ~p~n", [Error]),
	    []
    end.

