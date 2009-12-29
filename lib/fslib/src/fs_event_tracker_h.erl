%%%-------------------------------------------------------------------
%%% File    : fs_event_tracker_h.erl
%%% Author  : Martin J Logan <mlogan@linux.local>
%%%
%%% @doc  Track statistics for directory_service.
%%% @end
%%%
%%% Created :  7 Aug 2003 by Victoria V Kulikou <vint@linux.local>
%%%-------------------------------------------------------------------
-module(fs_event_tracker_h).

-behaviour(gen_event).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/1, 
         start_link/0, 
         clear/0, 
         clear/1, 
         add_handler/0, 
         add_handler/1, 
	 get_events/1,
	 get_events/0,
         check_event/1, 
         check_event/2, 
         configuration_spec/1, 
         subscribe/4, 
         subscribe/5, 
         send_event/1,
         send_event/2
	]).

%%--------------------------------------------------------------------
%% gen_event callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Record Definitions
%%--------------------------------------------------------------------
-record(state, {access_count, subscriptions}).

%%--------------------------------------------------------------------
%% Macro Definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | {error, {already_started, Pid}}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Result = gen_event:start_link({local, ?SERVER}),
    ok = add_handler(),
    Result.

%%--------------------------------------------------------------------
%% @doc Starts the server for gas.
%% @spec start_link(Configuration) -> 
%%  {ok, Pid} | {error, {already_started, Pid}}
%% @end
%%--------------------------------------------------------------------
start_link([]) -> start_link().

%%--------------------------------------------------------------------
%% @doc Adds an event handler.
%% @spec add_handler() -> ok | {'EXIT', Reason}
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Adds an event handler to a specific manager.
%% <pre>
%% Note * 
%%  {ok, ManagerPID} is returned to make this call supervisable. 
%% 
%% Types:
%%  Manager = pid() | atom()
%%  ManagerPID = pid()
%% </pre>
%% @spec add_handler(Manager) -> {ok, ManagerPID} | {'EXIT', Reason}
%% @end
%%--------------------------------------------------------------------
add_handler(Manager) ->
    ok = gen_event:add_handler(Manager, ?MODULE, []),
    {ok, whereis(Manager)}.

%%--------------------------------------------------------------------
%% @doc Sends an Event to Manager.
%% <pre>
%% Types:
%%   Event  = term()
%% </pre>
%% @spec send_event(Manager, Event) -> ok
%% @end
%%--------------------------------------------------------------------
send_event(Manager, Event) ->
    % XXX This catch is a result of an error in OTP. I sent 
    % a message to the list and I hope they fix it.
    case catch gen_event:notify(Manager, {send_event, Event}) of
    	_ -> ok
    end.

%% @spec send_event(Event) -> ok
%% @equiv send_event(SERVER, Event)
send_event(Event) -> send_event(?SERVER, Event).

%%--------------------------------------------------------------------
%% @doc Clears all event counts.
%% @spec clear(Manager) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
clear(Manager) -> gen_event:call(Manager, ?MODULE, clear).

%% @spec clear() -> ok | exit()
%% @equiv clear(SERVER)
clear() -> clear(?SERVER).

%%--------------------------------------------------------------------
%% @doc Subscribes to notification upon the receipt of an event.
%% <pre>
%% Variables:
%%  Module - Callback module.
%%  Function - Callback function.
%%  Args = a list of arguments that will be applied to Module:Function
%%         as the second argument.
%% 
%% Example Callback:
%%  subscription - subscribe(Manager, event, mod, func, [a1, a2])
%%  callback     - mod:func(event, [a1, a2])
%%
%% Types:
%%   Event  = term()
%%   Module = Function = atom
%%   Args = [term()]
%% </pre>
%% @spec subscribe(Manager, Event, Module, Function, Args) -> ok
%% @end
%%--------------------------------------------------------------------
subscribe(Manager, Event, Module, Function, Args) ->
    gen_event:call(Manager, ?MODULE, {subscribe, Event, Module, Function, Args}).

%% @spec subscribe(Event, Module, Function, Args) -> ok
%% @equiv subscribe(MANAGER, Event, Module, Function, Args)
subscribe(Event, Module, Function, Args) ->
    subscribe(?SERVER, Event, Module, Function, Args).

%%--------------------------------------------------------------------
%% @doc Return a list of the events the server knows about.
%% <pre>
%% Types:
%%  Events = [term()]
%% </pre>
%% @spec get_events(Manager) -> {ok, Events} | exit()
%% @end
%%--------------------------------------------------------------------
get_events(Manager) -> gen_event:call(Manager, ?MODULE, get_events).

%% @spec get_events() -> {ok, Events} | exit()
%% @equiv get_events(MANAGER)
get_events() -> get_events(?SERVER).

%%--------------------------------------------------------------------
%% @doc Checks the count of an event.
%% <pre>
%% Types:
%%   Event  = term()
%% </pre>
%% @spec check_event(Manager, Event) -> {ok, Value} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_event(Manager, Event) ->
    case catch gen_event:call(Manager, ?MODULE, {check_event, Event}) of
        {ok, Value} -> 
            {ok, Value};
        error -> 
            error_logger:info_msg("fs_event_tracker_h:check_event asked for event that does not exist. ~p~n", [Event]),
            {ok, 0};
        {'EXIT', Reason} -> 
            {error, Reason}
    end.

%% @spec check_event(Event) -> {ok, Value} | exit()
%% @equiv check_event(SERVER, Event)
check_event(Event) -> check_event(?SERVER, Event).

%%--------------------------------------------------------------------
%% @doc Returns the keys, required or optional, used for configuration of this process.
%% <pre>
%% Conforms to the GAS behaviour.
%% Variables:
%%  Function - The function that the config spec pertains to.
%%
%% Configuration:
%%  fset_manager - The name of the event manager to be added to.
%% 
%% Configuration Types:
%%     {fset_manager, atom()}
%% </pre>
%% @spec configuration_spec(Function) -> ConfigurationSpec
%% @end
%%--------------------------------------------------------------------
configuration_spec(start_link)  -> [];
configuration_spec(add_handler) -> [{required, fset_manager}].


%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{access_count = dict:new(), subscriptions = dict:new()}}.

%%--------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_event({send_event, Event}, #state{access_count = AC, subscriptions = Subs} = State) ->
    % Call all subscribed functions.
    case dict:find(Event, Subs) of
        error        -> ok;
        {ok, MFList} -> lists:foreach(fun({M, F, A}) -> M:F(Event, A) end, MFList)
    end,
    NewAC = dict:update(Event, fun(Count) -> Count + 1 end, 1, AC),
    {ok, State#state{access_count = NewAC}}.

%%--------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%--------------------------------------------------------------------
handle_call({subscribe, Event, M, F, A}, #state{access_count = AC, subscriptions = Subs} =  State) ->
    {ok, ok, State#state{subscriptions = dict:append(Event, {M, F, A}, Subs)}};
handle_call({check_event, Event}, #state{access_count = AC} =  State) ->
    Reply = case dict:find(Event, AC) of
        error  -> {ok, 0};
        Reply_ -> Reply_
    end,
    {ok, Reply, State};
handle_call(get_events, #state{access_count = AC} =  State) ->
    {ok, {ok, dict:fetch_keys(AC)}, State};
handle_call(clear, #state{access_count = AC} =  State) ->
    {ok, ok, #state{access_count = dict:new(), subscriptions = dict:new()}}.

%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
