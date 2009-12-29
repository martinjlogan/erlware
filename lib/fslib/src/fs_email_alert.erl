%%% $Id: fs_email_alert.erl,v 1.2 2003/08/08 17:31:20 vkulikou Exp $
%%%-------------------------------------------------------------------
%%% File    : fs_email_alert.erl
%%% Author  : Martin J. Logan <martin@localhost.localdomain>
%%%
%%% @doc  Sends email on alert events.
%%% @end
%%%
%%% Created : 25 Apr 2003 by Martin J. Logan <martin@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(fs_email_alert).

-behaviour(gen_event).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/1, 
         start_link/2, 

         add_handler/1,
         add_handler/2,

         configuration_spec/1,

         info_msg/2,
         info_msg/3,
         info_msg/4,

         error_msg/2,
         error_msg/3,
         error_msg/4,

         clear/2,
         clear/1,

         reset/2,
         reset/1
        ]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Macro Definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(INFO_SUBJ, "Info Message").
-define(ERROR_SUBJ, "Error Message").

%%--------------------------------------------------------------------
%% Record Definitions
%%--------------------------------------------------------------------
-record(state, {error_recipients, info_recipients, app_name, from, edge_trigger}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%% <pre>
%% Variables:
%%  Manager - The manager name for the manager to start.
%%  Options - A list of tuples as perscribed by configuration_spec.
%% </pre>
%% @spec start_link(Manager, Options) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link(Manager, Options) ->
    {ok, Pid} = gen_event:start_link({local, Manager}),
    ok        = add_handler(Options),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc Starts the server with a custom server name for reentrancy.
%% <pre>
%% Variables:
%%  Options - A list of tuples as perscribed by configuration_spec
%% </pre>
%% @spec start_link(Options) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    {ok, Pid} = gen_event:start_link({local, ?SERVER}),
    ok        = add_handler(Options),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc Adds an event handler.
%% <pre>
%% Variables:
%%  Manager - The manager name for the manager to add to.
%%  Options - A list of tuples as perscribed by configuration_spec.
%% </pre>
%% @spec add_handler(Manager, Options) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
add_handler(Manager, Options) ->
    gen_event:add_handler(?SERVER, ?MODULE, Options).

%% @spec add_handler(Options) -> {ok, Pid}
%% @equiv add_handler(fs_email_alert, Options)
add_handler(Options) ->
    gen_event:add_handler(?SERVER, ?MODULE, Options).


%%--------------------------------------------------------------------
%% @doc Returns the keys, required or optional, used for configuration of this process.
%% <pre>
%% Returns the configuration_spec for this module. Used to conform to the GAS behaviour.
%%
%% Variables:
%%  Function - The function that configuration_spec pertains to.
%%
%% Types:
%%  ConfigurationSpec = [Spec]
%%   Spec = {optional, ConfToken} | {required, ConfToken}
%%    ConfToken = {App, Key} | Key
%%  Function = atom()  
%%
%% Configuration:
%%  info_recipients - A list of recipients for info messages. 
%%  error_recipients - A list of recipients for error messages. 
%%  edge_triggered - Specify whether an alarm is triggered every time
%%                               a message is seen or just on the edge.
%%  app_name - Specify a custom app name.
%%
%% Configuration Types:
%%  info_recipients = [string()]
%%  error_recipients = [string()]
%%  edge_triggered = bool()
%%  app_name = atom() 
%% </pre>
%% @spec configuration_spec(Function) -> CongifurationSpec
%% @end
%%--------------------------------------------------------------------
configuration_spec(start_link) ->
    [{required, info_recipients},
     {required, error_recipients},
     {optional, edge_triggered},
     {optional, app_name}].

%%--------------------------------------------------------------------
%% @doc Sends an info message.
%% <pre>
%% The subject supplied by by the provided subject, or a default
%% if none is supplied.
%%
%% Variables:
%%  Manager - The manager referance for the manager to notify.
%%  Tag - A tag to identify this event.
%%  Subj - The subject of the message.
%%  Msg - The Msg for notification.
%%
%% Types:
%%  Manager =  {local,Name} | {global,Name} | pid()
%%    Name = atom()
%%  Tag = term()
%%  Subj = string()
%%  Msg = term()
%% </pre>
%% @spec info_msg(Manager, Tag, Subj, Msg) -> ok
%% @end
%%--------------------------------------------------------------------
info_msg(Manager, Tag, Subj, Msg) ->
    gen_event:notify(Manager, {info, {Tag, Subj, Msg}}).

%% @spec info_msg(Tag, Subj, Msg) -> ok
%% @equiv info_msg(fs_email_alert, Tag, Subj, Msg)
info_msg(Tag, Subj, Msg) when is_list(Subj) ->
    gen_event:notify(?SERVER, {info, {Tag, Subj, Msg}});

%% @equiv info_msg(Manager, Tag, INFO_SUBJECT, Msg)
%% @spec info_msg(Manager, Tag, Msg) -> ok
info_msg(Manager, Tag, Msg) ->
    gen_event:notify(Manager, {info, {Tag, ?INFO_SUBJ, Msg}}).

%% @equiv info_msg(fs_email_alert, Tag, INFO_SUBJECT, Msg)
%% @spec info_msg(Tag, Msg) -> ok
info_msg(Tag, Msg) ->
    gen_event:notify(?SERVER, {info, {Tag, ?ERROR_SUBJ, Msg}}).

%%--------------------------------------------------------------------
%% @doc Sends an error message.
%% <pre>
%% The subject supplied by by the provided subject, or a default
%% if none is supplied.
%%
%% Variables:
%%  Manager - The manager referance for the manager to notify
%%  Tag - A tag to identify this event.
%%  Subj - The subject of the message
%%  Msg - The message for notification
%%
%% Types:
%%  Manager =  {local,Name} | {global,Name} | pid()
%%    Name = atom()
%%  Tag = term()
%%  Subj = string()
%%  Msg = term()
%% </pre>
%% @spec error_msg(Manager, Tag, Subject, Msg) -> ok
%% @end
%%--------------------------------------------------------------------
error_msg(Manager, Tag, Subj, Msg) ->
    gen_event:notify(Manager, {error, {Tag, Subj, Msg}}).

%% @equiv error_msg(fs_email_alert, Tag, Subj, Msg)
%% @spec error_msg(Manager, Tag, Msg) -> ok
error_msg(Tag, Subj, Msg) when is_list(Subj) ->
    gen_event:notify(?SERVER, {error, {Tag, Subj, Msg}});

%% @equiv error_msg(Manager, Tag, ERROR_SUBJECT, Msg)
%% @spec error_msg(Manager, Tag, Msg) -> ok
error_msg(Manager, Tag, Msg) ->
    gen_event:notify(Manager, {error, {Tag, ?ERROR_SUBJ, Msg}}).

%% @equiv error_msg(fs_email_alert, Tag, ERROR_SUBJECT, Msg)
%% @spec error_msg(Tag, Msg) -> ok
error_msg(Tag, Msg) ->
    gen_event:notify(?SERVER, {error, {Tag, ?ERROR_SUBJ, Msg}}).

%%--------------------------------------------------------------------
%% @doc Clears the edge value for a particular tag.
%% @spec clear(Manager, Tag) -> ok
%% @end
%%--------------------------------------------------------------------
clear(Manager, Tag) ->
    gen_event:notify(Manager, {clear, Tag}).

%% @equiv clear(fs_email_alert, Tag)
%% @spec clear(Tag) -> ok
clear(Tag) ->
    gen_event:notify(?SERVER, {clear, Tag}).

%%--------------------------------------------------------------------
%% @doc Resets the edge value for a particular tag.
%% @spec reset(Manager, Tag) -> ok
%% @end
%%--------------------------------------------------------------------
reset(Manager, Tag) ->
    gen_event:notify(Manager, {reset, Tag}).

%% @equiv reset(fs_email_alert, Tag)
%% @spec reset(Tag) -> ok
reset(Tag) ->
    gen_event:notify(?SERVER, {reset, Tag}).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          Other
%%--------------------------------------------------------------------
init(Options) ->
    {value, {_, IR}} = lists:keysearch(info_recipients, 1, Options),
    {value, {_, ER}} = lists:keysearch(error_recipients, 1, Options),

    % determine whether this is to be edge triggered or not. Default to false
    % to adhere to the PRINCIPLE OF LEAST SURPRISE(TM).
    EdgeTrigger = 
        case lists:keysearch(edge_triggered, 1, Options) of
            {value, {_, true}} -> fs_edge_trigger:new();
            _                  -> false
        end,

    AppName = 
        case lists:keysearch(app_name, 1, Options) of
            {value, {_, AN}} -> atom_to_list(AN);
            _ -> 
                % Trick to use the group leader to ascertain the application name.
                case ets:match(ac_tab, {{application_master, '$1'}, group_leader()}) of
                    [[AN]] -> atom_to_list(AN);
                    _      -> []
                end
        end,
    
    From = AppName ++ "@" ++ atom_to_list(node()),

    {ok, #state{error_recipients = ER, info_recipients = IR, app_name = AppName, 
                from = From, edge_trigger = EdgeTrigger}}.

%%--------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_event({info, {Tag, Subject, Message}}, #state{edge_trigger = false} = State) ->
    Recipients = State#state.info_recipients,
    From       = State#state.from,
    fs_email:send(From, Recipients, Subject, Message),
    {ok, State};

handle_event({info, {Tag, Subject, Message}}, #state{edge_trigger = EdgeTrigger} = State) ->
    case fs_edge_trigger:edge(Tag, EdgeTrigger) of
        {true, NewEdgeTrigger} -> 
            Recipients = State#state.info_recipients,
            From       = State#state.from,
            fs_email:send(From, Recipients, Subject, Message),
            {ok, State#state{edge_trigger = NewEdgeTrigger}};
        {false, _} -> 
            {ok, State}
    end;


handle_event({error, {Tag, Subject, Message}}, #state{edge_trigger = false} = State) ->
    Recipients = State#state.error_recipients,
    From       = State#state.from,
    fs_email:send(From, Recipients, Subject, Message),
    {ok, State};

handle_event({error, {Tag, Subject, Message}}, #state{edge_trigger = EdgeTrigger} = State) ->
    case fs_edge_trigger:edge(Tag, EdgeTrigger) of
        {true, NewEdgeTrigger} -> 
            Recipients = State#state.error_recipients,
            From       = State#state.from,
            fs_email:send(From, Recipients, Subject, Message),
            {ok, State#state{edge_trigger = NewEdgeTrigger}};
        {false, _} -> 
            {ok, State}
    end;

handle_event({clear, Tag}, #state{edge_trigger = EdgeTrigger} = State) ->
    {ok, State#state{edge_trigger = fs_edge_trigger:clear(Tag, EdgeTrigger)}};

handle_event({reset, Tag}, #state{edge_trigger = EdgeTrigger} = State) ->
    {ok, State#state{edge_trigger = fs_edge_trigger:reset(Tag, EdgeTrigger)}}.

%%--------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%--------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

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




