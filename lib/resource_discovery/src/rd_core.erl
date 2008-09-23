%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%   Cache and distribute resources.
%%% @end
%%% Created : 17 Sep 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(rd_core).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 store_resources/2,
	 store_resources/1,
	 async_inform/3,
	 inform/4,
	 inform/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("macros.hrl").

-define(SERVER, ?MODULE). 

-record(state, {resources, callback_list}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Cache resources for future use.
%% @spec (Node, Resources) -> ok
%% @end
%%--------------------------------------------------------------------
store_resources(Node, Resources) ->
    gen_server:cast({?SERVER, Node}, {store_resources, Resources}).

%% @spec (Resources) -> ok
%% @equiv (node(), Resources) 
store_resources(Resources) ->
    store_resources(node(), Resources).

%%--------------------------------------------------------------------
%% @doc inform an rd_core server of local resources and target types.
%%      This returns with a list of resources from the remove server 
%%      that match the target types specified.
%% @spec (Node, TargetTypes, LocalResources, Timeout) -> {ok, RemoteResources}
%% where
%%  TargetTypes = [resource_type()]
%%  LocalResources = [resource()]
%%  RemoteResources = [resource()]
%% @end
%%--------------------------------------------------------------------
inform(Node, TargetTypes, LocalResources, Timeout) ->
    ?INFO_MSG("inform ~p of target types ~p and local resources ~p~n", [Node, TargetTypes, LocalResources]),
    gen_server:call({?SERVER, Node}, {inform, {TargetTypes, LocalResources}}, Timeout).

%% @spec (Node, TargetTypes, LocalResources) -> {ok, RemoteResources}
%% @equiv (Node, TargetTypes, LocalResources, 60000) 
inform(Node, TargetTypes, LocalResources) ->
    inform(Node, TargetTypes, LocalResources, 60000).

%%--------------------------------------------------------------------
%% @doc inform an rd_core server of local resources and target types.
%%      This will prompt the remote servers to asyncronously send
%%      back remote resource information.
%% @spec (Node, TargetTypes, LocalResources) -> ok
%% where
%%  TargetTypes = [resource_type()]
%%  LocalResources = [resource()]
%%  RemoteResources = [resource()]
%% @end
%%--------------------------------------------------------------------
async_inform(Node, TargetTypes, LocalResources) ->
    ?INFO_MSG("async inform ~p of target types ~p and local resources ~p~n", [Node, TargetTypes, LocalResources]),
    gen_server:cast({?SERVER, Node}, {async_inform, {node(), TargetTypes, LocalResources}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    case resource_discovery:contact_nodes() of
	no_contact_node -> ok;
	pong            -> ok
    end,
    
    {ok, #state{resources = rd_store:lookup_resource_struct(), callback_list = rd_store:lookup_callback_modules()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_resource, Type}, _From, #state{resources = Resources} = State) ->
    case rd_store:round_robin_lookup(Type, Resources) of 
	{ok, {Resource, NewResources}} -> {reply, {ok, Resource}, State#state{resources = NewResources}};
	{error, _}                     -> {reply, {error, no_resources}, State}
    end;
handle_call({get_resource, Type, Index}, _From, #state{resources = Resources} = State) ->
    case rd_store:index_lookup(Type, Index, Resources) of 
	{ok, Resource} -> {reply, {ok, Resource}, State};
	{error, _}     -> {reply, {error, no_resources}, State}
    end;
handle_call({get_all_resources, Type}, _From, State) ->
    {reply, {ok, rd_store:lookup_all_resources(Type, State#state.resources)}, State};
handle_call({get_num_resource, Type}, _From, State) ->
    {reply, {ok, rd_store:lookup_num_resources(Type, State#state.resources)}, State};

%% Get the number of types on the network 
handle_call(get_num_types, _From, State) ->
    {reply, {ok, rd_store:lookup_num_types(State#state.resources)}, State};

%% Get a list of types that we have.
handle_call(get_types, _From, State) ->
    {reply, {ok, rd_store:lookup_types(State#state.resources)}, State};

handle_call({delete_resource, {Type, Instance}}, _From, #state{resources = Resources} = State) ->
    ?INFO_MSG("remove resource:: ~p~n", [{Type, Instance}]),
    NewResources = rd_store:delete_resource(Type, Instance, Resources),
    {reply, ok, State#state{resources = NewResources}};
handle_call({inform, {TargetTypes, RemoteResources}}, _From, #state{resources = Resources} = State) ->
    ?INFO_MSG("just informed by network of the following remote resources ~p~n", [RemoteResources]),
    NewResources = rd_store:store_resources(is_of_target_types(rd_store:lookup_target_types(), RemoteResources), Resources),
    make_callbacks(rd_store:lookup_callback_modules(), NewResources),
    {reply, {ok, is_of_target_types(TargetTypes, rd_store:lookup_local_resources())}, State#state{resources = NewResources}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({store_resources, RemoteResources}, #state{resources = Resources} = State) ->
    ?INFO_MSG("storing remote resources ~p~n", [RemoteResources]),
    NewResources = rd_store:store_resources(is_of_target_types(rd_store:lookup_target_types(), RemoteResources), Resources),
    make_callbacks(rd_store:lookup_callback_modules(), RemoteResources),
    {noreply, State#state{resources = NewResources}};
handle_cast({async_inform, {FromNode, TargetTypes, RemoteResources}}, #state{resources = Resources} = State) ->
    ?INFO_MSG("just informed async from ~p of the following remote resources ~p~n", [FromNode, RemoteResources]),
    NewResources = rd_store:store_resources(is_of_target_types(rd_store:lookup_target_types(), RemoteResources), Resources),
    make_callbacks(rd_store:lookup_callback_modules(), NewResources),
    ReturnResources = is_of_target_types(TargetTypes, rd_store:lookup_local_resources()),
    store_resources(FromNode, ReturnResources),
    {noreply, State#state{resources = NewResources}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc return a list of resources that have a resource_type() found in the target
%%      types list.
%% @end
is_of_target_types(TargetTypes, Resources) ->
    Fun = 
	fun({Type, _Instance} = Resource, Acc) ->
		case lists:member(Type, TargetTypes) of
		    true  -> [Resource|Acc];
		    false -> Acc
		end
	end,
    lists:foldl(Fun, [], Resources).

%% @private
%% @doc call each callback function for each new resource in its own process.
make_callbacks(CallBackModules, NewResources) ->
    lists:foreach(
      fun(Module) ->
	      lists:foreach( fun(Resource) -> spawn(fun() -> Module:resource_up(Resource) end) end, NewResources)
      end,
      CallBackModules).

					 
