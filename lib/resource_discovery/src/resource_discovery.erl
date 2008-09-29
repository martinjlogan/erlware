%%%-------------------------------------------------------------------
%%% File    : resource_discovery.erl
%%% Author  : Martin J. Logan <martinjlogan@erlware.org>
%%% @doc 
%%%
%%% @type resource() = {resource_type(), resource_instance()}. The type of a resource followed by its identifier. Local
%%%       resources are communicated to other resource discovery instances and cached by those how have the
%%%       local resource type set as a target type. 
%%% @type resource_type() = atom(). The name of a resource, how it is identified. For example
%%%       a type of service that you have on the network may be identified by it's node name
%%%       in which case you might have a resource type of 'my_service' of which there may be
%%%       many node names representing resources such as {my_service, myservicenode@myhost}. 
%%% @type resource_instance() =  term() | messageable_resource_instance(). The resource being managed
%%%       Typicaly something that can be messaged directly with "!" but can be any term.
%%% @type messageable_resource_instance() = {name, node()} | node() | pid(). A resource that is directly messageable via "!". 
%%% @end
%%%-------------------------------------------------------------------
-module(resource_discovery).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

% Standard exports.
-export([
         start/2
        ]).

% Add
-export([
         add_local_resources/1,
         add_target_types/1,
         add_callback_modules/1
        ]).

% Get
-export([
         get_resource/1, 
         get_resource/2, 
         get_all_resources/1, 
         get_num_resource/1, 
         get_types/0,
         get_num_types/0,
         get_contact_nodes/0
        ]).

% Delete
-export([
         delete_local_resource/1,
         delete_target_type/1,
         delete_resource/2, 
         delete_resource/1 
        ]).

% Other
-export([
         inform_network/1,
         inform_network/0,
         async_inform_network/0,
         contact_nodes/0,
         execute_with_resource/2,
         execute_with_same_resource/2,
         rpc_call_with_discovery/4
        ]).

-include("macros.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(RD, rd_core).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the resource discovery application.
%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case rd_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Adds to the list of target types. Target types are the types
%%      of resources that this instance of resource_discovery will cache following
%%      a notification of such a resource from a resource_discovery instance.
%%      This includes the local instance.
%% @spec add_target_types(TargetTypes) -> ok | {error, Reason}
%% where
%%  TargetTypes = [resource_type()] | resource_type()
%% @end
%%------------------------------------------------------------------------------
add_target_types(TargetTypes) -> 
    rd_store:store_target_types(TargetTypes).

%%------------------------------------------------------------------------------
%% @doc Adds to the list of local resources. These locally resident resources 
%%      will be broadcast out to other resource discovery instances in the current node cloud.
%% @spec add_local_resources(LocalResources) -> ok 
%% where
%%  LocalResources = [resource()] | resource() 
%% @end
%%------------------------------------------------------------------------------
add_local_resources(LocalResources) -> 
    rd_store:store_local_resources(LocalResources).

%%------------------------------------------------------------------------------
%% @doc Add a callback module or modules to the list of callbacks to be called upon new resources entering the system.
%% @spec add_callback_modules(Modules) -> ok | exit()
%% where
%%  Modules = atom() | [atom()]
%% @end
%%------------------------------------------------------------------------------
add_callback_modules(Modules) ->
    rd_store:store_callback_modules(Modules).

%%------------------------------------------------------------------------------
%% @doc Returns a cached resource.
%% @spec (Type) -> {ok, resource_instance()} | {error, no_resources}
%% @end
%%------------------------------------------------------------------------------
get_resource(Type) ->
    gen_server:call(?RD, {get_resource, Type}). 

%%------------------------------------------------------------------------------
%% @doc Replies with the cached resource at the index specified. If for example we had
%%      a list of resources that looked like {R1, R2, R3} and index 2 was
%%      requested the user would receive R2.
%% @spec (resource_type(), Index::integer()) -> {ok, resource_instance()} | {error, no_resources}
%% @end
%%------------------------------------------------------------------------------
get_resource(Type, Index) ->
    gen_server:call(?RD, {get_resource, Type, Index}). 

%%------------------------------------------------------------------------------
%% @doc Returns ALL cached resources for a particular type.
%% @spec get_all_resources(type()) -> {ok, [resource_instance()]}
%% @end
%%------------------------------------------------------------------------------
get_all_resources(Type) ->
    gen_server:call(?RD, {get_all_resources, Type}). 

%%------------------------------------------------------------------------------
%% @doc Removes a cached resource from the resource pool. Only returns after the
%%      resource has been deleted.
%% @spec delete_resource(resource_type(), resource_instance()) -> ok
%% @end
%%------------------------------------------------------------------------------
delete_resource(Type, Instance) ->
    gen_server:call(?RD, {delete_resource, {Type, Instance}}).

%% @spec delete_resource(resource()) -> ok
%% @equiv delete_resource(resource_type(), resource_instance())
delete_resource({Type, Instance}) ->
    delete_resource(Type, Instance).

%%------------------------------------------------------------------------------
%% @doc Counts the cached instances of a particular resource type.
%% @spec get_num_resource(type()) -> {ok, integer()}
%% @end
%%------------------------------------------------------------------------------
get_num_resource(Type) ->
    gen_server:call(?RD, {get_num_resource, Type}). 

%%------------------------------------------------------------------------------
%% @doc Remove a target type. Returns the number of resources deleted.
%% @spec delete_target_type(resource_type()) -> {ok, integer()}
%% @end
%%------------------------------------------------------------------------------
delete_target_type(Type) ->
    rd_store:delete_target_type(Type).

%%------------------------------------------------------------------------------
%% @doc Remove a local resource.
%% @spec delete_local_resource(resource()) -> void()
%% @end
%%------------------------------------------------------------------------------
delete_local_resource(LocalResource) ->
    rd_store:delete_local_resource(LocalResource).

%%------------------------------------------------------------------------------
%% @doc Gets a list of the types that have resources that have been cached.
%% @spec () -> [resource_type()]
%% @end
%%------------------------------------------------------------------------------
get_types() ->
    gen_server:call(?RD, get_types). 

%%------------------------------------------------------------------------------
%% @doc Gets the number of resource types locally cached.
%% @spec get_num_types() -> integer()
%% @end
%%------------------------------------------------------------------------------
get_num_types() ->
    gen_server:call(?RD, get_num_types). 

%%------------------------------------------------------------------------------
%% @doc inform the network of your existance and all the resources that are
%%      provided by the local node as well as all the target types that the local
%%      instance of Resource Discovery would like to know about. Returns only
%%      after all nodes have been transacted with.
%% @spec inform_network(Timeout) -> ok | exit(Reason)
%% where
%%  Timeout = Milliseconds::integer()
%% @end
%%------------------------------------------------------------------------------
inform_network(Timeout) ->
    Nodes = [node()|nodes()],
    ?INFO_MSG("initiating a syncronous inform network with a timeout of ~p to nodes ~p~n", [Timeout, Nodes]),
    LocalResources = rd_store:lookup_local_resources(),
    TargetTypes    = rd_store:lookup_target_types(),
    Self = self(),
    NodePidList = 
	lists:map(fun(Node) ->
			  {Node, spawn(fun() ->
					       Self ! {?MODULE,
						       Node,
						       self(),
						       rd_core:inform(Node, TargetTypes, LocalResources)}
				       end)}
		  end,
		  Nodes),
    {ok, {Resources, Errors}} = wait_on_responses(NodePidList, Timeout),

    case Errors of
	[] -> ok;
	Errors -> ?INFO_MSG("Responses returned and errors found: ~p~n", [Errors])
    end,

    rd_core:store_resources(Resources).

%% @spec inform_network() -> ok | exit(Reason)
%% @equiv inform_network(10000)
inform_network() ->
    inform_network(10000).
     

wait_on_responses(NodePidList, Timeout) ->
    wait_on_responses(NodePidList, [], [], Timeout).

wait_on_responses([], ResourceAcc, ErrorAcc, _Timeout) ->
    {ok, {ResourceAcc, ErrorAcc}};
wait_on_responses(NodePidList, ResourceAcc, ErrorAcc, Timeout) ->
    receive
	{?MODULE, Node, Pid, {ok, Resources}} -> 
	    wait_on_responses(
	      lists:delete({Node, Pid}, NodePidList),
	      lists:flatten([ResourceAcc, Resources]),
	      ErrorAcc,
	      Timeout); 
	{?MODULE, Node, Pid, Error} ->
	    wait_on_responses(lists:delete({Node, Pid}, NodePidList), ResourceAcc, [{Node, Pid, Error}|ErrorAcc], Timeout)
    after
	Timeout ->
	    {ok, {ResourceAcc, ErrorAcc}}
    end.

					    
%%------------------------------------------------------------------------------
%% @doc inform the network asyncronously of your existance and all the resources 
%%      that are provided by the local node as well as all the target types that
%%      the local instance of Resource Discovery would like to know about.
%% @spec () -> ok | exit(Reason)
%% @end
%%------------------------------------------------------------------------------
async_inform_network() ->
    LocalResources = rd_store:lookup_local_resources(),
    TargetTypes    = rd_store:lookup_target_types(),
    Nodes          = [node()|nodes()],
    ?INFO_MSG("initiating aasyncronous inform network to nodes ~p~n", [Nodes]),
    lists:foreach(fun(Node) -> rd_core:async_inform(Node, TargetTypes, LocalResources) end, Nodes).

%%------------------------------------------------------------------------------
%% @doc Contacts resource discoveries initial contact node.
%%
%% The initial contact node is specified in configuration with:
%% <code>
%%   {contact_nodes, [NodeName]}
%% </code>
%% The config can be overridden by specifying a contact node at the command line
%% like so:
%% <code>
%%  -contact_node foo@bar.com
%% </code>
%%
%% @spec contact_nodes(Timeout) -> ok | {error, bad_contact_node} | {error, no_contact_node}
%% where
%%  Timeout = Milliseconds::integer()
%% @end
%%------------------------------------------------------------------------------
contact_nodes(Timeout) ->
    {ok, ContactNodes} =
	case lists:keysearch(contact_node, 1, init:get_arguments()) of
	    {value, {contact_node, [I_ContactNode]}} ->
		gas:set_env(resource_discovery, contact_nodes, [I_ContactNode]),
		{ok, [list_to_atom(I_ContactNode)]};
	    _ ->
		gas:get_env(resource_discovery, contact_nodes, [])
	end,
    ping_contact_nodes(ContactNodes, Timeout).

%% @spec contact_nodes() -> pong | pang | no_contact_node
%% @equiv contact_nodes(10000)
contact_nodes() ->
    contact_nodes(10000).

ping_contact_nodes([], _Timeout) ->
    ?INFO_MSG("No contact node specified. Potentially running in a standalone node~n", []),
    {error, no_contact_node};
ping_contact_nodes(Nodes, Timeout) ->
    fs_lists:do_until(fun(Node) ->
			      case fs_net:sync_ping(Node, Timeout) of
				  pong ->
				      ok;
				  pang ->
				      ?INFO_MSG("ping contact node at ~p failed~n", [Node]), 
				      {error, bad_contact_node}
			      end
		      end,
		      ok,
		      Nodes).

%%------------------------------------------------------------------------------
%% @doc Get the contact node for the application.
%% @spec get_contact_nodes() -> {ok, Value} | undefined
%% where
%%  Value = node() | [node()]
%% @end
%%------------------------------------------------------------------------------
get_contact_nodes() ->
    gas:get_env(resource_discovery, contact_nodes).
    
%%------------------------------------------------------------------------------
%% @doc Execute a fun on a cached resource until it returns true or there are no more resources.
%% If the fun returns false the resource that caused the failure will be deleted from the
%% cache of available resources. This function uses the function get_resource/1.
%% This is a convenience function.
%% <pre>
%% Varibles:
%%  Type - The resource type to get from resource discovery.
%%  Fun - The fun to execute. It is of arity one. The fun() must return a bool(). 
%%        Example Fun = fun(Resource) -> ok == distributed_data_store:store(Resource, Data) end.
%% </pre>
%% @spec execute_with_resource(Resource, Fun) -> ok | error
%% @end
%%------------------------------------------------------------------------------
execute_with_resource(Type, Fun) ->
    case get_resource(Type) of
        {ok, Resource} ->
            case Fun(Resource) of
                true -> 
                    ok;
                false ->
                    delete_resource(Type, Resource),
                    execute_with_resource(Type, Fun)
            end;
        {error, no_resources} ->
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Execute a fun on a cached resource until it returns true or there are no more resources.
%% If the fun returns false the resource that caused the failure will be deleted from the
%% cache of available resources. This function uses the function get_resource/2.
%% This is a convenience function.
%% <pre>
%% Varibles:
%%  Type - The resource type to get from resource discovery.
%%  Fun - The fun to execute. It is of arity one. The fun() must return a bool(). 
%%        Example Fun = fun(Resource) -> ok == distributed_data_store:store(Resource, Data) end.
%% </pre>
%% @spec execute_with_same_resource(Resource, Fun) -> ok | error
%% @end
%%------------------------------------------------------------------------------
execute_with_same_resource(Type, Fun) ->
    case get_resource(Type, 1) of
        {ok, Resource} ->
            case Fun(Resource) of
                true -> 
                    ok;
                false ->
                    delete_resource(Type, Resource),
                    execute_with_resource(Type, Fun)
            end;
        {error, no_resources} ->
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Execute an rpc on a cached resource.  If the result of the rpc is {badrpc, reason} the 
%%      resource is deleted and the next resource is tried, else the result is 
%%      returned to the user.
%% <pre>
%% Varibles:
%%  Type - The resource type to get from resource discovery.
%% </pre>
%% @spec (Type, Module, Function, Args) -> RPCResult | {error, no_resources}
%% @end
%%------------------------------------------------------------------------------
rpc_call_with_discovery(Type, Module, Function, Args) ->
    case resource_discovery:get_resource(Type) of
	{ok, Resource} -> 
	    io:format("got a resource ~p~n", [Resource]),
	    case rpc:call(Resource, Module, Function, Args) of
		{badrpc, Reason} ->
		    io:format("got a badrpc ~p~n", [Reason]),
		    resource_discovery:delete_resource(Type, Resource),
		    rpc_call_with_discovery(Type, Module, Function, Args);
		Reply ->
		    io:format("result of rpc was ~p~n", [Reply]),
		    Reply
	    end;
        {error, no_resources} -> 
	    {error, no_resources}
    end.


