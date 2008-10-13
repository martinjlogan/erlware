%%%-------------------------------------------------------------------
%%% File    : rd_store.erl
%%% Author  : Martin J. Logan <martin@gdubya.botomayo>
%%% @doc The storage management functions for resource discovery
%%% @end
%%%-------------------------------------------------------------------
-module(rd_store).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

% Create
-export([
         new_LPS/0
        ]).

% Lookup
-export([
         round_robin_lookup/2,
         index_lookup/3,
         lookup_callback_modules/0,
         lookup_resource_struct/0,
         lookup_local_resources/0,
         lookup_target_types/0,
         lookup_all_resources/2,
         lookup_num_resources/2,
         lookup_num_types/1,
         lookup_types/1
        ]).

% Delete
-export([
         delete_local_resource/1,
         delete_target_type/1,
         delete_resource/3
        ]).

% Store
-export([
         store_local_resources/1,
         store_callback_modules/1,
         store_target_types/1,
         store_resources/2
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(LT, rd_local_resources).
-define(TT, rd_target_types).
-define(CM, rd_callback_modules).
-define(RS, rd_resource_store).

%%====================================================================
%% External functions
%%====================================================================

%%%---------------------------
%%% Local Type, Target type Storage
%%%---------------------------

%%-----------------------------------------------------------------------
%% @doc LPS stands for "Local Parameter Store" 
%% Initialises persistent storage.
%% @end
%%-----------------------------------------------------------------------
new_LPS() ->
    ets:new(?RS, [named_table, public]),
    ets:new(?CM, [named_table, public]),
    ets:new(?LT, [named_table, public]),
    ets:new(?TT, [named_table, public]).

%%-----------------------------------------------------------------------
%% @doc Store the callback modules for the local system.
%% @spec store_callback_modules(Modules) -> ok
%% @end
%%-----------------------------------------------------------------------
store_callback_modules(Modules) ->
    lists:foreach(fun(Module) when is_atom(Module) -> ets:insert(?CM, {Module}) end, fs_lists:make_list_if_not(Modules)).
     
%%-----------------------------------------------------------------------
%% @doc Output the callback modules.
%% @spec lookup_callback_modules() -> [atom()]
%% @end
%%-----------------------------------------------------------------------
lookup_callback_modules() ->
    [E || {E} <- ets:match_object(?CM, '$1')].

%%-----------------------------------------------------------------------
%% @doc Store the "I haves" or local_resources for resource discovery.
%% @spec store_local_resources(LocalResources) -> ok
%% where
%%  LocalResources = [local_resource()] | local_resource()
%% @end
%%-----------------------------------------------------------------------
store_local_resources(LocalFullTypeList) ->
    lists:foreach(
      fun({LocalType, Resource}) -> ets:insert(?LT, {LocalType, Resource}) end,
      fs_lists:make_list_if_not(LocalFullTypeList)).
     
%%-----------------------------------------------------------------------
%% @doc Output the local_resources.
%% @spec lookup_local_resources() -> LocalTypeList
%% @end
%%-----------------------------------------------------------------------
lookup_local_resources() ->
    ets:match_object(?LT, '$1').

%%-----------------------------------------------------------------------
%% @doc Remove a local_resource.
%% <pre>
%% Expects:
%%  LocalResource = resource()
%% </pre>
%% @spec delete_local_resource(LocalResource) -> true
%% @end
%%-----------------------------------------------------------------------
delete_local_resource(LocalResource) ->
    ets:delete(?LT, LocalResource).


%%-----------------------------------------------------------------------
%% @doc Store one or many target type or "I want" values.
%% @spec store_target_types(TargetTypeList) -> ok
%% @end
%%-----------------------------------------------------------------------
store_target_types(TargetTypeList) ->
    lists:foreach(fun(TargetType) when is_atom(TargetType) -> ets:insert(?TT, {TargetType}) end,
		  fs_lists:make_list_if_not(TargetTypeList)).


%%-----------------------------------------------------------------------
%% @doc Output the target_type()s.
%% @spec lookup_target_types() -> TargetTypeList
%% @end
%%-----------------------------------------------------------------------
lookup_target_types() ->
    [E || {E} <- ets:match_object(?TT, '$1')].

%%-----------------------------------------------------------------------
%% @doc Deletes a target type.
%% <pre>
%% Expects:
%%  TargetType = atom()
%% </pre>
%% @spec delete_target_type(TargetType) -> true
%% @end
%%-----------------------------------------------------------------------
delete_target_type(TargetType) ->
    ets:delete(?TT, TargetType).

%%%---------------------------
%%% Network Resource Storage
%%%---------------------------

%%-----------------------------------------------------------------------
%% @doc Gets resource of a particular type at a given index.
%% <pre>
%% Expects:
%%  Type   - The type of resource you want.
%%  Struct - The resource structure.
%% </pre>
%% @spec index_lookup(Type, Index, Struct) -> {error, Struct} | {ok, Resource}
%% @end
%%-----------------------------------------------------------------------
index_lookup(Type, Index, Struct) ->
    case dict:find(Type, Struct) of
	{ok, []} -> {error, Struct};
	{ok, RL} -> get_nth(Index, RL, Struct);
	error    -> {error, Struct}
    end.

get_nth(N, List, Struct) ->
    if 
        length(List) >= N -> 
            {ok, lists:nth(N, List)};
        true -> 
            {error, Struct}
    end.

%%-----------------------------------------------------------------------
%% @doc Gets resource of a particular type outputs and places it in last position.
%% <pre>
%% Expects:
%%  Type   - The type of resource you want.
%%  Struct - The resource structure.
%% </pre>
%% @spec round_robin_lookup(Type, Struct) -> 
%% 	{error, Struct} | {ok, {Resource, NewStruct}}
%% @end
%%-----------------------------------------------------------------------
round_robin_lookup(Type, Struct) ->
    case dict:find(Type, Struct) of
	{ok, [Resource|RL]} -> {ok, {Resource, dict:store(Type, (RL ++ [Resource]), Struct)}};
	{ok, []}            -> {error, Struct};
	error               -> {error, Struct}
    end.
			       
%%-----------------------------------------------------------------------
%% @doc Adds new resources.
%% @spec (Resources, Struct) -> NewStruct
%% where
%%  Resources = resource() | [resource()]
%% @end
%%-----------------------------------------------------------------------
store_resources(Resources, Struct) ->
    NewStruct = 
	lists:foldl(fun({Type, ResourceID}, Struct_) ->
			    update_resource_struct(Type, ResourceID, Struct_)
		    end,
		    Struct,
		    fs_lists:make_list_if_not(Resources)),
    ets:insert(?RS, {resources, NewStruct}),
    NewStruct.

update_resource_struct(Type, ResourceID, Struct) ->
    case dict:find(Type, Struct) of
	{ok, []} -> 
	    dict:store(Type, [ResourceID], Struct);
	{ok, RL} -> 
	    case lists:member(ResourceID, RL) of
		true  -> Struct;
		false -> dict:store(Type, [ResourceID|RL], Struct)
	    end;
	error -> 
	    dict:store(Type, [ResourceID], Struct)
    end.

%%-----------------------------------------------------------------------
%% @doc Remove a single value from the resource list.
%% <pre>
%% Variables:
%%  Type   - The type of resource you want.
%%  Value  - The value to be removed
%%  Struct - The resource structure.
%% </pre>
%% @spec delete_resource(Type, Value, Struct) -> NewStruct
%% @end
%%-----------------------------------------------------------------------
delete_resource(Type, Value, Struct) ->
    NewStruct = 
	case dict:find(Type, Struct) of
	    {ok, []} -> Struct;
	{ok, RL} -> dict:store(Type, lists:delete(Value, RL), Struct);
	    error    -> Struct
	end,
    ets:insert(?RS, {resources, NewStruct}),
    NewStruct.
    

%%-----------------------------------------------------------------------
%% @doc Outputs a list of all resources for a particular type.
%% <pre>
%% Expects:
%%  Type   - The type of resource you want.
%%  Struct - The resource structure.
%% </pre>
%% @spec lookup_all_resources(Type, Struct) -> ResourceList
%% @end
%%-----------------------------------------------------------------------
lookup_all_resources(Type, Struct) ->
    case dict:find(Type, Struct) of
	{ok, RL} -> RL;
	error    -> []
    end.

%%-----------------------------------------------------------------------
%% @doc Outputs a list of all resources for a particular type.
%% @spec () -> Struct
%% @end
%%-----------------------------------------------------------------------
lookup_resource_struct() ->
    try 
	[{resources, Struct}] = ets:lookup(?RS, resources),
	Struct
    catch
	_C:_E ->
	    StructC = dict:new(),
	    ets:insert(?RS, {resources, StructC}),
	    StructC
    end.
    

%%-----------------------------------------------------------------------
%% @doc Outputs the number of resource types.
%% <pre>
%% Expects:
%%  Struct = The resource structure.
%% </pre>
%% @spec lookup_num_types(Struct) -> integer()
%% @end
%%-----------------------------------------------------------------------
lookup_num_types(Struct) ->
    length(dict:fetch_keys(Struct)).

%%-----------------------------------------------------------------------
%% @doc Outputs the types of resources.
%% <pre>
%% Expects: 
%%  Struct = The resource structure.
%% </pre>
%% @spec lookup_types(Struct) -> [Types]
%% @end
%%-----------------------------------------------------------------------
lookup_types(Struct) ->
    dict:fetch_keys(Struct).

%%-----------------------------------------------------------------------
%% @doc Outputs the number of resources of a particualr type.
%% <pre>
%% Expects: Struct
%%  Struct = The resource structure.
%% </pre>
%% @spec lookup_num_resources(Type, Struct) -> integer()
%% @end 
%%-----------------------------------------------------------------------
lookup_num_resources(Type, Struct) ->
    case dict:find(Type, Struct) of
	{ok, []} -> 0;
	{ok, RL} -> length(RL);
	_        -> 0
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
