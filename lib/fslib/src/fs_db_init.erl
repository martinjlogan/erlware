%%%-------------------------------------------------------------------
%%% File    : fs_db_init.erl
%%% Author  : Martin J. Logan <martin@localhost.localdomain>
%%%
%%% @doc  
%%%
%%% <p>Brings mnesia databases together dynamically. This module is intended
%%% only to cover the simple cases for replication. The simple cases are defined
%%% as:</p>
%%%
%%% 1. Starting from scratch as the first mnesia db in a replication cluster and 
%%% in this case self seeding the db. <br/>
%%%
%%% 2. Starting with an empty schema and joining a replication cluster.<br/>
%%%
%%% @end
%%%
%%% Created :  1 Feb 2003 by Martin J. Logan <martin@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(fs_db_init).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, start_link/3]).

%%--------------------------------------------------------------------
%% Internal  exports
%%--------------------------------------------------------------------
-export([db_init/4]).

%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(DEFAULT_SCHEMA_TYPE, disc_copies).
-define(WAIT_FOR_TABLES, 10000).
 
%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server. 
%% <pre>
%% Variables:
%%  CallBackModule - The module that exhibits the db init behaviour.
%%  Args - A list of arguments delivered to the CallBackModule:init/1 function.
%%  Options - A list of options for fs_db_init.
%%
%% Types:
%%  Args = list()
%%  Options = [Option]
%%   Option {schema_type, Type} | keep_schema
%%    Type = ram_copies | disc_copies | disc_only_copies 
%%
%% </pre>
%% @spec start_link(CallBackModule, Args, Options) -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link(CallBackModule, Args, Options) ->
    proc_lib:start_link(?MODULE, db_init, [self(), CallBackModule, Args, Options]).

%% @spec start_link(CallBackModule) -> {ok, pid()} | {error, Reason}
%% @equiv start_link(CallBackModule, [], [])
start_link(CallBackModule) ->
    start_link(CallBackModule, [], []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%--------------------------------------------------------------------
%% @doc Initialize db then die.
%% @end
%%--------------------------------------------------------------------
db_init(Parent, CallbackModule, Args, Options) ->
    case apply(CallbackModule, init, [Args]) of
        no_init -> 
	    proc_lib:init_ack(Parent, {ok, self()}),
            exit(normal);
        {ok, []} ->
	    case lists:member(keep_schema, Options) of
		false -> 
		    delete_schema(),
		    local_init(CallbackModule, schema_type(Options)),
		    proc_lib:init_ack(Parent, {ok, self()});
		true -> 
		    mnesia:start(),
		    case catch length(mnesia:system_info(local_tables)) of
			Length when Length > 1 -> 
			    proc_lib:init_ack(Parent, {ok, self()});
			_ ->
			    delete_schema(),
			    local_init(CallbackModule, schema_type(Options)),
			    proc_lib:init_ack(Parent, {ok, self()})
		    end
	    end;
        {ok, DBNodes} ->
	    delete_schema(),
	    remote_init(CallbackModule, schema_type(Options), DBNodes),
	    proc_lib:init_ack(Parent, {ok, self()})
    end.

            
%% deletes a local schema.
delete_schema() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start().
            
%%--------------------------------------------------------------------
%% Locally initialize tables.
%% Returns: void() | exit(Reason)
%%--------------------------------------------------------------------
local_init(CallbackModule, Type) -> 
    ok            = schema_type_specific(Type),
    {ok, Records} = apply(CallbackModule, local_init, []),
    lists:foreach(fun(Record) -> 
			  ok = mnesia:dirty_write(Record) 
		  end, 
		  Records).

%%--------------------------------------------------------------------
%% Join with another node in a mnesia cluster.
%% Types:
%%  Reason = schema_type_change | replication
%%
%% Returns:
%%  ok | {error, Reason} 
%%--------------------------------------------------------------------
remote_init(CallbackModule, Type, []) ->
    {error, replication};
remote_init(CallbackModule, Type, [Node|T]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} -> 
            {ok, TableTypeList} = apply(CallbackModule, remote_init, []),

            Res = mnesia:add_table_copy(schema, node(), Type),
            error_logger:info_msg("fs_db_init:remote_init schema type ~p~n", [Res]),

            Fun = fun({Table, TableType}) ->
                          Res1 = mnesia:add_table_copy(Table, node(), TableType),
                          error_logger:info_msg(
                            "fs_db_init:remote_init add_table copy = ~p~n", [Res1])
		  end,
            lists:foreach(Fun, lists:delete(schema, TableTypeList)),

            Tables = mnesia:system_info(tables),
            case mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES) of
                ok              -> schema_type_specific(Type);
                {error, Reason} -> {error, replication}
            end;
        _ -> 
            remote_init(CallbackModule, Type, T)
    end.
    

%% If the type is disc copy make it so.
%% Returns: ok | {error, schema_type_change}
schema_type_specific(disc_only_copies) ->
    %mnesia:stop(),
    Result = res(mnesia:change_table_copy_type(schema, node(), disc_only_copies)),
    error_logger:info_msg("changing schema to disc_only_copy: ~p~n", [Result]),
    %mnesia:start(),
    Result;
schema_type_specific(disc_copies) ->
    %mnesia:stop(),
    Result = res(mnesia:change_table_copy_type(schema, node(), disc_copies)),
    error_logger:info_msg("changing schema to disc_copy: ~p~n", [Result]),
    %mnesia:start(),
    Result;
schema_type_specific(_) -> 
    ok.

res({atomic, ok}) -> ok;
res(_)            -> {error, schema_type_change}.
    

%%--------------------------------------------------------------------
%% Determine the storage type of the schema.
%% Types:
%%  Type = ram_copies | disc_copies | disc_only_copies.
%%
%% Returns:
%%  exit({error, {enosuchtype, BadType}}) | Type
%%--------------------------------------------------------------------
schema_type([{schema_type, Type}|T]) -> type(Type);
schema_type([_|T])                   -> schema_type(T);
schema_type([])                      -> ?DEFAULT_SCHEMA_TYPE.

type(ram_copies)       -> ram_copies;
type(disc_copies)      -> disc_copies;
type(disc_only_copies) -> disc_only_copies;
type(BadType)          -> exit({error, {enosuchtype, BadType}}).

















