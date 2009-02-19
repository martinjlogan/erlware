%%%-------------------------------------------------------------------
%%% File    : gas_sup.erl
%%% Author  : Martin J. Logan <martin@localhost.localdomain>
%%%
%%% @doc  
%%%
%%% <p>The supervisor for the gas application.  This supervisor plays 
%%% a critical role in gas. It uses data obtained from the gas_config module 
%%% to create a dynamicly generated child spec containing, as perminant 
%%% children, all modules specified, without duplicate service names.</p>
%%%
%%% Configuration:
%%%  {err_log_tty, bool()}
%%%
%%%  Note* err_log_tty defaults to false disallowing error_logger
%%%        output to the terminal.
%%% @end
%%%
%%% Created : 23 Jan 2003 by Martin J. Logan <martin@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(gas_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
         init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%=============================================================
%% External functions
%%=============================================================
%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @spec start_link(StartArgs) -> {ok, pid()} | Error
%% @end
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%=============================================================
%% Server functions
%%=============================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    error_logger:tty(false),
    gas_override_config:override(),

    {ok, Value} = gas:get_env(gas, err_log_tty, false),
    error_logger:tty(Value),

    error_logger:info_msg("gas_sup:init/1~n"),

    RestartStrategy    = one_for_one,
    MaxRestarts        = 1000,
    MaxTimeBetRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

    case gas:get_env(gas, mod_specs) of
        {ok, ModSpecs} ->
	    {ok, Wiring} = gas:get_env(gas, wiring, []),
	    ChildSpecs   = create_child_specs(ModSpecs, Wiring),
	    error_logger:info_msg("gas_sup with the following child specs ~p~n", [ChildSpecs]),
	    {ok, {SupFlags, ChildSpecs}};
        undefined ->
	    ignore
    end.


%%=============================================================
%% Internal functions
%%=============================================================

%%--------------------------------------------------------------------
%% Function: create_child_specs/1
%% Description: Creates a list of proper child specs.
%% 
%% Expects:
%%  ModSpecs - A list of name, module, function, arg specs to build a supervision tree.
%%  If args is not specified then configuration specs must be defined.
%%
%% Types:
%%  ModSpecs = [ModSpec]
%%   ModSpec = {Name, {M, F}} | {Name, {M, F, A}}
%%    Name = M = F = atom()
%%    A = [term()]
%% 
%% Returns: ChildSpecs | exit(badmatch).
%%--------------------------------------------------------------------
create_child_specs([{Name, {M, F, A}}|T], Wiring) ->
    [{Name, 
     {M, F, A}, 
     permanent, 
     200, 
     worker, 
     [Name]} | create_child_specs(T, Wiring)];
create_child_specs([{Name, {M, F}}|T], Wiring) ->
    case lists:keysearch(Name, 1, Wiring) of
	false ->
	    [{Name, 
	      {M, F, [get_opts(M, F)]}, 
	      permanent, 
	      200, 
	      worker, 
	      [Name]} | create_child_specs(T, Wiring)];
	{value, {Name, WireSpecs}} ->
	    error_logger:info_msg("gas_sup:create_child_specs/2 using the following wirespecs ~p for ~p~n", [WireSpecs, Name]),
	    [{Name, 
	      {M, F, return_args(WireSpecs)}, 
	      permanent, 
	      200, 
	      worker, 
	      [Name]} | create_child_specs(T, Wiring)]
    end;
create_child_specs([], _Wiring) ->
    [].

return_args(WireSpecs) ->
    return_args(WireSpecs, []).

return_args([{transform_previous, TransformSpec}|T], Acc) ->
    TransformedValue = gas_transform:transform_term(Acc, TransformSpec),
    return_args(T, TransformedValue);
return_args([{wire, App, Key, TransformSpec}|T], Acc) ->
    {ok, Value} = gas:get_env(App, Key),
    TransformedValue = gas_transform:transform_term(Value, TransformSpec),
    return_args(T, Acc ++ [TransformedValue]);
return_args([Value|T], Acc) ->
    return_args(T, Acc ++ [Value]);
return_args([], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Function: get_opts/1
%% Description: Gets configuration parameters for children.
%%
%% Expects:
%%  Module - The name of the module that contains the code for the child process.
%%
%% Types:
%%  Module = atom()
%% Opts = [ConfTuples]
%%  ConfTuples = {Key, Value}
%%   Key = Value = term()
%%  ConfigurationSpec = [Spec]
%%   Spec = {optional, ConfToken} | {required, ConfToken}
%%    ConfToken = {App, Key} | Key 
%%
%% Notes: 
%% * If just ConfToken = Key, assume App = gas.
%%
%% Returns:
%%  Opts - A list of configuration tuples.
%%--------------------------------------------------------------------
get_opts(Module, Function) ->
    get_opts2(Module:configuration_spec(Function)).

get_opts2([{required, {App, Key}}|T]) ->
    {ok, Value} = gas:get_env(App, Key),
    [{Key, Value}|get_opts2(T)];
get_opts2([{required, Key}|T]) ->
    get_opts2([{required, {gas, Key}}|T]);
get_opts2([{optional, {App, Key}}|T]) ->
    case gas:get_env(App, Key) of
        {ok, Value} -> [{Key, Value}|get_opts2(T)];
        undefined   -> get_opts2(T)
    end;
get_opts2([{optional, Key}|T]) ->
    get_opts2([{optional, {gas, Key}}|T]);
get_opts2([]) ->
    [].
