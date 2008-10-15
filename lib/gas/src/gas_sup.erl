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
    case gas:get_argument(gas, err_log_tty, atom) of
    	{ok, true} -> ok;
	_          -> error_logger:tty(false)
    end,

    gas_override_config:override(),

    error_logger:info_msg("gas_sup:init~n"),

    RestartStrategy    = one_for_one,
    MaxRestarts        = 1000,
    MaxTimeBetRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

    case gas:get_env(gas, mod_specs) of
        {ok, ModSpecs} ->
	    ChildSpecs = create_child_specs(ModSpecs),
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
%%  ModSpecs - A list of name, module, function, arg specs to build a supervision tree. If args is not specified then configuration specs must be defined.
%%
%% Types:
%%  ModSpecs = [ModSpec]
%%   ModSpec = {Name, {M, F}} | {Name, {M, F, A}}
%%    Name = M = F = atom()
%%    A = [term()]
%% 
%% Returns: ChildSpecs | exit(badmatch).
%%--------------------------------------------------------------------
create_child_specs([{Name, {M, F, A}}|T]) ->
    [{Name, 
     {M, F, A}, 
     permanent, 
     200, 
     worker, 
     [Name]} | create_child_specs(T)];
create_child_specs([{Name, {M, F}}|T]) ->
    [{Name, 
     {M, F, [get_opts(M, F)]}, 
     permanent, 
     200, 
     worker, 
     [Name]} | create_child_specs(T)];
create_child_specs([]) ->
    [].


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
