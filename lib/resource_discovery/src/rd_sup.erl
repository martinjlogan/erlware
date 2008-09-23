%%%-------------------------------------------------------------------
%%% File    : rd_sup.erl
%%% Author  : Martin J. Logan <martin@gdubya.botomayo>
%%% @doc The super.
%%%-------------------------------------------------------------------
-module(rd_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/1,
         start_heartbeat/1,
         stop_heartbeat/0
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
-define(HEART, heart).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @spec start_link(StartArgs) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Starts the heartbeat process and adds it to the supervision tree.
%% <pre>
%% Types: 
%%  Name = atom()
%%  Frequency = integer()
%%
%% Note:
%% * requency, is the frequency, in mili seconds, for which 
%%   resource_monitor:inform_network/0 is called.
%% </pre>
%% @spec start_heartbeat(Frequency) -> void()
%% @end
%%--------------------------------------------------------------------
start_heartbeat(Frequency) ->
    supervisor:start_child(rd_sup,
			   {?HEART, {rd_heartbeat, start_link, [Frequency]},
			    permanent, 500, worker, [rd_heartbeat]}).
%%--------------------------------------------------------------------
%% @doc Stops the hearbeat.
%% @spec stop_heartbeat() -> ok
%% @end
%%--------------------------------------------------------------------
stop_heartbeat() ->
    supervisor:terminate_child(rd_sup, ?HEART),
    supervisor:delete_child(rd_sup, ?HEART).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% @hidden
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy    = one_for_one,
    MaxRestarts        = 1000,
    MaxTimeBetRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

    % Create the storage for the local parameters; i.e. LocalTypes 
    % and TargetTypes.
    rd_store:new_LPS(),

    ChildSpecs = 
        [ 
          {resource_discovery,
           {rd_core, start_link, []},
           permanent,
           1000,
           worker,
           [resource_discovery]}
         ],

    {ok, {SupFlags, ChildSpecs}}.





