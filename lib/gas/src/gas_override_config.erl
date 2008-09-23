%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Override application config based on some specified override file. 
%%% @end
%%% Created : 20 Sep 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gas_override_config).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 home_filepath/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

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
%% @doc return a path to a home directory config file if one is configured.
%% @spec () -> {ok, HomeFilePath} | undefined
%% @end
%%--------------------------------------------------------------------
home_filepath() ->
    case gas:get_env(gas, home_filename) of
	{ok, OverrideFileName} ->
	    case os:getenv("HOME") of
		false ->
		    error_logger:info_msg("The HOME environment variable is not set~n"),
		    undefined;
		Home ->
		    filename:join(Home, OverrideFileName)
	    end;
	undefined ->
	    undefined
    end.
    
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
    insert_config_data(),
    {ok, #state{}, 0}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(timeout, State) ->
    {stop, normal, State}.

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
add_config_values(ConfigList) ->
    add_config_values(ConfigList, stop).

add_config_values([{AppName, ConfigList}|T], ShouldContinue) ->
    case add_config_values(AppName, ConfigList, stop) of
	stop     -> add_config_values(T, ShouldContinue);
	continue -> add_config_values(T, continue)
    end;
add_config_values([], ShouldContinue) ->
    ShouldContinue.

add_config_values(AppName, [{home_filename, Value}|T], stop) ->
    error_logger:info_msg("gas_override_config:add_config_values adding ~p ~p ~p~n", [AppName, home_filename, Value]),
    gas:set_env(AppName, home_filename, Value),
    add_config_values(AppName, T, continue);
add_config_values(AppName, [{Key, Value}|T], ShouldContinue) ->
    error_logger:info_msg("gas_override_config:add_config_values adding ~p ~p ~p~n", [AppName, Key, Value]),
    gas:set_env(AppName, Key, Value),
    add_config_values(AppName, T, ShouldContinue);
add_config_values(_AppName, [], ShouldContinue) ->
    ShouldContinue.

insert_config_data() ->
    case home_filepath() of
	undefined ->
	    ok;
	OverrideFilePath ->
	    error_logger:info_msg("performing config override with config from ~s~n", [OverrideFilePath]),
	    case add_config_values(read_config_file(OverrideFilePath)) of
		stop ->
		    ok;
		continue ->
		    insert_config_data()
	    end
    end.

read_config_file(OverrideFilePath) ->
    case file:consult(OverrideFilePath) of
	{ok, [ConfigList]} ->
	    ConfigList;
	{error, enoent} ->
	    []
    end.
	    

