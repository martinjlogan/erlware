%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Override application config based on some specified override file. 
%%% @end
%%% Created : 20 Sep 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gas_override_config).

%% API
-export([
	 override/0,
	 override_file_path/0
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Override config values.
%%
%% @spec override() -> ok 
%% @end
%%--------------------------------------------------------------------
override() ->
    insert_config_data().

%%--------------------------------------------------------------------
%% @doc return a path to an override config file if one is configured.
%%      if a file path is added it is used, if the configured file is
%%      just a name then it is assumed to be under the users home
%%      directory. If the home_file_path config entry is used instead
%%      the file is always searched off of the users home dir.
%% @spec () -> {ok, OverrideFilePath} | undefined
%% @end
%%--------------------------------------------------------------------
override_file_path() ->
    case gas:get_env(gas, override_file_path) of
	{ok, OverrideFileName} ->
	    %% @todo for now this only works for unix file paths
	    case filename:dirname(OverrideFileName) of
		Res when Res == OverrideFileName orelse Res == "." ->
		    return_home_file_path(OverrideFileName);
		_Else ->
		    {ok, OverrideFileName}
		end;
	undefined ->
	    case gas:get_env(gas, home_file_path) of
		undefined          -> undefined;
		{ok, HomeFilePath} -> return_home_file_path(HomeFilePath)
	    end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
return_home_file_path(HomeFilePath) ->
    case os:getenv("HOME") of
	false ->
	    error_logger:info_msg("The HOME environment variable is not set~n"),
	    undefined;
	Home ->
	    {ok, filename:join(Home, HomeFilePath)}
    end.

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
    case override_file_path() of
	undefined ->
	    ok;
	{ok, OverrideFilePath} ->
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
	    

