%%%-------------------------------------------------------------------
%%% @author  : Martin J. Logan <martin@erlware.org>
%%% Created : 24 Jan 2003 by Martin J. Logan 
%%%-------------------------------------------------------------------
-module(gas).

-behaviour(application).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start/2,
         stop/1,
         load_config/3,
         set_env/3,
	 modify_config_file/4,
	 modify_config_value/4,
         get_env/3,
         get_env/2,
         get_env/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc The starting point for an erlang application.
%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case gas_sup:start_link(StartArgs) of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Called upon the termintion of an application.
%% @spec stop(State) -> void() 
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc Stores all data associated with a particular app in the local store.
%% <pre>
%%
%% Note: If type is not specified then it defaults to acting on AppInstance = "general".
%%
%% Expects:
%%  Source - The source of the configuration.
%%  AppGroup - The application grouping to be selected upon.
%%  AppInstance - "general" or a specific app instance.
%%
%% Types:
%%  Source = node() | string() | atom()
%%  App  = term()
%%  AppInstance = term() | "general"
%%
%% </pre>
%%
%% @spec
%%  load_config(Source, AppGroup, AppInstance) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
load_config(_Source, _AppGroup, _AppInstance) -> {error, not_yet_implemented}.

%%--------------------------------------------------------------------
%% @doc Outputs a configuration specified enviornment value. Gas allows the caller to pull config for an application that 
%%      the caller does not belong to. This is useful for applications that are purely library code and don't actually start.
%% <pre>
%%
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be selectd
%%  DefaultValue  - If the key is not a defined env param then DefaultValue is returned.
%%
%% Types:
%%  AppGroup = Key = term()
%%  Value = term()
%%  DefaultValue = term()
%%
%% </pre>
%%
%% @spec
%%  get_env(AppGroup, Key, DefaultValue) -> {ok, Value} | {ok, DefaultValue}
%% @end
%%--------------------------------------------------------------------
get_env(AppGroup, Key, Default) ->
    case ets:lookup(ac_tab, {env, AppGroup, Key}) of
    	[{{env, AppGroup, Key}, Value}] -> {ok, Value};
	[]                              -> {ok, Default}
    end.

%%--------------------------------------------------------------------
%% @doc Outputs a configuration specified enviornment value.
%% <pre>
%%
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be selectd
%%  Default  - If the key is not a defined env param then Default is returned.
%%
%% Types:
%%  AppGroup = Key = term()
%%  Value = term()
%%  Default = term()
%%
%% </pre>
%%
%% @spec
%%  get_env(AppGroup, Key) -> {ok, Value} | undefined
%% @end
%%--------------------------------------------------------------------
get_env(AppGroup, Key) ->
    case ets:lookup(ac_tab, {env, AppGroup, Key}) of
    	[{{env, AppGroup, Key}, Value}] -> {ok, Value};
	[]                              -> undefined
    end.

%% @spec get_env(Key) -> {ok, Value} | undefined
%% @equiv get_env(gas, Key, undefined)
get_env(Key) ->
    application:get_env(Key).


%%--------------------------------------------------------------------
%% @doc Alters the configuration store.
%% WARNING!!
%%  Use this function only if you know what you are doing, that is,
%%  on your own applications. It is very application and configuration
%%  parameter dependent when and how often the value is read by the 
%%  application, and careless use of this function may put the application 
%%  in a wierd, inconsistent, and malfunctioning state. 
%% <pre>
%%
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be inserted
%%  Val      - The value to be inserted.
%%
%% Types:
%%  AppGroup = Key = term()
%%  Value = term()
%% </pre>
%%
%% @spec
%%  set_env(AppGroup, Key, Val) -> ok
%% @end
%%--------------------------------------------------------------------
set_env(Application, Key, Val) ->
    application:set_env(Application, Key, Val).


%%--------------------------------------------------------------------
%% @doc Alters the configuration file by inserting or overwriting a key value pair for a particular application.
%% <pre>
%%
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be inserted
%%  Val      - The value to be inserted.
%% </pre>
%%
%% @spec
%%  modify_config_file(FileLocation, AppGroup, Key, Val) -> ok 
%% where
%%  AppGroup = term()
%%  Key = term()
%%  Value = term()
%% @end
%%--------------------------------------------------------------------
modify_config_file([H|_] = FileLocation, AppGroup, Key, Val) when is_integer(H) ->
    case gas:get_env(AppGroup, Key) of
	undefined -> 
	    write_out_key_group(FileLocation, AppGroup, Key),
	    write_out_entry(FileLocation, AppGroup, Key, Val);
	{ok, _Value} ->
	    write_out_entry(FileLocation, AppGroup, Key, Val) 
    end;

%%--------------------------------------------------------------------
%% @doc Alters the configuration file by inserting or overwriting a
%%      key value pair for a particular application. The function tests
%%      all config files supplied for the value and modifies the first
%%      file where the value is present.  If the value is not present
%%      in any of the files the first file receives the value. 
%% <pre>
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be inserted
%%  Val      - The value to be inserted.
%% </pre>
%%
%% @spec
%%  modify_config_file(FileLocations, AppGroup, Key, Val) -> ok 
%% where
%%  FileLocations = [string()]
%%  AppGroup = term()
%%  Key = term()
%%  Value = term()
%% @end
%%--------------------------------------------------------------------
modify_config_file(FileLocations, AppGroup, Key, Val) ->
    FileLocation = 
	lists:foldl(fun(FilePath, TargetPath) ->
			    try proplists:get_value(Key, proplists:get_value(AppGroup, read_config_file(FilePath))) of
				undefined ->
				    TargetPath;
				_Value ->
				    case TargetPath of
					undefined -> FilePath;
					_         -> TargetPath
				    end
			    catch
				_C:_E -> TargetPath
			    end
		    end,
		    undefined,
		    FileLocations),
    case FileLocation of
	undefined ->
	    modify_config_file(hd(FileLocations), AppGroup, Key, Val);
	FileLocation ->
	    modify_config_file(FileLocation, AppGroup, Key, Val) 
    end.

write_out_entry(FileLocation, AppGroup, Key, Val) ->
  {ok, [Terms]} = file:consult(FileLocation),
  NewTerms    = substitute_entry(Terms, AppGroup, Key, Val),
  {ok, IOD}   = file:open(FileLocation, [write]),
  io:fwrite(IOD, "~p. ~n", [NewTerms]).

substitute_entry([{AppGroup, GroupBody}|T], AppGroup, Key, Value) ->
    [{AppGroup, lists:foldr(fun({Key_, _}, Acc) when Key_ == Key -> [{Key, Value}|Acc];
			       (KeyVal, Acc)                     -> [KeyVal|Acc]
			    end, [], GroupBody)}|T];
substitute_entry([AppGroup2|T], AppGroup, Key, Value) ->
    [AppGroup2|substitute_entry(T, AppGroup, Key, Value)];
substitute_entry([], _AppGroup, _Key, _Value) ->
    [].

write_out_key_group(FileLocation, AppGroup, Key) ->
    Terms = read_config_file(FileLocation),
    ModdedTerms = lists:foldr(
		    fun({AppGroup_, GroupBody}, Acc) when AppGroup == AppGroup_ ->
			    [{AppGroup, [{Key, []}|GroupBody]}|Acc];
		       (AppGroup_, Acc) ->
			    [AppGroup_|Acc]
		    end,
		    [], Terms),

    NewTerms = 
	case ModdedTerms == Terms of
	    true  -> [{AppGroup, [{Key, []}]}|Terms];
	    false -> ModdedTerms
	end,
    {ok, IOD}   = file:open(FileLocation, [write]),
    io:fwrite(IOD, "~p. ~n", [NewTerms]).

%%--------------------------------------------------------------------
%% @doc Modify the value of a config entry with a fun. The fun will take the old value and return a new one.
%%      The new value will be placed inside the configuration file as well as the runtime config store.
%% <pre>
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be inserted
%%  Fun      - The fun to modify the value. The fun is of the form fun(OldValue) -> NewValue end.
%%
%% Example: modify_config_value("/tmp/app.config", gas, spec, 
%%                              fun(undefined) -> [NewValue];
%%                                 (OldValue)  -> [NewValue|OldValue] end).
%%
%% modify_config_value/4 passes OldValue to the fun supplied. It is important to handle the case for undefined in case
%% there was no previous entry in the config for the key supplied.
%% </pre>
%%
%% @spec modify_config_value(FileLocation, AppGroup, Key, Fun) -> ok 
%% where
%%  AppGroup = term()
%%  Key = term()
%%  Reason = no_such_config_entry
%% @end
%%--------------------------------------------------------------------
modify_config_value(FileLocation, AppGroup, Key, Fun) ->
    case gas:get_env(AppGroup, Key, undefined) of
	{ok, undefined} -> 
	    {error, no_such_config_entry};
	{ok, Value} ->
	    NewValue = Fun(Value),
	    gas:set_env(AppGroup, Key, NewValue),
	    modify_config_file(FileLocation, AppGroup, Key, NewValue)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

read_config_file(OverrideFilePath) ->
    case file:consult(OverrideFilePath) of
	{ok, [ConfigList]} ->
	    ConfigList;
	{error, enoent} ->
	    []
    end.

