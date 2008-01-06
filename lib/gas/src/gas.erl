%%%-------------------------------------------------------------------
%%% File    : gas.erl
%%% Author  : Martin J. Logan <martin@localhost.localdomain>
%%%
%%% @doc  
%%%
%%% <p>The <em>GAS</em> application stands for General Application Services.
%%% The function of GAS is to provide a dynamic supervision structure for
%%% so that applications can share resources and avoid the complications  
%%% associated with duplicate service process inclusion. For example consider
%%% what happens when two applications in a release both use a single registered 
%%% library process like fs_elwrap.erl. GAS makes sure it is inluded only once for
%%% a given release.</p>
%%%
%%% <p><em>GAS</em> works by allowing sercvices to be included via
%%% a  release specific specification i.e a configuration file. 
%%% When two applications are to be included in one release they 
%%% simply add there configurations sets.</p>
%%%
%%% <p>GAS also provides an standard configuration 
%%% interface. The interface allows for a decoupling of the configuration 
%%% API and its implementation.</p>
%%%
%%% <p><H1>CallBack Functions</H1></p>
%%%
%%% <p>Returns the keys, required or optional, used for configuration 
%%% of a process. This function must be exported from any module that 
%%% is going to be used to be used by GAS. The function of this callback 
%%% is to return the configuration keys that GAS will use to auto 
%%% configure the service the module encapsulates.</p>
%%%
%%% <pre>
%%% configuration_spec(Function) -> CongifurationSpec
%%%
%%% Variables:
%%%  Function - The function that configuration_spec pertains to.
%%%
%%% Types:
%%%  ConfigurationSpec = [Spec]
%%%   Spec = {optional, ConfToken} | {required, ConfToken}
%%%    ConfToken = {App, Key} | Key
%%%  Function = atom()
%%% </pre>
%%% @end
%%%
%%% Created : 24 Jan 2003 by Martin J. Logan <martin@localhost.localdomain>
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
         get_env/1,
	 get_argument/4,
	 get_argument/3,
	 get_argument/2,
	 get_argument/1
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
%% @doc Alters the configuration file.
%% <pre>
%%
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be inserted
%%  Val      - The value to be inserted.
%% </pre>
%%
%% @spec
%%  modify_config_file(FileLocation, AppGroup, Key, Val) -> ok | {error, Reason}
%% where
%%  AppGroup = term()
%%  Key = term()
%%  Value = term()
%%  Reason = no_such_config_entry
%% @end
%%--------------------------------------------------------------------
modify_config_file(FileLocation, AppGroup, Key, Val) ->
    case gas:get_env(AppGroup, Key, undefined) of
	{ok, undefined} -> 
	    {error, no_such_config_entry};
	{ok, _Value} ->
	    {ok, [Terms]} = file:consult(FileLocation),
	    NewTerms    = substitute_entry(Terms, AppGroup, Key, Val),
	    {ok, IOD}   = file:open(FileLocation, [write]),
	    io:fwrite(IOD, "~p. ~n", [NewTerms])
    end.

substitute_entry([{AppGroup, GroupBody}|T], AppGroup, Key, Value) ->
    [{AppGroup, lists:foldr(fun({Key_, _}, Acc) when Key_ == Key -> [{Key, Value}|Acc];
			       (KeyVal, Acc)                     -> [KeyVal|Acc]
			    end, [], GroupBody)}|T];
substitute_entry([AppGroup2|T], AppGroup, Key, Value) ->
    [AppGroup2|substitute_entry(T, AppGroup, Key, Value)];
substitute_entry([], _AppGroup, _Key, _Value) ->
    [].


%%--------------------------------------------------------------------
%% @doc Modify the value of a config entry with a fun. The fun will take the old value and return a new one.  The new value will
%%      be placed inside the configuration file. 
%% <pre>
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be inserted
%%  Fun      - The fun to modify the value. The fun is of the form fun(OldValue) -> NewValue end.
%% </pre>
%%
%%
%% @spec modify_config_value(FileLocation, AppGroup, Key, Fun) -> ok | {error, Reason}
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
	    modify_config_file(FileLocation, AppGroup, Key, Fun(Value))
    end.

%%--------------------------------------------------------------------
%% @doc For use in association with command line parameter overriding. 
%% <pre>
%%
%% Outputs a type command line parameter converted to a specific type if such a 
%% parameter has been provided. In the absence of such a parameter the function 
%% outputs a parameter, of the same name, from a configuration file. 
%%
%% Note* The fun that is passed in should either complete successfully and return just value
%% or it should terminate with an exit.
%%
%% Format of Fun:
%%  In doing a conversion of a value received from the command line it is important to recognize that 
%%  The value will start as a list of strings. To convert a command line parameter to an integer the fun() would 
%%  be written as follows: fun([Value]) -> list_to_integer(Value) end.
%%
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be selectd
%%  Type     - The type the returned value is to be converted to.
%%  DefaultValue  - If the key is not a defined env param then DefaultValue is returned.
%%
%% Types:
%%  AppGroup = Key = term()
%%  Type = fun() | atom() currently (atom | integer | string | list | list_string)
%%  Value = term()
%%
%% Notes:
%%  Type = list_string expects a list of words for which we return a single string
%%  joined with spaces. This is useful for re-creating a command-line-argument
%%  that was originally a single string but which has then been broken up into a number of words.
%%
%% </pre>
%%
%% @spec get_argument(AppGroup, Key, Type, DefaultValue) -> {ok, Value} | {ok, DefaultValue} | exit()
%% @end
%%--------------------------------------------------------------------
get_argument(AppGroup, Key, Type, DefaultValue) ->
    case lists:keysearch(Key, 1, init:get_arguments()) of
    	{value, {Key, Value}} -> 
	    convert_type(Value, Type);
	_                     -> 
	    case get_env(AppGroup, Key) of
		{ok, Value} -> {ok, Value};
	        _           -> {ok, DefaultValue}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc For use in association with command line parameter overriding. 
%% <pre>
%%
%% Outputs a type command line parameter converted to a specific type if such a 
%% parameter has been provided. In the absence of such a parameter the function 
%% outputs a parameter, of the same name, from a configuration file. 
%%
%% Note* The fun that is passed in should either complete successfully and return just value
%% or it should terminate with an exit.
%%
%% Format of Fun:
%%  In doing a conversion of a value received from the command line it is important to recognize that 
%%  The value will start as a list of strings. To convert a command line parameter to an integer the fun() would 
%%  be written as follows: fun([Value]) -> list_to_integer(Value) end.
%%
%% Expects:
%%  AppGroup - The application grouping to be outputd.
%%  Key      - The Key of the value to be selectd
%%  Type     - The type the returned value is to be converted to.
%%
%% Types:
%%  AppGroup = Key = term()
%%  Type = fun() | atom() currently (atom | integer | string | list | list_string)
%%  Value = term()
%%
%% Notes:
%%  Type = list_string expects a list of words for which we return a single string
%%  joined with spaces. This is useful for re-creating a command-line-argument
%%  that was originally a single string but which has then been broken up into a number of words.
%%
%% </pre>
%%
%% @spec get_argument(AppGroup, Key, Type) -> {ok, Value} | undefined | exit()
%% @end
%%--------------------------------------------------------------------
get_argument(AppGroup, Key, Type) ->
    case lists:keysearch(Key, 1, init:get_arguments()) of
    	{value, {Key, Value}} -> convert_type(Value, Type);
	_                     -> get_env(AppGroup, Key)
    end.

%% @spec get_argument(Key, Type) -> {ok, Value} | undefined
%% @equiv get_argument(gas, Key, Type)
get_argument(Key, Type) ->
    case lists:keysearch(Key, 1, init:get_arguments()) of
    	{value, {Key, Value}} -> convert_type(Value, Type);
	_                     -> get_env(Key)
    end.

%% @spec get_argument(Key) -> {ok, Value} | undefined
%% @equiv get_argument(gas, Key, list)
get_argument(Key) -> get_argument(Key, list).

%%====================================================================
%% Internal functions
%%====================================================================
% Converts a value to a particular type based on the spec
% Currently accepts a fun() of the atom()s integer | atom | string
% returns: {ok, Value} | exit(Reason)
convert_type([Value], atom)    -> {ok, list_to_atom(Value)};
convert_type([Value], string)  -> {ok, Value};
convert_type(Value, list)      -> {ok, Value};
convert_type([Value], integer) -> {ok, list_to_integer(Value)};
convert_type(Value, list_string) ->
    convert_list_string_type (Value);
convert_type(Value, Fun)       -> {ok, Fun(Value)}.

convert_list_string_type ([ValueHead | ValueTail]) when integer (ValueHead) ->
    {ok, [ValueHead | ValueTail]};

convert_list_string_type ([ValueHead | ValueTail]) when list (ValueHead) ->
    TheListOfStrings = [ValueHead | ValueTail],
    CatenateWithSpaces = fun (StringX, AccIn) ->
	if AccIn == "" ->
	    TempString = "";
	true ->
	    TempString = lists:append (AccIn, " ")
	end,
	% XXX 
	_AccOut = lists:append (TempString, StringX)
    end,
    TheSingleCatenatedString = lists:foldl (CatenateWithSpaces, "", TheListOfStrings),
    {ok, TheSingleCatenatedString};

convert_list_string_type (_) ->
    {ok, ""}.

