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
	    case catch add_config_values(read_config_file(OverrideFilePath)) of
		stop ->
		    error_logger:info_msg("performing config override with config from the commandline~n", []),
		    add_config_values(convert_commandline_args()),
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
	    
convert_commandline_args() ->
    PlainArgs = init:get_plain_arguments(),
    error_logger:info_msg("gas_override_config:convert_commandline_args/0 plain args ~p~n", [PlainArgs]),
    FlattenedArgs = flattened_commandline_args(grab_overrides(PlainArgs)),
    error_logger:info_msg("gas_override_config:convert_commandline_args/0 arg string ~p~n", [FlattenedArgs]),
    ConvertedArgs = lists:map(fun(Arg) -> convert_string_to_terms(Arg) end, no_space(FlattenedArgs)),
    collect_args(ConvertedArgs).

grab_overrides(["-override"|T]) -> T;
grab_overrides([_|T])           -> grab_overrides(T);
grab_overrides([])              -> [].

collect_args(ConvertedArgs) ->
    collect_args(ConvertedArgs, dict:new()).

collect_args([[$-|App], Key, Value|T], Dict) ->
    collect_args(T, dict:append_list(list_to_atom(App), [{Key, Value}], Dict));
collect_args([], Dict) ->
    dict:to_list(Dict).
    

flattened_commandline_args([[$[|_]|_] = Args) ->
    error_logger:info_msg("beggining a list at Args ~p~n", [Args]),
    {Elements, Rest} = pull_until($], Args),
    [lists:flatten(Elements)|flattened_commandline_args(Rest)];
flattened_commandline_args([[${|_]|_] = Args) ->
    {Elements, Rest} = pull_until($}, Args),
    [lists:flatten(Elements)|flattened_commandline_args(Rest)];
flattened_commandline_args([Arg|T]) ->
    [Arg|flattened_commandline_args(T)];
flattened_commandline_args([]) ->
    [].

pull_until(Char, List) ->
    error_logger:info_msg("char ~p and list ~p~n", [Char, List]),
    pull_until(Char, List, []).

pull_until(Char, [El|T], Acc) ->
    case hd(lists:reverse(El)) of
	Char -> {lists:reverse([El|Acc]), T};
	_    -> pull_until(Char, T, [El|Acc])
    end;
pull_until(_Char, [], Acc) ->
    {error, {bad_term, Acc}}.
	    
    

%%----------------------------------------------------------------------------
%% @private
%% @doc Take a commandline string and convert it into a term taking into account some of the nuances of faxien.  This really comes down
%%      to two cases: 
%%       the fact that 2.1 gets turned into 2.111111 or somesuch by parse_term.  This needs to be a vsn string not a float but the user
%%       should not have to enter "2.1" from the commandline.
%%      and
%%       the fact that http://repo.blah.com/pub needs to be converted to a string off the commandline where it appears bare. The user
%%       should not have to type quotes as in "http://repo.blah.com/pub" but without them parse_term will fail. 
%% @end
%%----------------------------------------------------------------------------
convert_string_to_terms(ArgString) ->
    case regexp:match(ArgString, "^[0-9]+\.[0-9]+$") of
	{match, _, _} -> 
	    ArgString;
	_  ->
	    ScanableArg = ArgString ++ ". ", 
	    {ok, Toks, _Line} = erl_scan:string(ScanableArg, 1),
	    case catch erl_parse:parse_term(Toks) of
		{ok, Term} -> 
		    Term;
		Error ->
                    convert_string_to_terms(special_case(Error, ArgString))
	    end
    end.

special_case(Msg, ArgString) ->
    error_logger:info_msg("gas_override_config:special_case/3 Special case discovered for ~p~nconverting input" ++
			  " arg ~p to string and reprocessing~n", [Msg, ArgString]),
    "\"" ++ ArgString ++ "\"".

%%----------------------------------------------------------------------------
%% @private
%% @doc Windows sometimes uses spaces in its directory names, if a \ is found at the end of a string connect 
%%      it with the next string
%% @end
%%----------------------------------------------------------------------------
no_space(Args) ->
    no_space(Args, []).

no_space([], Acc) ->
    lists:reverse(Acc);
no_space([E], Acc) ->
    lists:reverse([E|Acc]);
no_space([A1, A2|T], Acc) ->
    case hd(lists:reverse(A1)) of
	$\\ -> no_space(T,[A1 ++ " " ++ A2|Acc]);
	_   -> no_space([A2|T], [A1|Acc])
    end.
	    
