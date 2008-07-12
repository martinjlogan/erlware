%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@sixfoe>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Paths specific to sinan mostly having to do with projects.
%%% @end
%%% Created :  1 Jul 2008 by Martin Logan <martinjlogan@sixfoe>
%%%-------------------------------------------------------------------
-module(ewl_sinan_paths).

%% API
-export([
	 find_project_root/1,
         get_build_flavors/1
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc
%%   find "_build.cfg" in the current directory. if not recurse
%%   with parent directory.
%% @spec (Directory::string()) -> string()
%% @end
%%-------------------------------------------------------------------
find_project_root("/") ->
    throw(no_build_config);
find_project_root(Directory) ->
    ConfigFile = filename:join(Directory, "_build.cfg"),
    case filelib:is_file(ConfigFile) of
        true ->
            Directory;
        false ->
            find_project_root(filename:dirname(Directory))
    end.

%%-------------------------------------------------------------------
%% @doc
%%   Return a list of the build flavors presently built for.
%% @spec (ProjectRootDir::string()) -> [string()] 
%% @end
%%-------------------------------------------------------------------
get_build_flavors(ProjectRootDir) ->
    [filename:basename(E) || E <- filelib:wildcard(filename:join(ProjectRootDir, "_build/*"))].

%%%===================================================================
%%% Internal functions
%%%===================================================================
