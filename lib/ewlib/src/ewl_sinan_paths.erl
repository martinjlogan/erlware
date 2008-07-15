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
         get_build_flavors/1,
	 dist_tarball_path/4,
	 built_app_path/4
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

%%-------------------------------------------------------------------
%% @doc Return the location of a built application.
%% @spec built_app_path(ProjectRootDir, BuildFlavor, AppName, AppVsn) -> string()
%% @end
%%-------------------------------------------------------------------
built_app_path(ProjectRootDir, BuildFlavor, AppName, AppVsn) ->
    filename:join([ProjectRootDir, "_build", BuildFlavor, "apps", lists:flatten([AppName, "-", AppVsn])]).

%%-------------------------------------------------------------------
%% @doc Return the location of a dist tarball
%% @spec dist_tarball_path(ProjectRootDir, BuildFlavor, ReleaseName, ReleaseVsn) -> string()
%% @end
%%-------------------------------------------------------------------
dist_tarball_path(ProjectRootDir, BuildFlavor, ReleaseName, ReleaseVsn) ->
    filename:join([ProjectRootDir, "_build", BuildFlavor, "tar", lists:flatten([ReleaseName, "-", ReleaseVsn, ".tar.gz"])]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
