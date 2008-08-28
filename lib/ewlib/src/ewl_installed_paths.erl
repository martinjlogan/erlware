%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@erlware.org>
%%% @doc Understands the path structure of various packages types and states.  In order to keep that information encapsulated
%%% here there are a few path dependent operations in this module as well. 
%%% @copyright (C) 2007, Martin Logan, Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(ewl_installed_paths).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 application_container_path/2,
	 erts_container_path/2,
	 executable_container_path/1,
	 release_file_container_path/3,
	 release_container_path/1
	]).

-export([
	 installed_release_dir_path/3,
	 installed_release_rel_file_path/3,
	 installed_release_bin_dir_path/3,
	 installed_release_cmds_dir_path/3,
	 installed_app_dir_path/4,
	 installed_erts_path/2,
	 installed_config_file_path/4
	]).

%%====================================================================
%% Fundamental Paths
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Returns the path to the directory releases are stored in.
%% @spec release_container_path(InstallationPath) -> string()
%% @end
%%--------------------------------------------------------------------
release_container_path(InstallationPath) ->
    lists:flatten([InstallationPath, "/release_packages"]).

%%--------------------------------------------------------------------
%% @doc Returns the path to the directory applications are stored in.
%% @spec application_container_path(InstallationPath, ErtsVsn) -> string()
%% @end
%%--------------------------------------------------------------------
application_container_path(InstallationPath, ErtsVsn) ->
    filename:join([InstallationPath, "packages", ErtsVsn, "lib"]).

%%--------------------------------------------------------------------
%% @doc Returns a path to the directory where executable files sit. 
%% @spec executable_container_path(InstallationPath) -> string()
%% @end
%%--------------------------------------------------------------------
executable_container_path(InstallationPath) when is_list(InstallationPath) -> 
    lists:flatten([InstallationPath, "/bin/"]).

%%--------------------------------------------------------------------
%% @doc Returns a path to the directory under which all the erts packages lie.
%% @spec erts_container_path(InstallationPath, ErtsVsn) -> string()
%% @end
%%--------------------------------------------------------------------
erts_container_path(InstallationPath, ErtsVsn) -> 
    filename:join([InstallationPath, "packages", ErtsVsn]).

%%--------------------------------------------------------------------
%% @doc Returns a path to the directory under which the release file sits.
%% @spec release_file_container_path(InstallationPath, RelName, RelVsn) -> string()
%% @end
%%--------------------------------------------------------------------
release_file_container_path(InstallationPath, RelName, RelVsn) ->
    lists:flatten([installed_release_dir_path(InstallationPath, RelName, RelVsn), "/release/"]).

%%====================================================================
%% Package Paths
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns a full installaed erts path.
%% @spec installed_erts_path(InstallationPath, ErtsVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_erts_path(InstallationPath, ErtsVsn) when is_list(ErtsVsn) -> 
    lists:flatten([erts_container_path(InstallationPath, ErtsVsn), "/erts-",  ErtsVsn]).

%%--------------------------------------------------------------------
%% @doc Returns a full installaed application path i.e underneith this directory lies the src and ebin dirs.
%% @spec installed_app_dir_path(InstallationPath, ErtsVsn, AppName, AppVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_app_dir_path(InstallationPath, ErtsVsn, AppName, AppVsn) when is_list(AppVsn) -> 
    lists:flatten([application_container_path(InstallationPath, ErtsVsn), "/", AppName, "-", AppVsn]).

%%--------------------------------------------------------------------
%% @doc Returns a full installaed release path. Under this directory the releases directory would sit and perhaps the bin dir.
%% @spec installed_release_dir_path(InstallationPath, RelName, RelVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_release_dir_path(InstallationPath, RelName, RelVsn) when is_list(RelVsn) -> 
    lists:flatten([release_container_path(InstallationPath), "/", RelName, "-", RelVsn]).

%%--------------------------------------------------------------------
%% @doc Returns the full path to a rel file.
%% @spec installed_release_rel_file_path(InstallationPath, RelName, RelVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_release_rel_file_path(InstallationPath, RelName, RelVsn) -> 
    filename:join([release_file_container_path(InstallationPath, RelName, RelVsn), RelName ++ ".rel"]).

%%--------------------------------------------------------------------
%% @doc Returns the path to the cmds directory in an installed release.
%% @spec installed_release_cmds_dir_path(InstallationPath, RelName, RelVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_release_cmds_dir_path(InstallationPath, RelName, RelVsn) -> 
    ewl_file:join_paths(installed_release_dir_path(InstallationPath, RelName, RelVsn), "cmds").

%%--------------------------------------------------------------------
%% @doc Returns the path to the bin directory in an installed release.
%% @spec installed_release_bin_dir_path(InstallationPath, RelName, RelVsn) -> string()
%% @end
%%--------------------------------------------------------------------
installed_release_bin_dir_path(InstallationPath, RelName, RelVsn) -> 
    filename:join([installed_release_dir_path(InstallationPath, RelName, RelVsn), "bin"]).

%%--------------------------------------------------------------------
%% @doc return the path to config.
%% @spec installed_config_file_path(InstallationPath, RelName, RelVsn, ConfigFileName) -> string()
%% @end
%%--------------------------------------------------------------------
installed_config_file_path(InstallationPath, RelName, RelVsn, ConfigFileName) ->
    filename:join([release_file_container_path(InstallationPath, RelName, RelVsn), ConfigFileName]).
