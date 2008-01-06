%%%-------------------------------------------------------------------
%%% Copyright: Eric Merritt, Martin Logan, Erlware
%%% File     : ewr_repo_dav.erl
%%% Author   : Martin Logan <martinjlogan@erlware.org>
%%%
%%% @doc This module houses respository structure aware functions in ewrepo. *Note* this file should not
%%%      contain any convenience functions or shortcuts.  Those should be placed in higher level modules so that this 
%%%      stays free of any clutter.
%%%
%%% <pre>
%%% Example Suffix Breakdown: 
%%%          /5.5.5/Generic/lib/mnesia/2.3 
%%%          ErtsVsn/Area/Side/PackageName/PackageVsn
%%%
%%%          /5.5.5/Generic/lib/mnesia/2.3/mnesia.tar.tz 
%%%          /5.5.5/Meta/release/sinan/1.0/sinan.rel
%%%          ErtsVsn/Area/Side/PackageName/PackageVsn/File
%%%
%%% Types:
%%%  Area = "Generic" | "Meta" | Architecture
%%%   Architecture = string()
%%%  Side = "lib" | "releases"
%%% </pre>
%%%
%%% @end
%%%
%%% Created : 30 Nov 2007 by Martin Logan <martinjlogan@erlware.org>
%%%-------------------------------------------------------------------
-module(ewr_repo_paths).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("eunit.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 package_vsn_suffix/5,
	 package_name_suffix/4,
	 side_suffix/3,
	 area_suffix/2,
	 erts_suffix/1
        ]).

-export([
	 package_suffix/5,
	 erts_package_suffix/2,
	 dot_app_file_suffix/3,
	 dot_rel_file_suffix/3
        ]).

%%====================================================================
%% External functions
%%====================================================================

%%====================================================================
%% The following functions return just paths as opposed to paths to files
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given erts version. 
%% @spec erts_suffix(ErtsVsn::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
erts_suffix(ErtsVsn) when is_list(ErtsVsn) ->
    lists:flatten(["/", ErtsVsn]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given area. 
%% @spec area_suffix(ErtsVsn::string(), Area::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
area_suffix(ErtsVsn, Area) when is_list(Area) ->
    ewl_file:join_paths(erts_suffix(ErtsVsn), Area).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given side; lib, or releases, etc... 
%% @spec side_suffix(ErtsVsn::string(), Area::string(), Side::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
%% TODO This first clause will be removed when Meta area gets Sides. This will happen after the new Sinan 0.9.0.0 is tested stable.
side_suffix(ErtsVsn, "Meta" = Area, Side) when Side == "lib"; Side == "releases" ->
    area_suffix(ErtsVsn, Area);
side_suffix(ErtsVsn, Area, Side) when Side == "lib"; Side == "releases" ->
    ewl_file:join_paths(area_suffix(ErtsVsn, Area), Side).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given package name.
%% @spec package_name_suffix(ErtsVsn::string(), Area::string(), Side::string(), PackageName::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
package_name_suffix(ErtsVsn, Area, Side, PackageName) when is_list(PackageName) ->
    ewl_file:join_paths(side_suffix(ErtsVsn, Area, Side), PackageName).
    
%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given package name.
%% @spec package_vsn_suffix(ErtsVsn::string(), Area::string(), Side::string(), PackageName::string(), PackageVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
package_vsn_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn) when is_list(PackageVsn) ->
    ewl_file:join_paths(package_name_suffix(ErtsVsn, Area, Side, PackageName), PackageVsn).
    

%%====================================================================
%% The following functions return paths to actual files in the repo structure
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the tarball for a given erts vsn.
%% @spec erts_package_suffix(ErtsVsn::string(), Area::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
erts_package_suffix(ErtsVsn, Area) when is_list(ErtsVsn) ->
    ewl_file:join_paths(area_suffix(ErtsVsn, Area), "erts.tar.gz").

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the actual package for the given name and version.
%% @spec package_suffix(ErtsVsn::string(), Area::string(), Side::string(), PackageName::string(), PackageVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn) when is_list(PackageVsn) ->
    lists:flatten([package_vsn_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn), "/", PackageName, ".tar.gz"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the .app file to be stored in the repo.
%% @spec dot_app_file_suffix(ErtsVsn::string(), AppName::string(), AppVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
dot_app_file_suffix(ErtsVsn, AppName, AppVsn) when is_list(AppVsn) ->
    lists:flatten([package_vsn_suffix(ErtsVsn, "Meta", "lib", AppName, AppVsn), "/", AppName, ".app"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the .rel file to be stored in the repo.
%% @spec dot_rel_file_suffix(ErtsVsn::string(), ReleaseName::string(), ReleaseVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
dot_rel_file_suffix(ErtsVsn, ReleaseName, ReleaseVsn) when is_list(ReleaseVsn) ->
    lists:flatten([package_vsn_suffix(ErtsVsn, "Meta", "releases", ReleaseName, ReleaseVsn), "/", ReleaseName, ".rel"]).

%%====================================================================
%% Internal functions
%%====================================================================

dot_rel_file_suffix_test() ->
    ?assertMatch("/5.5.5/Meta/faxien/1.0/faxien.rel", dot_rel_file_suffix("5.5.5", "faxien", "1.0")).

erts_package_suffix_test() ->
    ?assertMatch("/5.5.5/myos/erts.tar.gz", erts_package_suffix("5.5.5", "myos")).

package_vsn_suffix_test() ->
    ?assertMatch("/5.5.5/Generic/lib/mnesia/1.0", package_vsn_suffix("5.5.5", "Generic", "lib", "mnesia", "1.0")).
