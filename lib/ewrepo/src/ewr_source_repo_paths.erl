%%%-------------------------------------------------------------------
%%% @copyright Erlware
%%% @author    Martin Logan <martinjlogan@erlware.org>
%%%
%%% @doc This module houses respository structure aware functions in ewrepo. *Note* this file should not
%%%      contain any convenience functions or shortcuts.  Those should be placed in higher level modules so that this 
%%%      stays free of any clutter.
%%%
%%% <pre>
%%% Example Suffix Breakdown: 
%%%          lib/mnesia/2.3
%%%          Side/PackageName/PackageVsn
%%%
%%%          lib/mochiweb/2.3/mochiweb_edoc.tar.tz 
%%%          release/sinan/1.0/sinan.rel
%%%          Side/PackageName/PackageVsn/File
%%% </pre>
%%%
%%%  @type area() = string() | Generic | Meta 
%%%
%%% @end
%%%
%%% Created : 30 Nov 2007 by Martin Logan <martinjlogan@erlware.org>
%%%-------------------------------------------------------------------
-module(ewr_source_repo_paths).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("eunit.hrl").
-include("ewrepo.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 package_vsn_suffix/3,
	 package_name_suffix/2,
	 side_suffix/1
        ]).

-export([
	 package_suffix/3,
	 dot_app_file_suffix/2,
	 dot_rel_file_suffix/2,
	 release_control_file_suffix/2,
	 signature_file_suffix/3,
	 checksum_file_suffix/3
        ]).

-export([
	 decompose_suffix/1
        ]).
%%====================================================================
%% External functions
%%====================================================================

%%====================================================================
%% The following functions return just paths as opposed to paths to files
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given side; lib, or releases, etc... 
%% @spec side_suffix(ErtsVsn::string(), Area::string(), Side::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
%% TODO This first clause will be removed when Meta area gets Sides. This will happen after the new Sinan 0.9.0.0 is tested stable.
side_suffix(Side) when Side == "lib"; Side == "releases" ->
    lists:flatten(["/", Side]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given package name.
%% @spec package_name_suffix(Side::string(), PackageName::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
package_name_suffix(Side, PackageName) when is_list(PackageName) ->
    filename:join([side_suffix(Side), PackageName]).
    
%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given package name and version.
%% @spec package_vsn_suffix(ErtsVsn::string(), Area::string(), Side::string(), PackageName::string(), PackageVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
package_vsn_suffix(Side, PackageName, PackageVsn) when is_list(PackageVsn) ->
    filename:join([package_name_suffix(Side, PackageName), PackageVsn]).
    
%%====================================================================
%% The following functions return paths to actual files in the repo structure
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the actual package for the given name and version.
%% @spec package_suffix(ErtsVsn::string(), Area::string(), Side::string(), PackageName::string(), PackageVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
package_suffix(Side, PackageName, PackageVsn) when is_list(PackageVsn) ->
    lists:flatten([package_vsn_suffix(Side, PackageName, PackageVsn), "/", PackageName, ".tar.gz"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the .app file to be stored in the repo.
%% @spec dot_app_file_suffix(ErtsVsn::string(), AppName::string(), AppVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
dot_app_file_suffix(AppName, AppVsn) when is_list(AppVsn) ->
    lists:flatten([package_vsn_suffix("lib", AppName, AppVsn), "/", AppName, ".app"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the .rel file to be stored in the repo.
%% @spec dot_rel_file_suffix(ErtsVsn::string(), ReleaseName::string(), ReleaseVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
dot_rel_file_suffix(ReleaseName, ReleaseVsn) when is_list(ReleaseVsn) ->
    lists:flatten([package_vsn_suffix("releases", ReleaseName, ReleaseVsn), "/", ReleaseName, ".rel"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the control file to be stored in the repo.
%% @spec release_control_file_suffix(ErtsVsn::string(), ReleaseName::string(), ReleaseVsn::string()) -> string()
%% @end 
%%--------------------------------------------------------------------
release_control_file_suffix(ReleaseName, ReleaseVsn) when is_list(ReleaseVsn) ->
    filename:join([package_vsn_suffix("releases", ReleaseName, ReleaseVsn), "control"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the signature file stored in the repo for a package (does not apply to erts).
%% @spec signature_file_suffix(ErtsVsn::string(), Side::string(), PackageName::string(), PackageVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
signature_file_suffix(Side, PackageName, PackageVsn) when is_list(PackageVsn) ->
    lists:flatten([package_vsn_suffix(Side, PackageName, PackageVsn), "/", "signature"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the checksum file stored in the repo for a package (does not apply to erts).
%% @spec checksum_file_suffix(ErtsVsn::string(), Side::string(), PackageName::string(), PackageVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
checksum_file_suffix(Side, PackageName, PackageVsn) when is_list(PackageVsn) ->
    lists:flatten([package_vsn_suffix(Side, PackageName, PackageVsn), "/", "checksum"]).

%%====================================================================
%% Other External Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the .rel file to be stored in the repo.
%% @spec decompose_suffix(Suffix) -> {ok, [Segment]} | {error, Reason}
%% where
%%  Segment = {Type, SegmentText}
%%   Type = erts_vsn | area | side | package_name | package_vsn | file
%% @end
%%--------------------------------------------------------------------
decompose_suffix(Suffix) ->
    Tokens = string:tokens(Suffix, "/"),
    try 
	analyse_tokens(Tokens)
    catch 
	_Class:Exception ->
	    Exception
    end.
	
%%====================================================================
%% Internal Functions
%%====================================================================

analyse_tokens(Tokens) ->
    side(Tokens).

side([]) ->	    
    [];
side([Side|T]) when Side == "lib"; Side == "releases" ->
    [{side, Side}|package_name(T)];
side([Side|_]) ->
    throw({error, {bad_side, Side}}).

package_name([]) ->
    [];
package_name([PackageName|T]) ->
    case regexp:match(PackageName, "^" ++ ?PACKAGE_NAME_REGEXP) of
	{match, 1, Length} when length(PackageName) == Length ->
	    [{package_name, PackageName}|package_vsn(T)];
	_Error ->
	    throw({error, {bad_package_name, PackageName}})
    end.
    
package_vsn([]) ->
    [];
package_vsn([PackageVsn|T]) ->
    case regexp:match(PackageVsn, "^" ++ ?PACKAGE_VSN_REGEXP) of
	{match, 1, Length} when length(PackageVsn) == Length ->
	    [{package_vsn, PackageVsn}|file(T)];
	_Error ->
	    throw({error, {bad_package_vsn, PackageVsn}})
    end.

file([]) ->
    [];
file([File]) ->
    case regexp:match(File, ?REPO_FILE_EXT_REGEXP) of
	{match, _, _} ->
	    [{file, File}];
	_Error ->
	    throw({error, {bad_file, File}})
    end.
    
    
%%====================================================================
%% Test Functions
%%====================================================================

dot_app_file_suffix_test() ->
    ?assertMatch("/lib/faxien/1.0/faxien.app", dot_app_file_suffix("faxien", "1.0")).

dot_rel_file_suffix_test() ->
    ?assertMatch("/releases/faxien/1.0/faxien.rel", dot_rel_file_suffix("faxien", "1.0")).

package_vsn_suffix_test() ->
    ?assertMatch("/lib/mnesia/1.0", package_vsn_suffix("lib", "mnesia", "1.0")).

decompose_suffix_test() ->
    ?assertMatch({error, {bad_file, "gas.tar.z"}}, 
		 decompose_suffix("lib/gas/5.1.0/gas.tar.z")),

    ?assertMatch([{side, "lib"}],
		  decompose_suffix("lib")),

    ?assertMatch([{side, "lib"}, {package_name, "gas"},
		  {package_vsn, "5.1.0"}, {file, "gas.tar.gz"}], 
		  decompose_suffix("lib/gas/5.1.0/gas.tar.gz")),

    ?assertMatch([{side, "lib"}, {package_name, "gas"},
		  {package_vsn, "5.1-alpha"}, {file, "gas.tar.gz"}], 
		  decompose_suffix("lib/gas/5.1-alpha/gas.tar.gz")).
    
