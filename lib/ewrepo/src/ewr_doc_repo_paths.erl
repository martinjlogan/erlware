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
%%%          lib/mnesia/2.3/html
%%%          Side/PackageName/PackageVsn/DocFormat
%%%
%%%          lib/mochiweb/2.3/html/mochiweb_edoc.tar.tz 
%%%          release/sinan/1.0/html/sinan.rel
%%%          Side/PackageName/PackageVsn/DocFormat/File
%%% </pre>
%%%
%%%  @type area() = string() | Generic | Meta 
%%%
%%% @end
%%%
%%% Created : 30 Nov 2007 by Martin Logan <martinjlogan@erlware.org>
%%%-------------------------------------------------------------------
-module(ewr_doc_repo_paths).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("eunit.hrl").
-include("ewrepo.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 doc_type_suffix/4,
	 package_vsn_suffix/3,
	 package_name_suffix/2,
	 side_suffix/1
        ]).

-export([
	 signature_file_suffix/4,
	 package_suffix/4,
	 checksum_file_suffix/4
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
%% @spec package_vsn_suffix(Side::string(), PackageName::string(), PackageVsn::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
package_vsn_suffix(Side, PackageName, PackageVsn) when is_list(PackageVsn) ->
    filename:join([package_name_suffix(Side, PackageName), PackageVsn]).
    
%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the repo location for a given package name version and doc type.
%% @spec package_vsn_suffix(Side::string(), PackageName::string(), PackageVsn::string(), DocType::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
doc_type_suffix(Side, PackageName, PackageVsn, DocType) when is_list(PackageVsn) ->
    filename:join([package_vsn_suffix(Side, PackageName, PackageVsn), DocType]).

%%====================================================================
%% The following functions return paths to actual files in the repo structure
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the signature file stored in the repo for a package 
%% @spec signature_file_suffix(Side::string(), PackageName::string(), PackageVsn::string(), DocType::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
signature_file_suffix(Side, PackageName, PackageVsn, DocType) when is_list(PackageVsn) ->
    filename:join([doc_type_suffix(Side, PackageName, PackageVsn, DocType), "signature"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the actual package for the given name and version.
%% @spec package_suffix(Side::string(), PackageName::string(), PackageVsn::string(), DocType::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
package_suffix(Side, PackageName, PackageVsn, DocType) when is_list(PackageVsn) ->
    lists:flatten([doc_type_suffix(Side, PackageName, PackageVsn, DocType), "/", PackageName, ".tar.gz"]).

%%--------------------------------------------------------------------
%% @doc Returns the suffix pointing to the checksum file stored in the repo for a package (does not apply to erts).
%% @spec checksum_file_suffix(Side::string(), PackageName::string(), PackageVsn::string(), DocType::string()) -> 
%%        string()
%% @end 
%%--------------------------------------------------------------------
checksum_file_suffix(Side, PackageName, PackageVsn, DocType) when is_list(PackageVsn) ->
    filename:join([doc_type_suffix(Side, PackageName, PackageVsn, DocType), "checksum"]).

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
	    [{package_vsn, PackageVsn}|doc_type(T)];
	_Error ->
	    throw({error, {bad_package_vsn, PackageVsn}})
    end.

doc_type([]) ->
    [];
doc_type([DocType|T]) ->
    case lists:member(DocType, ["edoc"]) of
	true ->
	    [{doc_type, DocType}|file(T)];
	false ->
	    throw({error, {bad_doc_type, DocType}})
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

package_vsn_suffix_test() ->
    ?assertMatch("/lib/mnesia/1.0", package_vsn_suffix("lib", "mnesia", "1.0")),

    ?assertMatch({error, {bad_file, "gas.tar.z"}}, 
		 decompose_suffix("/lib/gas/5.1.0/edoc/gas.tar.z")),

    ?assertMatch([{side, "lib"}],
		  decompose_suffix("lib")),

    ?assertMatch([{side, "lib"}, {package_name, "gas"},
		  {package_vsn, "5.1.0"}, {doc_type, "edoc"}, {file, "gas.tar.gz"}], 
		  decompose_suffix("/lib/gas/5.1.0/edoc/gas.tar.gz")),

    ?assertMatch([{side, "lib"}, {package_name, "gas"},
		  {package_vsn, "5.1-alpha"}, {doc_type, "edoc"}, {file, "gas.tar.gz"}], 
		  decompose_suffix("lib/gas/5.1-alpha/edoc/gas.tar.gz")).
    
