%%%-------------------------------------------------------------------
%%% Copyright (c) 2006 Erlware
%%%
%%% Permission is hereby granted, free of charge, to any 
%%% person obtaining a copy of this software and associated 
%%% documentation files (the "Software"), to deal in the 
%%% Software without restriction, including without limitation 
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit 
%%% persons to whom the Software is furnished to do so, subject to 
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall 
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------
%%% @author Martin Logan
%%% @doc
%%%  Places an application in a repo.
%%% 
%%% Types:
%%%  @type repo() = string(). Contains address and repo designation. 
%%%   Example: http://www.erlware.org/stable   
%%%  @type repo_suffix() = string(). Contains ErtsVsn/Area/Application/Vsn/TarFile.
%%%  @type timeout() = integer() | infinity. Timeouts are specified in milliseconds.
%%%
%%% @todo add appropriate META directory handling
%%% @end
%%% @copyright 2006
%%%-------------------------------------------------------------------
-module(ewr_put).

%% API
-export([
	 put_erts_package/3,
	 put_source_package/3,
	 put_binary_package/3,
	 put_generic_package/3,
	 put_release_package/3
	]).

%%====================================================================
%% API
%%====================================================================

%%-------------------------------------------------------------------
%% @doc
%%  Place a version of erts that has been compiled for a specific architecture into a repo.
%% <pre>
%% Example:
%%  put_erts_package(["http"//www.erlware.org/stable"], "/usr/local/lib/erlang/erts-5.5.5").
%%
%% Variables: 
%%  ErtsDirPath - a full path to an erts directory.
%% </pre>
%% @spec put_erts_package(Repos::list(), ErtsDirPath::string(), Timeout::timeout()) -> ok | {error, Reason}
%% @end
%%-------------------------------------------------------------------
put_erts_package(Repos, RawErtsDirPath, Timeout) ->
    try 
	ErtsDirPath = string:strip(filename:absname(RawErtsDirPath), right, $/),	
        error_logger:info_msg("ewr_put:put_erts_package(~p, ~p)", [Repos, ErtsDirPath]),
        {ok, {"erts", ErtsVsn}} = package_dir_to_name_and_vsn(ErtsDirPath),
        {ok, TmpDir}            = ewl_file:create_tmp_dir(filename:dirname(ErtsDirPath)),
        TmpErtsDirPath          = lists:flatten([TmpDir, "/erts/", ErtsVsn]),
        ewl_file:mkdir_p(TmpErtsDirPath),
        ok                      = ewl_file:copy_dir(ErtsDirPath, TmpErtsDirPath),
	SysInfo                 = ewr_util:system_info(),
        Suffix                  = ewr_repo_paths:erts_package_suffix(ErtsVsn, SysInfo),
        {ok, TarFile}           = tar_file_binary(filename:basename(Suffix), TmpDir ++ "/erts"),
        {ok, _URL}              = Res = repos_put(Repos, Suffix, TarFile, Timeout),
        ewl_file:delete_dir(TmpDir),
        Res
    catch 
	_Class:Exception ->
	    {error, Exception}
    end.

%%-------------------------------------------------------------------
%% @doc
%%  Place an unbuilt  application into a local open repository in 
%%  the version agnostic repository location.
%% <pre>
%% Example:
%%  put_source_package(["http"//www.erlware.org/stable"], "/home/jdoe/my_proj/lib/my_app").
%%
%% Variables: 
%%  AppDir - a full path to the directory of the app to be put.
%% </pre>
%% @spec put_source_package(Repos::list(), AppDir::string(), Timeout::timeout()) -> ok | {error, Reason}
%% @end
%%-------------------------------------------------------------------
put_source_package(Repos, AppDir, Timeout) ->
    put_application_package(Repos, AppDir, ["Sources"], Timeout).

%%-------------------------------------------------------------------
%% @doc
%%  Place a machine architecture neutral package in a repo.  These sorts of packages are typically comprised of pure erlang code.
%% <pre>
%% Example:
%%  put_generic_package(["http"//www.erlware.org/stable"], "/home/jdoe/my_proj/lib/my_app").
%%
%% Variables: 
%%  AppDir - a full path to the directory of the app to be put.
%% </pre>
%% @spec put_generic_package(Repos::list(), AppDir::string(), Timeout::timeout()) -> ok | {error, Reason}
%% @end
%%-------------------------------------------------------------------
put_generic_package(Repos, AppDir, Timeout) ->
    put_application_package(Repos, AppDir, ["Generic"], Timeout).

%%-------------------------------------------------------------------
%% @doc
%%  Place an architecture specific application into a local open 
%%  repository.
%% <pre>
%% Example:
%%  put_binary_package(["http"//www.erlware.org/stable"], "/home/jdoe/my_proj/lib/my_app").
%%
%% Variables: 
%%  AppDir - a full path to the directory of the app to be put.
%% </pre>
%% @spec put_binary_package(Repos::list(), AppDir::string(), Timeout::timeout()) -> ok | {error, Reason}
%% @end
%%-------------------------------------------------------------------
put_binary_package(Repos, AppDir, Timeout) ->
    SysInfo = ewr_util:system_info(),
    put_application_package(Repos, AppDir, [SysInfo], Timeout).

%%-------------------------------------------------------------------
%% @todo this function should match the workings of put application package and put erts package. i.e no longer exit. 
%% @doc
%%  Place a machine architecture neutral package in a repo.  These sorts of packages are typically comprised of pure erlang code.
%%  Note releases are put into the repo under generic.  If there is any architecture specific code in a release it is currently
%%  up to the author to add a file directly underneith the release called "build".  This build file will be executed following
%%  the installation of all applications in the release file.  This build file should handle building all platform specific code
%%  for the release.
%% <pre>
%% Example:
%%  put_release_package(["http"//www.erlware.org/stable"], "/home/jdoe/my_rel").
%%
%% Variables: 
%%  ReleaseDirPath - path to a release directory. 
%% </pre>
%% @spec put_release_package(Repos, ReleaseDirPath, Timeout::timeout()) -> {ok, URL} | exit()
%%   where
%%    Repos = [string()]
%%    RelName = string()
%%    RelVsn = string()
%%    ReleaseDirPath = string() 
%% @end
%%-------------------------------------------------------------------
put_release_package(Repos, RawReleaseDirPath, Timeout) ->
    ReleaseDirPath = string:strip(filename:absname(RawReleaseDirPath), right, $/),	
    error_logger:info_msg("ewr_put:put_release_package(~p, ~p)", [Repos, ReleaseDirPath]),
    {ok, {RelName, RelVsn}} = package_dir_to_name_and_vsn(ReleaseDirPath),
    case filelib:is_dir(ReleaseDirPath ++ "/lib") of
	true -> exit({error, "can't publish a release with a lib directory, publish applications separately"});
	false -> ok
    end,
    Suffix        = ewr_repo_paths:package_suffix(ewr_util:erts_version(), "Generic", "releases", RelName, RelVsn),
    {ok, TarFile} = tar_file_binary(filename:basename(Suffix), ReleaseDirPath),
    {ok, _URL}    = Res = repos_put(Repos, Suffix, TarFile, Timeout),
    Res.




%%====================================================================
%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @private
%% @doc
%%  Put bits onto multiple filesystems.  This function creates the directory strcuture speficied if it does not exist.
%% <pre>
%% Example:
%%  repos_put(["http://www.erlware.org/stable"], "home/jdoe/my_proj/lib/my_app/my_app.tar.gz", MyAppTarBinary, 100000).
%%
%% Variables: 
%%  AppDir - a full path to the directory of the app to be put.
%%  URL - The url that the payload was PUT to.
%% </pre>
%% @spec repos_put(Repos::list(), Suffix::string(), Payload::binary(), Timeout::timeout()) -> {ok, URLS} | {error, ErrorReport}
%% @end
%%-------------------------------------------------------------------
repos_put(Repos, Suffix, Payload, Timeout) ->
    payloads_put(Repos, fun(Repo) -> ewr_repo_dav:repo_put(Repo, Suffix, Payload, Timeout) end).
				 

%%-------------------------------------------------------------------
%% @private
%% @doc
%%  Put bits onto multiple filesystems.  This function creates the directory strcuture speficied if it does not exist.
%% <pre>
%% Example:
%%  payloads_put(["http://www.erlware.org/stable"], fun(Repo) -> ewr_repo_dav:repo_put(Repo, Suffix, Payload, Timeout) end).
%%
%% Variables: 
%%  AppDir - a full path to the directory of the app to be put.
%%  URL - The url that the payload was PUT to.
%% </pre>
%% @spec payloads_put(Repos::list(), PayloadFun::fun()) -> {ok, URLS} | {error, ErrorReport}
%% @end
%%-------------------------------------------------------------------
payloads_put(Repos, PayloadFun) ->
     Res = 
	lists:foldl(fun(Repo, {Good, Bad}) -> 
			    case catch PayloadFun(Repo) of
				{ok, Url} -> {[Url|Good], Bad};
				Error     -> {Good, [{Repo, Error}|Bad]}
			    end 
		    end,
		    {[], []}, Repos),
    case Res of
	{Success, []}      -> {ok, Success};
	{[], Failure}      -> {error, {publish_failure, Failure}};
	{Success, Failure} -> {error, {publish_partial_failure, {publish_success, Success}, {publish_failure, Failure}}}
    end.
			   

%%-------------------------------------------------------------------
%% @private
%% @doc 
%%  Place a package into a remote repo.  This top level function
%%  coordinates all activities involved with that. 
%% @spec put_application_package(Repos, RawAppDir, Areas, Timeout) -> {ok, [URL]} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
put_application_package(Repos, RawAppDir, Areas, Timeout) ->
    try 
        AppDir                = filename:absname(RawAppDir),
        AppName               = ewr_util:app_name(AppDir),
	{ok, [{vsn, AppVsn}]} = ewr_util:fetch_local_appfile_key_values(AppDir, [vsn]),
        {ok, _URLS} = Result  = just_put_application_package(Repos, AppName, AppVsn, AppDir, Areas, Timeout),
        Result
    catch 
        _Class:Exception ->
            {error, Exception}
    end.


%%-------------------------------------------------------------------
%% @private
%%
%% @todo Instead of fetching latest version from the .app file get it from the vsn list
%% @todo Lock the version file
%%
%% @doc
%%  Handles placeing the AppName-Vsn.app file 
%%
%% @spec put_dot_app_file(Repo::string(), AppName::string(), AppVsn::string(), AppDir::string(), Timeout::integer()) -> 
%%               {ok, URL} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
put_dot_app_file(Repo, AppName, AppVsn, AppDir, Timeout) ->
    {ok, AppFile} = file:read_file(lists:flatten([AppDir, "/ebin/", AppName, ".app"])),
    Suffix        = ewr_repo_paths:dot_app_file_suffix(ewr_util:erts_version(), AppName, AppVsn),
    ewr_repo_dav:repo_put(Repo, Suffix, AppFile, Timeout).
    

%%-------------------------------------------------------------------
%% @private
%% @doc
%%  Put an application package out to a repo in the Areas specified.  If the package has not been tarred do so before 
%%  placing it. On success this function returns {ok, URL} where URL is the url to which the application was placed, 
%%  if multiple areas are specified a list of all URLs placed to will be returned; {ok, [URL]}.
%%
%% @spec just_put_application_package(Repos::list(), AppName::string(), AppVsn::string(), 
%%                           RawAppDir::string(), Areas::list(), Timeout) -> 
%%                        {ok, URL} | {ok, [URL]} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
just_put_application_package(Repos, AppName, AppVsn, RawAppDir, Areas, Timeout) ->
    AppDir          = string:strip(RawAppDir, right, $/),	
    URLPathSuffixes = lists:map(fun(Area) -> 
					ewr_repo_paths:package_suffix(ewr_util:erts_version(), Area, "lib", AppName, AppVsn)
				end, Areas),
    TarName = filename:basename(hd(URLPathSuffixes)),

    {ok, TarBinary} = 
	case regexp:match(AppDir, ".*-" ++ AppVsn) of
	    {match, _, _} -> 
		TarDir = AppDir,
		io:format("Target dir already contains version number~n"),
		tar_file_binary(TarName, TarDir);
	    nomatch ->                      
		TarDirName = filename:basename(AppDir) ++ "-" ++ AppVsn,
		TarDir = lists:flatten([filename:dirname(AppDir), "/", TarDirName]),
		file:rename(AppDir, TarDir),
		{ok, TarBinary_} = tar_file_binary(TarName, TarDir),
		file:rename(TarDir, AppDir),
		{ok, TarBinary_}
	end,

    MapRes = lists:map(fun(Suf) -> 
			       PayloadFun = fun(Repo) -> 
						    {ok, URL} = ewr_repo_dav:repo_put(Repo, Suf, TarBinary, Timeout),
						    {ok, _MetaURL} = put_dot_app_file(Repo, AppName, AppVsn, AppDir, Timeout),
						    {ok, URL}
					    end,
			       {ok, URLs} = payloads_put(Repos, PayloadFun),
			       URLs
		   end, URLPathSuffixes),
    case MapRes of
	[URL]                      -> {ok, URL};
	URLS when length(URLS) > 1 -> {ok, URLS}
    end.
	    
    


package_dir_to_name_and_vsn(RawPackageDir) ->
    PackageDir = filename:basename(RawPackageDir),
    case regexp:match(PackageDir, "^[a-zA-Z_]+-[0-9\.]+") of
	{match, 1, _} ->
	    {ok, {[PackageName], PackageVsn}} = ewl_string_manip:n_tokens(PackageDir, 1, "-"),
	    {ok, {PackageName, PackageVsn}};
	_Error -> 
	    {error, "RelTarFile did not match <relname>-<rel version>.tar.gz"}
    end.

%% Creates a tar file and returns the binary.
tar_file_binary(TarName, TarDir) ->
    TarDirBase = filename:basename(TarDir),
    %% Add the tar file name to the end of each path suffix and the repo to the beginning. 
    io:format("Creating tar file, ~p from ~p~n", [TarName, TarDir]),
    % XXX May not work on windows
    {ok, CWD}     = file:get_cwd(),
    ok            = file:set_cwd(filename:dirname(TarDir)),
    ok            = ewl_file:compress(TarName, TarDirBase),
    {ok, TarFile} = file:read_file("./" ++ TarName),
    ok            = file:delete(TarFile),
    ok            = file:set_cwd(CWD),
    {ok, TarFile}.

