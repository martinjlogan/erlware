%%%-------------------------------------------------------------------
%%% Copyright (c) 2006 Eric Merritt, Martin Logan
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
%%% @author Eric Merritt,  Martin Logan
%%% @copyright 2006 Erlware
%%% @doc
%%%   Gets the list of fetchables from the environment and loads them
%%%   into the local repository.
%%%
%%% Types:
%%%  @type repo() = string(). Contains address and repo designation.
%%%   Example: http://www.erlware.org/stable
%%%  @type timeout() = integer() | infinity. Timeouts are specified in milliseconds.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ewr_fetch).

%% API
-export([
	 fetch_release_package/6,
	 fetch_release_package/5,
	 fetch_release_package/4,
         fetch_release_packages/5,
         fetch_release_packages/4,
         fetch_release_packages/3,
	 fetch_binary_package/6,
	 fetch_binary_package/5,
	 fetch_binary_package/4,
         fetch_binary_packages/5,
         fetch_binary_packages/4,
         fetch_binary_packages/3,
         fetch_source_package/6,
         fetch_source_package/5,
         fetch_source_package/4,
         fetch_source_packages/5,
         fetch_source_packages/4,
         fetch_source_packages/3,
	 fetch_erts_package/4,
	 fetch_erts_package/3,
	 fetch_package/8,
	 fetch_package/7
	]).

-define(FETCH_TIMEOUT, infinity).

-include("eunit.hrl").

%%====================================================================
%% API
%%====================================================================

%%-------------------------------------------------------------------
%% @spec fetch_source_package(Repos::list(), ErtsVsn, Package::string(), Version::string(), To::string(), Timeout::timeout()) ->
%%                            ok | {error, Reason}
%% where
%%  Timeout = integer() | infinity
%% @doc
%%  Fetch the single source package (Package and Version) into the
%%  location specified (To) from the repositories (Repos).
%%  usage
%%
%%  ``ewr_fetch:fetch_source_package(["http://urltorepo/stable"], "5.5.5", "maypackage", "1.2.3", "./target").''
%% @end
%%-------------------------------------------------------------------
fetch_source_package(Repos, ErtsVsn, Package, Version, To, Timeout) ->
    fetch_package(Repos, ErtsVsn, Package, Version, To, ["Sources"], lib, Timeout).

%% @spec fetch_source_package(Repos::list(), Package::string(), Version::string(), To::string(), Timeout) -> ok | {error, Reason}
%% @equiv fetch_source_package(Repos, ErtsVsn, Package, Version, To, Timeout)
fetch_source_package(Repos, Package, Version, To, Timeout) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_source_package(Repos, ErtsVsn, Package, Version, To, Timeout).

%% @spec fetch_source_package(Repos::list(), Package::string(), Version::string(), To::string()) -> ok | {error, Reason}
%% @equiv fetch_source_package(Repos, ErtsVsn, Package, Version, To)
fetch_source_package(Repos, Package, Version, To) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_source_package(Repos, ErtsVsn, Package, Version, To, ?FETCH_TIMEOUT).

%%-------------------------------------------------------------------
%% @spec fetch_source_packages(Repos::list(), ErtsVsn::string(), PackageList::list(), To::string(), Timeout::timeout()) ->
%%                             ok | {error, Reason}
%% @doc
%%  Fetch all packages in the list (PackageList {Name, Version}), into
%%  the location (To) from the repositories (Repos).
%%
%%  ``ewr_fetch:fetch_source_packages(["http://urltorepo/stable"],
%%                                    "5.5.5",
%%                                   [{"maypackage", "1.2.3"},
%%                                    {"another", "0.1.2"}],
%%                                   "./target",
%%                                   infinity).''
%% @end
%%-------------------------------------------------------------------
fetch_source_packages(Repos, ErtsVsn, [{Name, Version}|T], To, Timeout) ->
    fetch_source_package(Repos, ErtsVsn, Name, Version, To, Timeout),
    fetch_source_packages(Repos, ErtsVsn, T, To, Timeout);
fetch_source_packages(_Repos, _ErtsVsn, [], _To, _Timeout) ->
    ok.

%% @spec fetch_source_packages(Repos::list(), PackageList::list(), To::string(), Timeout::integer()) -> ok | {error, Reason}
%% @equiv fetch_source_packages(Repos, ErtsVsn, PackageList, To, Timeout)
fetch_source_packages(Repos, PackageList, To, Timeout) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_source_packages(Repos, ErtsVsn, PackageList, To, Timeout).

%% @spec fetch_source_packages(Repos::list(), PackageList::list(), To::string()) -> ok | {error, Reason}
%% @equiv fetch_source_packages(Repos, PackageList, To, Timeout)
fetch_source_packages(Repos, PackageList, To) ->
    fetch_source_packages(Repos, PackageList, To, ?FETCH_TIMEOUT).


%%-------------------------------------------------------------------
%% @spec fetch_binary_package(Repos::list(), ErtsVsn, Package::string(), Version::string(), To::string(), Timeout::timeout()) ->
%%                            ok | {error, Reason}
%% @doc
%%  Fetch the single binary package (Package and Version) into the
%%  location specified (To) from the repositories (Repos).
%%
%%  ``ewr_fetch:fetch_binary_package(["http://urltorepo/stable"],
%%                                   "5.5.5",
%%                                   "mypackage",
%%                                   "1.2.3",
%%                                   "./target",
%%                                   10000).''
%% @end
%%-------------------------------------------------------------------
fetch_binary_package(Repos, ErtsVsn, Package, Version, To, Timeout) ->
    SysInfo = ewr_util:system_info(),
    Areas = ["Generic"|ewr_util:create_system_info_series(SysInfo)],
    fetch_package(Repos, ErtsVsn, Package, Version, To, Areas, lib, Timeout).

%% @spec fetch_binary_package(Repos::list(), Package::string(), Version::string(), To::string(), Timeout) -> ok | {error, Reason}
%% @equiv fetch_binary_package(Repos, ErtsVsn, Package, Version, To, Timeout)
fetch_binary_package(Repos, Package, Version, To, Timeout) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_binary_package(Repos, ErtsVsn, Package, Version, To, Timeout).

%% @spec fetch_binary_package(Repos::list(), Package::string(), Version::string(), To::string()) -> ok | {error, Reason}
%% @equiv fetch_binary_package(Repos, Package, Version, To, Timeout)
fetch_binary_package(Repos, Package, Version, To) ->
    fetch_binary_package(Repos, Package, Version, To, ?FETCH_TIMEOUT).

%%-------------------------------------------------------------------
%% @spec fetch_binary_packages(Repos::list(), ErtsVsn::string(), PackageList::list(), To::string(), Timeout::timeout()) ->
%%                             ok | {error, Reason}
%% @doc
%%  Fetch all packages in the list (PackageList {Name, Version}), into
%%  the location (To) from the repositories (Repos).
%%
%%  ``ewr_fetch:fetch_binary_packages(["http://urltorepo/stable"],
%%                                   "5.5.5",
%%                                   [{"mypackage", "1.2.3"},
%%                                    {"another", "0.1.2"}],
%%                                   "./target",
%%                                   infinity).''
%% @end
%%-------------------------------------------------------------------
fetch_binary_packages(Repos, ErtsVsn, [{Name, Version}|T], To, Timeout) ->
    fetch_binary_package(Repos, ErtsVsn, Name, Version, To, Timeout),
    fetch_binary_packages(Repos, ErtsVsn, T, To, Timeout);
fetch_binary_packages(_Repos, _ErtsVsn, [], _To, _Timeout) ->
    ok.

%% @spec fetch_binary_packages(Repos::list(), PackageList::list(), To::string(), Timeout::integer()) -> ok | {error, Reason}
%% @equiv fetch_binary_packages(Repos, ErtsVsn, PackageList, To, Timeout)
fetch_binary_packages(Repos, PackageList, To, Timeout) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_binary_packages(Repos, ErtsVsn, PackageList, To, Timeout).

%% @spec fetch_binary_packages(Repos::list(), PackageList::list(), To::string()) -> ok | {error, Reason}
%% @equiv fetch_binary_packages(Repos, PackageList, To, Timeout)
fetch_binary_packages(Repos, PackageList, To) ->
    fetch_binary_packages(Repos, PackageList, To, ?FETCH_TIMEOUT).

%%-------------------------------------------------------------------
%% @spec fetch_release_package(Repos::list(), ErtsVsn::string(), Package::string(), Version::string(),
%%                             To::string(), Timeout::timeout()) -> ok | {error, Reason}
%% @doc
%%  Fetch the single release package (Package and Version) into the
%%  location specified (To) from the repositories (Repos).
%%
%%  ``ewr_fetch:fetch_release_package(["http://urltorepo/stable"],
%%                                   "5.5.5",
%%                                   "mypackage",
%%                                   "1.2.3",
%%                                   "./target",
%%                                   10000).''
%% @end
%%-------------------------------------------------------------------
fetch_release_package(Repos, ErtsVsn, Package, Version, To, Timeout) ->
    SysInfo = ewr_util:system_info(),
    fetch_package(Repos, ErtsVsn, Package, Version, To, [SysInfo, "Generic"], releases, Timeout).

%% @spec fetch_release_package(Repos::list(), ErtsVsn::string(), Package::string(), Version::string(), To::string()) ->
%%                             ok | {error, Reason}
%% @equiv fetch_release_package(Repos, ErtsVsn, Package, Version, To, Timeout)
fetch_release_package(Repos, ErtsVsn, Package, Version, To) ->
    fetch_release_package(Repos, ErtsVsn, Package, Version, To, ?FETCH_TIMEOUT).

%% @spec fetch_release_package(Repos::list(), Package::string(), Version::string(), To::string()) -> ok | {error, Reason}
%% @equiv fetch_release_package(Repos, DefaultErtsVsn, Package, Version, To)
fetch_release_package(Repos, Package, Version, To) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_release_package(Repos, ErtsVsn, Package, Version, To).

%%-------------------------------------------------------------------
%% @spec fetch_release_packages(Repos::list(), ErtsVsn::string(), PackageList::list(), To::string(), Timeout::timeout()) ->
%%                              ok | {error, Reason}
%% @doc
%%  Fetch all packages in the list (PackageList {Name, Version}), into
%%  the location (To) from the repositories (Repos).
%%
%%  ``ewr_fetch:fetch_release_packages(["http://urltorepo/stable"],
%%                                   [{"mypackage", "1.2.3"},
%%                                    {"another", "0.1.2"}],
%%                                   "./target",
%%                                   10000).''
%% @end
%%-------------------------------------------------------------------
fetch_release_packages(Repos, ErtsVsn, [{Name, Version}|T], To, Timeout) ->
    fetch_release_package(Repos, ErtsVsn, Name, Version, To, Timeout),
    fetch_release_packages(Repos, ErtsVsn, T, To, Timeout);
fetch_release_packages(_Repos, _ErtsVsn, [], _To, _Timeout) ->
    ok.

%% @spec fetch_release_packages(Repos::list(), ErtsVsn::string(), PackageList::list(), To::string()) -> ok | {error, Reason}
%% @equiv fetch_release_packages(Repos, ErtsVsn, PackageList, To, Reason, Timeout)
fetch_release_packages(Repos, ErtsVsn, PackageList, To) ->
    fetch_release_packages(Repos, ErtsVsn, PackageList, To, ?FETCH_TIMEOUT).

%% @spec fetch_release_packages(Repos::list(), PackageList::list(), To::string()) -> ok | {error, Reason}
%% @equiv fetch_release_packages(Repos, ErtsVsn, PackageList, To)
fetch_release_packages(Repos, PackageList, To) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_release_packages(Repos, ErtsVsn, PackageList, To).


%%-------------------------------------------------------------------
%% @doc
%%   Fetch the specified package from the repos looking into the
%%   areas supplied by Areas.
%%
%% @spec fetch_erts_package(Repos, Version, To, Timeout) -> ok | {error, Reason}
%% where
%%  Repos = [string()]
%%  Version = string()
%%  To = string()
%%  Timeout = timeout()
%% @end
%%-------------------------------------------------------------------
fetch_erts_package(Repos, Version, To, Timeout) ->
    case is_package_local(To, "erts", Version) of
        false ->
            filelib:ensure_dir(filename:join([To, "tmp"])),
	    SysInfo = ewr_util:system_info(),
	    Areas = ewr_util:create_system_info_series(SysInfo),
	    ErtsAreas = lists:map(
			  fun(ErtsArea) ->
				  filename:dirname(ewr_repo_paths:erts_package_suffix(Version, ErtsArea))
			  end,
			  Areas),
            fetch_from_repos(Repos, ErtsAreas, "/erts.tar.gz", To, Timeout);
        true ->
            ok
    end.

%% @spec fetch_erts_package(Repos, Version, To) -> ok | {error, Reason}
%% @equiv fetch_erts_package(Repos, Version, To, Timeout)
fetch_erts_package(Repos, Version, To) ->
    fetch_erts_package(Repos, Version, To, ?FETCH_TIMEOUT).

%%-------------------------------------------------------------------
%% @spec fetch_package(Repos, ErtsVsn, Package, Version, To, Areas, Side::atom(), Timeout::timeout()) -> ok |
%%        {error, Reason}
%% @doc
%%   Fetch the specified package from the repos looking into the
%%   areas supplied by Areas.
%% @end
%%-------------------------------------------------------------------
fetch_package(Repos, ErtsVsn, Package, Version, To, Areas, Side, Timeout) when is_atom(Package) ->
    fetch_package(Repos, ErtsVsn, atom_to_list(Package), Version, To, Areas, Side, Timeout);
fetch_package(Repos, ErtsVsn, Package, Version, To, Areas, Side, Timeout) ->
    error_logger:info_msg("ewr_fetch:fetch_package(~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p)~n",
			  [Repos, ErtsVsn, Package, Version, To, Areas, Side, Timeout]),
    case is_package_local(To, Package, Version) of
        false ->
            filelib:ensure_dir(filename:join([To, "tmp"])),
            TarName = lists:flatten([Package, ".tar.gz"]),
            Pos = ewr_util:gen_repo_suffix(ErtsVsn, Package, Version, Areas, Side),
            fetch_from_repos(Repos, Pos, TarName, To, Timeout);
        true ->
            ok
    end.

%% @spec fetch_package(Repos, Package, Version, To, Areas, Side::atom(), Timeout::timeout()) -> ok | {error, Reason}
%% @equiv fetch_package(Repos, ErtsVsn, Package, Version, To, Areas, Side, Timeout)
fetch_package(Repos, Package, Version, To, Areas, Side, Timeout) ->
    ErtsVsn = ewr_util:erts_version(),
    fetch_package(Repos, ErtsVsn, Package, Version, To, Areas, Side, Timeout).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec is_package_local(To, Package, Version) -> true | false
%%
%% @doc
%%  Check to see if the package exists.
%% @end
%%--------------------------------------------------------------------
is_package_local(To, Package, Version) ->
    File = filename:join([To, lists:flatten([Package, "-", Version])]),
    Src = filename:join([To, Package]),
    case {filelib:is_dir(Src), filelib:is_dir(File)} of
        {false, false} ->
            false;
        _ ->
            true
    end.

%%-------------------------------------------------------------------
%% @spec fetch_from_repos(Repos, PossibleLocations, TarName, To, Timeout) -> ok
%% @doc
%%  Actually try to fetch the TarName tar file from the specified
%%  repository.
%% @end
%% @private
%%-------------------------------------------------------------------
fetch_from_repos([Repo | Rest], PosLocations, TarName, To, Timeout) ->
    error_logger:info_msg("fetching from the following pos locations ~p~n", [PosLocations]),
    case catch fetch_from_repo(Repo, PosLocations, TarName, To, Timeout) of
        ok ->
            ok;
        Error ->
	    error_logger:info_msg("fetch_from_repos/5 fetch failed with ~p trying again at ~p~n", [Error, Rest]),
            fetch_from_repos(Rest, PosLocations, TarName, To, Timeout)
    end;
fetch_from_repos([], _PosLocations, TarName, _To, _Timeout) ->
    throw({unable_to_pull_from_repos,
           lists:flatten(["Couldn't get ", TarName, " from repos"])}).


fetch_from_repo(Repo, [Uri | T], TarName, To, Timeout) ->
    Suffix = lists:flatten([Uri, TarName]),
    case get_package(Repo, Suffix, To, Timeout) of
        ok ->
            ok;
        _Other ->
            fetch_from_repo(Repo, T, TarName, To, Timeout)
    end;
fetch_from_repo(_Repo, [], TarName, _To, _Timeout) ->
    throw({not_available, TarName ++ " not available in repository"}).



%%-------------------------------------------------------------------
%% @spec get_package(Repo, Suffix, To, Timeout) -> bad_repo | ok
%%  where
%%   Timeout = Milliseconds::integer() | infinity
%% @doc
%%  Make the http request and pass the result to handle_result.
%% @end
%% @private
%%-------------------------------------------------------------------
get_package(Repo, Suffix, To, Timeout) ->
    ActualTo = filename:join([To, "tmp.tar.gz"]),
    case ewr_repo_dav:repo_get(Repo, Suffix, Timeout) of
        {ok, Data} ->
            write_data(Data, ActualTo),
            handle_tar_file(To, ActualTo),
	    ok;
        Res = {error, _} ->
            Res
    end.

%%-------------------------------------------------------------------
%% @spec write_data(Data, Location) -> ok | {error, Reason}
%% @doc
%%  Write the data to the specified location.
%% @end
%% @private
%%-------------------------------------------------------------------
write_data(Data, To) ->
    case file:open(To, [write, raw]) of
        {ok, Fd} ->
	    error_logger:info_msg("ewr_fetch:write_data writing to ~p~n", [To]),
            ok = file:write(Fd, Data),
            file:close(Fd),
            ok;
        {error, Reason} ->
            throw({file_open_error, Reason})
    end.

%%-------------------------------------------------------------------
%% @spec handle_tar_file(To, ActualTo) -> ok
%% @doc
%%  Put the tar file in the correct location, the directory
%%  specified by To and the file by ActualTo
%% @end
%% @private
%%-------------------------------------------------------------------
handle_tar_file(To, ActualTo) ->
    error_logger:info_msg("ewr_fetch:handle_tar_file writing to ~p -> ~p~n", [ActualTo, To]),
    try ewl_file:uncompress(ActualTo, To) of
	ok ->
	    ok
    catch 
        _Class:Exception ->
	    throw({error, {unable_to_untar, ActualTo}, Exception})
    end,
    file:delete(ewr_util:handle_cygwin_path(ActualTo)),
    ok.


