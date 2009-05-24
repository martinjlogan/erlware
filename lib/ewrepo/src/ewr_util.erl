%%%-------------------------------------------------------------------
%%% Copyright (c) 2007 Eric Merritt, Martin Logan
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
%%% @copyright 2007 Erlware
%%% @doc
%%%  Various common functions used throughout ewrepo
%%% @end
%%%-------------------------------------------------------------------
-module(ewr_util).

-include("eunit.hrl").

%% API
-export([
        repo_consult/3,
        system_info/0,
        create_system_info_series/1,
        erts_version/0,
        erts_version/1,
        gen_multi_erts_repo_suffix/5,
        gen_repo_suffix/2,
        gen_repo_suffix/3,
        gen_repo_suffix/4,
        gen_repo_suffix/5,
        gen_erts_suffix/1,
        gen_multi_erts_repo_stub_suffix/4,
        gen_multi_erts_repo_stub_suffix/3,
        gen_repo_stub_suffix/3,
        gen_repo_stub_suffix/4,
        consult_url/1,
        gen_metadata_stub_suffix/1,
        gen_metadata_suffix/2,
        gen_metadata_suffix/3,
        parse_version/1,
        merge_per_app_deps/2,
        join/2,
        version_to_string/1,
        app_name/1,
        fetch_local_appfile_key_values/2,
        handle_cygwin_path/1,
        is_version_greater/2,
        get_auth_options/1
    ]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Convert a version list in deps format to astring.
%%
%% @spec version_to_string(VList) -> Version
%% where
%%       VList = [integer()]
%%       Version = string()
%% @end
%%--------------------------------------------------------------------
version_to_string(Ver) ->
    join(lists:map(fun(Int) ->
                    integer_to_list(Int)
            end, Ver), ".").

%%-------------------------------------------------------------------
%% @doc 
%%  join a list of lists with a separator
%% @spec join(List, Sep) -> ok
%% @end
%%-------------------------------------------------------------------
join(List, Sep) ->
    lists:foldl(fun(A, "") ->
                A;
            (A, Acc) -> Acc ++ Sep ++ A
        end, "", List).

%%-------------------------------------------------------------------
%% @doc Given a system info string. Take the final version number
%%      down to 0 in the form of a series.
%% Example: myos1.2 = ["myos1.2", "myos1.1", "myos1.0"]
%% @spec create_system_info_series(ArchString) -> string() | exit()  
%% @end
%%-------------------------------------------------------------------
create_system_info_series(ArchString) ->
    try
        {ok, {[MinorVersionStringRev], Rest}} = ewl_string_manip:n_tokens(lists:reverse(ArchString), 1, "."),
        MinorVersionString = lists:reverse(MinorVersionStringRev),
        ArchStringPart = lists:reverse(Rest),
        MinorVersions = lists:reverse(lists:seq(0, list_to_integer(MinorVersionString))),
        lists:map(fun(MinorVersion) ->
                    lists:flatten([ArchStringPart, ".", integer_to_list(MinorVersion)])
            end,
            MinorVersions)
    catch
        _C:_E ->
            [ArchString]
    end.

%%-------------------------------------------------------------------
%% @doc Fetch the version of the underlying OS.
%% @spec system_info() -> string() | exit()  
%% @end
%%-------------------------------------------------------------------
system_info() ->
    case catch chop_sys_info(erlang:system_info(system_architecture)) of
        {'EXIT', Reason} ->
            error_logger:info_msg("sys_info threw an error ~p~n", [Reason]),
            throw({invalid_version, "Unable to get system architecture," ++
                    "this may be a pre R9 runtime. Erlang versions pre R9 are not supported"});
        SystemName ->
            case is_posix(SystemName) of
                true ->
                    string:strip(lists:flatten([SystemName, "-glibc-", glibc_version()]), right, $\n);
                false ->
                    actual_kernel_version(SystemName)
            end
    end.

actual_kernel_version(SystemName) ->
    case os:find_executable(uname) of
        false ->
            SystemName;
        Uname ->
            OSVsn = string:strip(os:cmd(Uname ++ " -r"), right, $\n),
            case regexp:sub(SystemName, "[0-9\.]+$", "") of
                {ok,SystemNameSansVsn,1} ->
                    chop_sys_info(SystemNameSansVsn ++ OSVsn);
                Error ->
                    error_logger:info_msg("ewr_util:actual_kernel_version error in system name parsing ~p~n", [Error]),
                    SystemName
            end
    end.



glibc_version() ->
    case erlang:system_info(allocator) of
        {glibc, [Major, Minor|_], _, _} -> lists:flatten([integer_to_list(Major), ".", integer_to_list(Minor)]);
        _Error                        -> throw({no_glibc, "No version of glibc found"})
    end.

%% If the system_architeture string has a version number appended to the back of it, as in i386-apple-darwin8.8.1, make sure
%% to strip it down to major minor as in i386-apple-darwin8.8
chop_sys_info(Arch) ->
    case regexp:match(Arch, "([0-9]+\\.)+[0-9]+") of
        {match, Start, Length} ->
            VersionString = string:substr(Arch, Start, Length),
            Base          = string:substr(Arch, 1, Start - 1),
            lists:flatten([Base, version(minor, VersionString)]);
        _NoMatch ->
            Arch
    end.

%%--------------------------------------------------------------------
%% @doc Returns the erts version
%% @spec erts_version(Magnitude) -> string()
%% where
%%  Magnitude = major | minor | patch
%% @end
%%--------------------------------------------------------------------
erts_version(Magnitude) -> version(Magnitude, erlang:system_info(version)).

version(patch, VersionString) ->
    VersionString;
version(minor, VersionString) ->
    [Major, Minor|_] = string:tokens(VersionString, ".-"),
    lists:flatten([Major, ".", Minor]);
version(major, VersionString) ->
    hd(string:tokens(VersionString, ".-")).

%% @equiv erts_version(minor)
%% @spec erts_version() -> string()
erts_version() ->
    erts_version(patch).




%%-------------------------------------------------------------------
%% @deprecated
%% @doc
%%   Given the package, areas attempt to generate a repo_suffix() for each area that does not go all the way to the package but
%%   stops in the package dir containg the package version information. Do this for all service/patch erts 
%%   versions below the target  erts version. 
%% @spec gen_multi_erts_repo_stub_suffix(TargetErtsVsn, Package, Areas, Side) -> GeneratedUrls::list()
%%  where
%%   TargetErtsVsn = string()
%% @end
%%-------------------------------------------------------------------
gen_multi_erts_repo_stub_suffix(TargetErtsVsn, Package, Areas, Side) -> 
    [MajorErtsVsn, MinorErtsVsn, HighPatchErtsVsn] = string:tokens(TargetErtsVsn, "."),
    ErtsVsns = lists:map(fun(PatchVsn) when PatchVsn > 0 ->
                lists:flatten([MajorErtsVsn, ".", MinorErtsVsn, ".", integer_to_list(PatchVsn)]);
            (0) ->
                lists:flatten([MajorErtsVsn, ".", MinorErtsVsn])
        end,
        lists:seq(0, list_to_integer(HighPatchErtsVsn))),
    lists:foldr(fun(ErtsVsn, Acc) -> Acc ++ gen_repo_stub_suffix(ErtsVsn, Package, Areas, Side) end, [], ErtsVsns).

%% @spec gen_multi_erts_repo_stub_suffix(Package, Areas, Side) -> GeneratedUrls::list()
%% @equiv gen_multi_erts_repo_stub_suffix(TargetErtsVsn, Package, Areas, Side)
gen_multi_erts_repo_stub_suffix(Package, Areas, Side) -> 
    TargetErtsVsn = erts_version(),
    gen_multi_erts_repo_stub_suffix(TargetErtsVsn, Package, Areas, Side). 


%%-------------------------------------------------------------------
%% @deprecated
%% @doc
%%   Given the package, areas attempt to generate a repo_suffix() for each area that does not go all the way to the package but
%%   stops in the package dir containg the package version information.
%% @spec gen_repo_stub_suffix(ErtsVsn, Package, Areas, Side) -> GeneratedUrls::list()
%%  where
%%   ErtsVsn = string()
%% @end
%%-------------------------------------------------------------------
gen_repo_stub_suffix(ErtsVsn, Package, Areas, Side) when Side == lib; Side == releases ->
lists:map(fun(Area) -> lists:flatten(["/", ErtsVsn, "/", Area, "/", atom_to_list(Side), "/", 
                    Package, "/"]) end, Areas);
gen_repo_stub_suffix(ErtsVsn, Package, Areas, Side) when Side == none ->
    lists:map(fun(Area) -> lists:flatten(["/", ErtsVsn, "/", Area, "/", 
                        Package, "/"]) end, Areas).

%% @spec gen_repo_stub_suffix(Package, Areas, Side) -> GeneratedUrls::list()
%% @equiv gen_repo_stub_suffix(ErtsVsn, Package, Areas, Side)
gen_repo_stub_suffix(Package, Areas, Side) ->
    ErtsVsn = erts_version(),
    gen_repo_stub_suffix(ErtsVsn, Package, Areas, Side).


%%-------------------------------------------------------------------
%% @deprecated
%% @doc
%%   Given the package, areas attempt to generate a repo_suffix() for each area.Do this for all service/patch erts 
%%   versions below the target erts version. 
%% @spec gen_multi_erts_repo_suffix(TargetErtsVsn, Package, Version, Areas, Side) -> GeneratedUrls::list()
%%  where
%%   TargetErtsVsn = string()
%% @end
%%-------------------------------------------------------------------
gen_multi_erts_repo_suffix(TargetErtsVsn, Package, Version, Areas, Side) -> 
    [MajorErtsVsn, MinorErtsVsn, HighPatchErtsVsn] = string:tokens(TargetErtsVsn, "."),
    ErtsVsns = [lists:flatten([MajorErtsVsn, ".", MinorErtsVsn, ".", integer_to_list(E)]) || 
        E <- lists:seq(0, list_to_integer(HighPatchErtsVsn))], 
    lists:foldr(fun(ErtsVsn, Acc) -> Acc ++ gen_repo_suffix(ErtsVsn, Package, Version, Areas, Side) end, [], ErtsVsns).


%%-------------------------------------------------------------------
%% @deprecated
%% @doc
%%   Given the package, areas attempt to generate a repo_suffix() for each area.
%% @spec gen_repo_suffix(ErtsVsn, Package, Version, Areas, Side) -> GeneratedUrls::list()
%% @end
%%-------------------------------------------------------------------
gen_repo_suffix(ErtsVsn, Package, Version, Areas, Side) when Side == lib; Side == releases ->
lists:map(fun(Area) -> lists:flatten(["/", ErtsVsn, "/", Area, "/", atom_to_list(Side), "/", 
                    Package, "/", Version, "/"]) end, Areas);
gen_repo_suffix(ErtsVsn, Package, Version, Areas, Side) when Side == none ->
    lists:map(fun(Area) -> lists:flatten(["/", ErtsVsn, "/", Area, "/", 
                        Package, "/", Version, "/"]) end, Areas).

%% @equiv gen_repo_suffix(ErtsVsn, Package, Version, Areas, lib)
%% @spec gen_repo_suffix(Package, Version, Areas, Side) -> GeneratedUrls::list()
gen_repo_suffix(Package, Version, Areas, Side) ->
    ErtsVsn = erts_version(),
    gen_repo_suffix(ErtsVsn, Package, Version, Areas, Side).

%% @equiv gen_repo_suffix(Package, Version, Areas, lib)
%% @spec gen_repo_suffix(Package, Version, Areas) -> GeneratedUrls::list()
gen_repo_suffix(Package, Version, Areas) ->
    gen_repo_suffix(Package, Version, Areas, lib).

%% XXX This is messed up this should be gen_repo_stub_suffix - do analysis and change this
%% @equiv gen_repo_stub_suffix(Package, Areas, lib)
%% @spec gen_repo_suffix(Package, Areas) -> GeneratedUrls::list()
gen_repo_suffix(Package, Areas) ->
    gen_repo_stub_suffix(Package, Areas, lib).

%%--------------------------------------------------------------------
%% @deprecated
%% @doc 
%%   Given the package, areas attempt to generate a metadata suffix that does not go all the way to the package but
%%   stops in the package dir containg the package version information.
%% @spec gen_metadata_stub_suffix(Package) -> Suffix
%% @end
%%--------------------------------------------------------------------
gen_metadata_stub_suffix(Package) when is_atom(Package) ->
    gen_metadata_stub_suffix(atom_to_list(Package));
gen_metadata_stub_suffix(Package) when is_list(Package) ->
    gen_repo_stub_suffix(Package, ["Meta"], none).

%%--------------------------------------------------------------------
%% @deprecated
%% @doc 
%%  Get the suffix for erts from the system.
%% @spec gen_erts_suffix(ErtsVsn::list()) -> Suffix::list()
%% @end
%%--------------------------------------------------------------------
gen_erts_suffix(ErtsVsn) when is_list(ErtsVsn) ->
    lists:flatten(["/", ErtsVsn, "/", system_info(), "/"]).

%%--------------------------------------------------------------------
%% @deprecated
%% @doc 
%%  Get the suffix for the metadata from the system.
%% @spec gen_metadata_suffix(ErtsVsn, Package, Version) -> Suffix
%% @end
%%--------------------------------------------------------------------
gen_metadata_suffix(ErtsVsn, Package, Version) when is_atom(Package), is_integer(Version) ->
    gen_metadata_suffix(ErtsVsn, atom_to_list(Package), integer_to_list(Version));
gen_metadata_suffix(ErtsVsn, Package, Version) when is_list(Package), is_list(Version) ->
    gen_repo_suffix(ErtsVsn, Package, Version, ["Meta"], none).

%% @equiv gen_metadata_suffix(ErtsVsn, Package, Version)
%% @spec gen_metadata_suffix(Package, Version) -> Suffix
gen_metadata_suffix(Package, Version) ->
    ErtsVsn = erts_version(),
    gen_metadata_suffix(ErtsVsn, Package, Version).


%%--------------------------------------------------------------------
%% @deprecated use repo_consult/3
%% @doc 
%%  Given a url that points to valid erlang terms, parse the resulting
%%  terms and return those terms to the user.
%% 
%% @spec consult_url(Url) -> {ok, Terms} | Error
%% @end
%%--------------------------------------------------------------------
consult_url(Url) ->
    ibrowse:start(),
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, Body} ->
            parse_consult_url_result(Body);
        Else ->
            {error, Else}
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  Given a repo and a suffix that points to valid erlang terms, parse the resulting
%%  terms and return those terms to the user.
%% 
%% @spec repo_consult(Repo, Suffix, Timeout) -> {ok, Terms} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
repo_consult(Repo, Suffix, Timeout) ->
    case ewr_repo_dav:repo_get(Repo, Suffix, Timeout) of
        {ok, Data} ->
            parse_consult_url_result(Data);
        {error, Reason} ->
            {error, Reason}
    end.

%%-------------------------------------------------------------------- 
%% @doc 
%%   Parse the version into a usable format.
%% @spec parse_version(V) -> versionList
%% @end
%%--------------------------------------------------------------------
parse_version(Ver) ->
    parse_version(Ver, [], []).

%%--------------------------------------------------------------------
%% @doc 
%%  Merge the versioned and unversioned parts of a *.app file so that
%%  a single set of dependencies emerge.
%% @spec merge_per_app_deps(Apps, VApps) -> NewDeps
%% @end
%% @private
%%--------------------------------------------------------------------
merge_per_app_deps(Apps, VApps) ->
    merge_per_app_deps(Apps, VApps, []).

%%--------------------------------------------------------------------
%% @doc Returns appfile information about a particular application.
%% <pre>
%% Example: fetch_local_app_vsn("/home/martin/work/otp/lib/gas", [vsn]) 
%%   returns {ok, [{vsn, "4.1.2"}]}
%% </pre>
%% @spec fetch_local_appfile_key_values(AppDir, AppFileKeys) -> {ok, KeysAndValues} | {error, Reason}
%% @end 
%%--------------------------------------------------------------------
fetch_local_appfile_key_values(AppDir, AppFileKeys) ->
    case file:consult(app_file(AppDir)) of
        {error, Reason} ->
            {error, {"App file problem for " ++ app_file(AppDir), Reason}};
        {ok, [AppTerm]} ->
            extract_app_keys_and_values_from_app_term(AppTerm, AppFileKeys)
    end.

%%--------------------------------------------------------------------
%% @doc Returns the appname for a given app_dir()|versioned_app_dir()
%% @spec app_name(AppDir::app_dir()) -> string()
%% @end
%%--------------------------------------------------------------------
app_name(AppDir) ->
    case regexp:match(AppDir, ".*-[0-9].*") of
        {match, _, _} -> 
            [_Vsn, AppNameString|_] = lists:reverse(string:tokens(AppDir, "/-")),
            AppNameString;
        nomatch ->
            filename:basename(AppDir)
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  In the case that erlang is installed via the windows binary but an OS cmd is executed via cygwin we have to resolve the 
%%  fact that / for erts is going to be a volume root like c:/ and for cygwin in that case it will be cygdrive/c. 
%% <pre>
%% Example if running from cygwin:
%%  handle_cygwin_path("/usr/local") -> "/cygdrive/c/usr/local" 
%%  handle_cygwin_path("/usr/bin/blah.sh") -> "/cygdrive/c/usr/bin/blah.sh" 
%%
%% Example if not running from cygwin:
%%  handle_cygwin_path("/usr/local") -> "/usr/local" 
%%  handle_cygwin_path("/usr/bin/blah.sh") -> "/usr/bin/blah.sh" 
%% </pre>
%%
%% @spec handle_cygwin_path(Target) -> NewTarget
%% @end
%%--------------------------------------------------------------------
handle_cygwin_path(Target) ->
    case erlang:system_info(system_architecture) of
        "win32" ->
            case catch os:cmd("uname") of
                [$C,$Y,$G,$W,$I,$N|_] ->
                    case filelib:is_dir(Target) of
                        true ->
                            handle_cygwin_dir(Target);
                        false ->
                            CygDir = handle_cygwin_dir(filename:dirname(Target)),
                            lists:flatten([CygDir, "/", filename:basename(Target)])
                    end;
                _ ->
                    Target
            end;
        _ ->
            Target
    end.

handle_cygwin_dir([$/,$c,$y,$g,$d,$r,$i,$v,$e,$/,_|_] = Dir) ->
    Dir;
handle_cygwin_dir(Dir) ->
    {ok, CWD} = file:get_cwd(),
    ok        = file:set_cwd(Dir),
    PWD       = os:cmd("pwd"),
    error_logger:info_msg("fax_install:cygdrive_dir_to_target cwd ~p pwd ~p~n", [CWD, PWD]),
    ok        = file:set_cwd(CWD),
    case regexp:match(PWD, "cygdrive") of
        {match, _, _} -> lists:flatten([string:substr(PWD, 1, 12), Dir]);
        _             -> Dir
    end.

get_auth_options(Repo) ->
    case file:consult(filename:join(home_dir(), ".faxien.secrets")) of
        {ok, Terms} ->
            lookup_url(Repo, Terms);
        {error, Error} ->
            error_logger:info_msg("Could not find auth options for repo~p (reason: ~p)~n", [Repo, Error]),
            []
    end.

%%====================================================================
%% Internal Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc Returns appfile information from an app term.
%% @spec extract_app_keys_and_values_from_app_term(AppTerm, AppKeys) -> {ok, KeysAndValues} | {error, Reason}
%% @end 
%%--------------------------------------------------------------------
extract_app_keys_and_values_from_app_term({application, _AppName, 
        KeyValues}, AppKeys) ->
    KeysAndValues = lists:foldl(fun(Key, Acc) -> 
                case lists:keysearch(Key, 1, KeyValues) of
                    {value, Value} ->
                        [Value|Acc];
                    false ->
                        Acc
                end 
        end, [], AppKeys),
    {ok, KeysAndValues};
extract_app_keys_and_values_from_app_term(BadTerm, _AppKeys) ->
    {error, {bad_app_file_term, BadTerm}}.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns a path to a .app file for a given app_dir()|versioned_app_dir()
%% @spec app_file(AppDir::app_dir()) -> string()
%% @end
%%--------------------------------------------------------------------
app_file(AppDir) ->
    AppDir ++ "/ebin/" ++ app_name(AppDir) ++ ".app".


%%--------------------------------------------------------------------
%% @doc 
%%  Parse the version into a list of numbers.
%% @spec parse_version(List, LocalAcc, NumAcc) -> VersionAsList
%% @end
%%--------------------------------------------------------------------
parse_version([$. | T], LAcc, Acc) ->
    Number = list_to_integer(lists:reverse(LAcc)),
    parse_version(T, [], [Number | Acc]);
parse_version([H | T], LAcc, Acc) ->
    parse_version(T, [H | LAcc], Acc);
parse_version([], LAcc, Acc) ->
    Number = list_to_integer(lists:reverse(LAcc)),
    lists:reverse([Number | Acc]).

%%--------------------------------------------------------------------
%% @doc 
%%  Parse the result of a consult_url request.
%%
%% @spec parse_consult_url_result(RawBody) -> ErlangTerm | {error, Reason}
%% @end
%%--------------------------------------------------------------------
parse_consult_url_result(RawBody) ->
    case erl_scan:string(RawBody) of
        {ok, Body, _} ->
            case erl_parse:parse_term(Body) of 
                Res = {ok, _} ->
                    Res;
                Error = {error, _} ->
                    Error
            end;
        Error = {error, _} ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc 
%%  Test the name to see if this is a posix system or not. Right
%%  now only linux is tested. 
%% @spec is_posix(Name) -> true | false
%% @end
%%--------------------------------------------------------------------
is_posix([$l, $i, $n, $u, $x | _T]) ->
    true;
is_posix([_ | T]) ->
    is_posix(T);
is_posix([]) ->
    false.


%%--------------------------------------------------------------------
%% @doc 
%%  Merge the versioned and unversioned parts of a *.app file so that
%%  a single set of dependencies emerge.
%% @spec merge_per_app_deps(Apps, VApps, Acc) -> NewDeps
%% @end
%% @private
%%--------------------------------------------------------------------
merge_per_app_deps(Apps, undefined, _Acc) ->
    Apps;
merge_per_app_deps([H | T], VersionedApps, Acc) ->
    case in_versioned(H, VersionedApps) of
        true ->
            merge_per_app_deps(T, VersionedApps, Acc);
        false ->
            merge_per_app_deps(T, VersionedApps, [H | Acc])
    end;
merge_per_app_deps([], VersionedApps, Acc) ->
    VersionedApps ++ Acc.


%%--------------------------------------------------------------------
%% @doc 
%%  Check to see if the app is in the list of versioned apps.
%% @spec in_versioned(App, AppList) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
in_versioned(App, [{App, _} | _]) ->
    true;
in_versioned(App, [{App, _, _} | _]) ->
    true;
in_versioned(App, [_ | T]) ->
    in_versioned(App, T);
in_versioned(_App, []) ->
    false.



%%--------------------------------------------------------------------
%% @doc Is version string A bigger than version string B?
%% <pre>
%% Example: is_version_greater("3-2-5-alpha", "3.10.6") will return false
%%          is_version_greater("3-2-alpha", "3.2.1-alpha") will return false
%% </pre>
%% @spec is_version_greater(VsnStringA::version(), VsnStringB::version()) -> bool()
%% @end
%%--------------------------------------------------------------------
is_version_greater(VsnStringA, VsnStringB) ->
    compare(string:tokens(VsnStringA, ".-"),string:tokens(VsnStringB, ".-")).

compare([StrDig|TA], [StrDig|TB]) -> 
    compare(TA, TB);
compare([StrDigA|_], [StrDigB|_]) -> 
    case {to_int(StrDigA), to_int(StrDigB)} of
        {Integer, String}    when is_integer(Integer),  is_list(String)      -> true;
        {IntegerA, IntegerB} when is_integer(IntegerA), is_integer(IntegerB) -> IntegerA > IntegerB;
        {String, Integer}    when is_integer(Integer),  is_list(String)      -> false;
        {StringA, StringB}   when is_list(StringA),     is_list(StringB)     -> StringA > StringB
    end;
compare([], [_|_]) -> false;
compare([_|_], []) -> true;
compare([], [])    -> false.

to_int(String) ->
    case catch list_to_integer(String) of
        Integer when is_integer(Integer) -> Integer;
        _                                -> String
    end.

%%====================================================================
%% tests
%%====================================================================

create_system_info_series_test() ->
    ?assertMatch(["a-1.3", "a-1.2", "a-1.1", "a-1.0"], create_system_info_series("a-1.3")),
    ?assertMatch(["myos1.10", "myos1.9", "myos1.8", "myos1.7",
            "myos1.6", "myos1.5", "myos1.4", "myos1.3",
            "myos1.2", "myos1.1", "myos1.0"], create_system_info_series("myos1.10")).

parse_consult_url_test() ->
    Test1 = "{hello, goodbye}.",
    Test2 = "[[0, 1, 2], [3, 4, 5, 6]].",
    ?assertMatch({ok, {hello, goodbye}}, 
        parse_consult_url_result(Test1)),
    ?assertMatch({ok, [[0, 1, 2], [3, 4, 5, 6]]},
        parse_consult_url_result(Test2)).

parse_version_test() ->
    ?assertMatch([2, 3, 2], parse_version("2.3.2", [], [])),
    ?assertMatch([2, 22, 55], parse_version("2.22.55", [], [])),
    ?assertMatch([3123, 3221, 1123, 1213], parse_version("3123.3221.1123.1213", [], [])),
    ?assertMatch([2], parse_version("2", [], [])).

is_version_greater_test() ->
    ?assertMatch(true, is_version_greater("0.24.0.1", "0.22.1.1")),
    ?assertMatch(true, is_version_greater("3-2-5-alpha", "3.1.6")),
    ?assertMatch(false, is_version_greater("3-2-5-alpha", "3.2.5-beta")),
    ?assertMatch(false, is_version_greater("3-2-alpha", "3.2.1-alpha")).

chop_sys_info_test() ->
    ?assertMatch("i386_darwin8.91", chop_sys_info("i386_darwin8.91.1")),
    ?assertMatch("i386_darwin1.10", chop_sys_info("i386_darwin1.10")),
    ?assertMatch("i386_darwin8.91", chop_sys_info("i386_darwin8.91.1.5")).

is_posix_test() ->
    ?assertMatch(true, is_posix("i686-pc-linux-gnu")).

lookup_url(URL, TermList) ->
    case proplists:get_value(faxien_secrets, TermList) of
        PropList when is_list(PropList) ->
            get_auth_for_url(URL, PropList);
        undefined ->
            error_logger:info_msg("Missing faxien_secrets in secrets file for repo ~p~n", [URL]),
            [];
        _Other ->
            error_logger:info_msg("Wrong format for faxien_secrets in secrets file for repo ~p: ~p~n", [URL, _Other]),
            []
    end.

get_auth_for_url(URL, PropList) ->
    case [Tuple || {K, _} = Tuple <- PropList, lists:prefix(K, URL)] of
        [] = L ->
            error_logger:info_msg("No auth options for repo ~p~n", [URL]),
            L;
        [{_K, AuthOpts}] ->
            check_ssl(AuthOpts),
            AuthOpts;
        [{_K, AuthOpts}|_] ->
            error_logger:info_msg("More than one matching auth option for repo ~p, first one used~n", [URL]),
            check_ssl(AuthOpts),
            AuthOpts
    end.

home_dir() ->
    case os:getenv("HOME") of
        undefined ->
            error_logger:info_msg("The HOME environment variable is not set~n"),
            "."; % Default to current dir
        Home ->
            Home
    end.

check_ssl(AuthOpts) when is_list(AuthOpts) ->
    case proplists:get_value(is_ssl, AuthOpts) of
        true ->
            start_ssl();
        _ ->
            ok
    end.

start_ssl() ->
    case application:start(ssl) of
        {error,{already_started,_}} ->
            ok;
        {error, Reason} = Error ->
            error_logger:info_msg("Failed to start ssl, error:~n~p~n", [Reason]),
            Error;
        ok ->
            ssl:seed(term_to_binary(make_ref())),
            ok
    end.

