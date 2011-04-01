%%%-------------------------------------------------------------------
%%% @copyright Erlware 2007
%%% @author    Martin Logan <martinjlogan@erlware.org>
%%%
%%% @doc This module contains all functions in ewrepo that make requests to a remote repo. *Note* this file should not
%%%      contain any convenience functions or shortcuts.  Those should be placed in higher level modules so that this 
%%%      stays free of any clutter.
%%% @end
%%%
%%% Created :  1 Dec 2007 by Martin Logan <martinjlogan@erlware.org>
%%%-------------------------------------------------------------------
-module(ewr_repo_dav).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 repo_get/3,
	 repo_put/4,
	 repo_list/3,
	 repo_mkcol/3
        ]).

%%====================================================================
%% External functions
%%====================================================================

%%-------------------------------------------------------------------
%% @doc
%%  Make the http request to fetch some data.
%%
%% <pre>
%% Example Invocation:
%%  repo_get("http://repo.erlware.org/writeable", "/5.5.5/Generic/lib/mnesia/2.3/mnesia.tar.gz", 100000).
%%
%% Example Suffix Breakdown: 
%%          /5.5.5/Generic/lib/mnesia/2.3 
%%          ErtsVsn/Area/Side/PackageName/PackageVsn
%%
%% Example Repo Breakdown: 
%%          http://repo.erlware.org/pub
%%          URL/Section
%% </pre>
%%
%% @spec repo_get(Repo, Suffix, Timeout) -> {ok, Data} | {error, Reason}
%%  where
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%-------------------------------------------------------------------
repo_get([$f,$i,$l,$e,$:,$/,$/|Repo] = FullRepo, Suffix, Timeout) ->
    FilePath = ewl_file:join_paths(Repo, Suffix),
    file:read_file(FilePath);
repo_get([$h,$t,$t,$p,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOpts = [],
    repo_get_with_auth(Repo, Suffix, Timeout, AuthOpts);
repo_get([$h,$t,$t,$p,$s,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOpts = ewr_util:get_auth_options(Repo),
    repo_get_with_auth(Repo, Suffix, Timeout, AuthOpts).

%%-------------------------------------------------------------------
%% @doc
%%  Put bits onto a filesystem.  This function creates the directory strcuture speficied if it does not exist.
%%
%% <pre>
%% Example Invocation:
%%  repo_put("http://repo.erlware.org/writeable", "/5.5.5/Generic/lib/mnesia/2.3/mnesia.tar.gz", Binary, 100000).
%%
%% Example Suffix Breakdown: 
%%          /5.5.5/Generic/lib/mnesia/2.3 
%%          ErtsVsn/Area/Side/PackageName/PackageVsn
%%
%% Example Repo Breakdown: 
%%          http://repo.erlware.org/pub
%%          URL/Section
%%
%% </pre>
%%
%% @spec repo_put(Repo::string(), Suffix::string(), Payload::binary(), Timeout) -> 
%%               {ok, URL} | {error, Reason}
%%  where
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%-------------------------------------------------------------------
repo_put([$f,$i,$l,$e,$:,$/,$/|Repo] = FullRepo, Suffix, Payload, _Timeout) ->
    FilePath = ewl_file:join_paths(Repo, Suffix),
    ewl_file:mkdir_p(filename:dirname(FilePath)),
    case file:write_file(FilePath, Payload) of
	ok    -> {ok, FullRepo};
	Error -> Error
    end;
repo_put([$h,$t,$t,$p,$:,$/,$/|_] = Repo, Suffix, Payload, Timeout) ->
    AuthOpts = [],
    repo_put_with_auth(Repo, Suffix, Payload, Timeout, AuthOpts);
repo_put([$h,$t,$t,$p,$s,$:,$/,$/|_] = Repo, Suffix, Payload, Timeout) ->
    AuthOpts = ewr_util:get_auth_options(Repo),
    repo_put_with_auth(Repo, Suffix, Payload, Timeout, AuthOpts).

%%-------------------------------------------------------------------
%% @doc
%% Individually creates each collection required by the Path. Has the syntax of mkdir -p but over webdav.
%%
%% <pre>
%% Variables:
%%  Timeout - is an integer specified in milliseconds. *Note* that this timeout is per collection creation not for the total 
%%            time it takes to complete an invocation of this function.
%%
%% Example Suffix Breakdown: 
%%          /5.5.5/Generic/lib/mnesia/2.3 
%%          ErtsVsn/Area/Side/PackageName/PackageVsn
%%
%% Example Repo Breakdown: 
%%          http://repo.erlware.org/pub
%%          URL/Section
%%
%% </pre>
%%
%% @spec repo_mkcol(Repo, Suffix, Timeout) -> ok | {error, Error}
%%  where
%%   Timeout = Milliseconds::integer() | infinity
%% @end
%%-------------------------------------------------------------------
repo_mkcol([$f,$i,$l,$e,$:,$/,$/|Repo], Suffix, _Timeout) ->
    try ewl_file:mkdir_p(ewl_file:join_paths(Repo, Suffix)) 
    catch
	_C:E ->
	    {error, E}
    end;
repo_mkcol([$h,$t,$t,$p,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOpts = [],
    repo_mkcol_with_auth(Repo, Suffix, Timeout, AuthOpts);
repo_mkcol([$h,$t,$t,$p,$s,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOpts = ewr_util:get_auth_options(Repo),
    repo_mkcol_with_auth(Repo, Suffix, Timeout, AuthOpts).

%%-------------------------------------------------------------------
%% @doc
%% Return a the contents of a directory.
%% @end
%%-------------------------------------------------------------------
-spec repo_list(Url::string(), Suffix::string(), Timeout::non_neg_integer()) ->
    {ok, DirContents::list()} | {error, Reason::term()}.
repo_list([$f,$i,$l,$e,$:,$/,$/|Repo], Suffix, _Timeout) ->
    FullPath = filename:join([Repo, Suffix, "*"]), 
    try
	{ok, [filename:basename(E) || E <- filelib:wildcard(FullPath)]}
    catch
	_C:_E ->
	    {error,{repo_list, FullPath}}
    end;
repo_list([$h,$t,$t,$p,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    repo_list_with_auth(Repo, Suffix, Timeout, []);
repo_list([$h,$t,$t,$p,$s,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOpts = get_auth_options(Repo),
    repo_list_with_auth(Repo, Suffix, Timeout, AuthOpts).


%%%===================================================================
%%% Internal functions
%%%===================================================================
repo_list_with_auth(Repo, Suffix, Timeout, AuthOpts) ->
    Url = ewl_file:join_paths(Repo, Suffix), 
    Opts = [{"Connection", "TE"},
	    {"TE", "trailers"},
	    {"Depth", "1"},
	    {"Content-Type", "application/xml"}],
    case catch ibrowse:send_req(Url, Opts, propfind, "", AuthOpts, Timeout) of
        {ok, "207", _, Body} -> 
	    {ok, parse_out_package_versions(Body)};
	{ok, Code, _, _} -> 
	    {error, {"No list found. http code: ", Code}};
        {error, _Reason} = Res -> 
	    Res;
        {'EXIT', Reason} -> 
            {error, Reason}
    end.
    
parse_out_package_versions(Body) ->
    {Elem, _} = xmerl_scan:string(Body),
    [filename:basename(E) || E <- tl(lists:sort(xmerl_xs:value_of(xmerl_xs:select("//D:href", Elem))))].

handle_ibrowse_return(Result, AcceptableCodes) ->
    case Result of
	{ok, Code, _, Payload} -> 
	    case lists:member(Code, AcceptableCodes) of
		true -> 
		    {ok, Payload};
		false ->
		    {error, {http_return_code, Code}}
	    end;
        Error = {error, _} -> 
	    Error
    end.
    
repo_get_with_auth(Repo, Suffix, Timeout, AuthOpts) ->
    URL = ewl_file:join_paths(Repo, Suffix),
    Res = ibrowse:send_req(URL, [], get, [], AuthOpts, Timeout),
    handle_ibrowse_return(Res, ["200"]).

repo_put_with_auth(Repo, Suffix, Payload, Timeout, AuthOpts) ->
    %% Creates the directory structure within the repo.
    repo_mkcol(Repo, filename:dirname(Suffix), Timeout),
    URL = ewl_file:join_paths(Repo, Suffix),
    Res = (catch ibrowse:send_req(URL, [], put, Payload, AuthOpts, Timeout)),
    case handle_ibrowse_return(Res, ["200", "201"]) of
        {ok, _} -> {ok, URL};
        Error   -> Error
    end.

repo_mkcol_with_auth(Repo, Suffix, Timeout, AuthOpts) ->
    (catch lists:foldl(fun(PathElement, Acc) -> 
                    NewAcc = Acc ++ PathElement ++ "/",
                    URL    = ewl_file:join_paths(Repo, NewAcc),
                    %% In place for the build in logging
                    handle_ibrowse_return(
                        ibrowse:send_req(URL, [], mkcol, [], AuthOpts, Timeout),
                        ["200", "201"]),
                    NewAcc
            end, [], string:tokens(Suffix, "/"))),
    ok.

get_auth_options(Repo) ->
    case file:consult(auth_file()) of
        {ok, Terms} ->
            lookup_url(Repo, Terms);
        {error, Error} ->
            []
    end.

auth_file() ->
    FaxAuth = filename:join(home_dir(), ".faxien.auth"),
    case filelib:is_file(FaxAuth) of
	false ->
	    filename:join(home_dir(), ".erlang.auth");
	true ->
	    FaxAuth
    end.
	    
lookup_url(URL, TermList) ->
    case proplists:get_value(faxien_secrets, TermList) of
        PropList when is_list(PropList) ->
            get_auth_for_url(URL, PropList);
        undefined ->
            [];
        _Other ->
            []
    end.

get_auth_for_url(URL, PropList) ->
    case [Tuple || {K, _} = Tuple <- PropList, lists:prefix(K, URL)] of
        [] = L ->
            L;
        [{_K, AuthOpts}] ->
            check_ssl(AuthOpts),
            AuthOpts;
        [{_K, AuthOpts}|_] ->
            check_ssl(AuthOpts),
            AuthOpts
    end.

home_dir() ->
    case os:getenv("HOME") of
        undefined ->
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
            Error;
        ok ->
            ssl:seed(term_to_binary(make_ref())),
            ok
    end.

