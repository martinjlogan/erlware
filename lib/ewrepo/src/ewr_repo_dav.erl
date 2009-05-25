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
    error_logger:info_msg("ewr_repo_dav:repo_get(~p, ~p, ~p)~n", [FullRepo, Suffix, Timeout]),
    FilePath = ewl_file:join_paths(Repo, Suffix),
    file:read_file(FilePath);
repo_get([$h,$t,$t,$p,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOptions = [],
    repo_get_with_auth(Repo, Suffix, Timeout, AuthOptions);
repo_get([$h,$t,$t,$p,$s,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOptions = ewr_util:get_auth_options(Repo),
    repo_get_with_auth(Repo, Suffix, Timeout, AuthOptions).

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
    AuthOptions = [],
    repo_put_with_auth(Repo, Suffix, Payload, Timeout, AuthOptions);
repo_put([$h,$t,$t,$p,$s,$:,$/,$/|_] = Repo, Suffix, Payload, Timeout) ->
    AuthOptions = ewr_util:get_auth_options(Repo),
    repo_put_with_auth(Repo, Suffix, Payload, Timeout, AuthOptions).

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
    AuthOptions = [],
    repo_mkcol_with_auth(Repo, Suffix, Timeout, AuthOptions);
repo_mkcol([$h,$t,$t,$p,$s,$:,$/,$/|_] = Repo, Suffix, Timeout) ->
    AuthOptions = ewr_util:get_auth_options(Repo),
    repo_mkcol_with_auth(Repo, Suffix, Timeout, AuthOptions).

%%====================================================================
%% Internal functions
%%====================================================================

handle_ibrowse_return(Result, AcceptableCodes) ->
    case Result of
	{ok, Code, _, Payload} -> 
	    case lists:member(Code, AcceptableCodes) of
		true -> 
		    {ok, Payload};
		false ->
		    error_logger:info_msg("ewr_repo_dav:handle_ibrowse_return/2 -> ~p~n", [Result]),
		    {error, {http_return_code, Code}}
	    end;
        Error = {error, _} -> 
	    error_logger:info_msg("ewr_repo_dav:handle_ibrowse_return/2 -> ~p~n", [Result]),
	    Error
    end.
    
repo_get_with_auth(Repo, Suffix, Timeout, AuthOptions) ->
    error_logger:info_msg("ewr_repo_dav:repo_get(~p, ~p, ~p)~n", [Repo, Suffix, Timeout]),
    URL = ewl_file:join_paths(Repo, Suffix),
    Res = ibrowse:send_req(URL, [], get, [], AuthOptions, Timeout),
    handle_ibrowse_return(Res, ["200"]).

repo_put_with_auth(Repo, Suffix, Payload, Timeout, AuthOptions) ->
    %% Creates the directory structure within the repo.
    repo_mkcol(Repo, filename:dirname(Suffix), Timeout),
    URL = ewl_file:join_paths(Repo, Suffix),
    error_logger:info_msg("ewr_repo_dav:repo_put putting to ~p~n", [URL]),
    Res = (catch ibrowse:send_req(URL, [], put, Payload, AuthOptions, Timeout)),
    case handle_ibrowse_return(Res, ["200", "201"]) of
        {ok, _} -> {ok, URL};
        Error   -> Error
    end.

repo_mkcol_with_auth(Repo, Suffix, Timeout, AuthOptions) ->
    (catch lists:foldl(fun(PathElement, Acc) -> 
                    NewAcc = Acc ++ PathElement ++ "/",
                    URL    = ewl_file:join_paths(Repo, NewAcc),
                    error_logger:info_msg("mkcol  on ~p~n", [URL]),
                    %% In place for the build in logging
                    handle_ibrowse_return(
                        ibrowse:send_req(URL, [], mkcol, [], AuthOptions, Timeout),
                        ["200", "201"]),
                    NewAcc
            end, [], string:tokens(Suffix, "/"))),
    ok.

