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
%%% @author Eric Merritt
%%% @copyright 2006 Erlware
%%% @doc
%%%   Figures out the dependencies for the application, via teh 
%%%   ewr_deps_engine.
%%% @end
%%%-------------------------------------------------------------------
-module(ewr_depends).


-include("eunit.hrl").

%% API
-export([
         check_project_dependencies/3
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Resolves dependencies using the information provided by Urls. 
%% 
%% DepInformation must be in the form of [{App, Vsn, {BareDeps, VersionedDeps}}, ...].
%% Given that information it will return the correct list of 
%% dependencies.
%%
%% @spec check_project_dependencies(Urls, DepInformation, SupplimentalDeps) -> Deps
%% @end
%%--------------------------------------------------------------------
check_project_dependencies(Urls, DepInformation, SupplimentalDeps) ->
    {Transformed, Apps, VList} = 
        lists:foldl(fun preformat_version_data/2, {[], [], []}, DepInformation),
    ewr_resolver:start_link(Urls, Transformed, VList),
    Edps = ewr_deps_engine:init(fun ewr_resolver:package_versions/1, 
                               fun ewr_resolver:package_dependencies/2),
    case ewr_deps_engine:deps(Edps, SupplimentalDeps ++ Apps) of
        fail ->
            error_logger:warning_msg("Dependency conflict! failing build!~n "),
            ewr_resolver:shutdown(),
            throw(dependency_resolution_error);
        {ok, Res} ->
            ewr_resolver:shutdown(),
            lists:foldl(fun({App, Vsn}, Acc) ->
                                [{App, ewr_util:version_to_string(Vsn)} | Acc]
                        end, [], Res)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Parse the app description into a format consumable by the 
%%  deps engine.
%% @spec preformat_version_data(AppInfo, Acc) -> {Deps, Pkg, Vsns}
%% @end
%%--------------------------------------------------------------------
preformat_version_data({App, Vsn, {ADeps, VDeps}}, {DepList, AppList, VsnList}) ->
    NVsn = ewr_util:parse_version(Vsn),
    VKey = {App, [NVsn]},
    Key = {App, NVsn},
    NDeps = ewr_util:merge_per_app_deps(ADeps, parse_vdeps(VDeps, [])),
    {[{Key, NDeps} | DepList], [Key | AppList], [VKey | VsnList]}.

%%--------------------------------------------------------------------
%% @doc 
%%  Parse the version string in versioned dependencies into something
%%  usable by the system.
%% @spec parse_vdeps(undefined, Acc) -> Deps
%% @end
%%--------------------------------------------------------------------
parse_vdeps(undefined, Acc) ->
    Acc;
parse_vdeps([{App, Vsn} | T], Acc) ->
    parse_vdeps(T, [{App, ewr_util:parse_version(Vsn)} | Acc]);
parse_vdeps([{App, Vsn, Else} | T], Acc) ->
    parse_vdeps(T, [{App, ewr_util:parse_version(Vsn), Else} | Acc]);
parse_vdeps([], Acc) ->
    Acc.
%%====================================================================
%% Tests
%%====================================================================
