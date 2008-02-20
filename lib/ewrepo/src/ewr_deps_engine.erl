%%%-------------------------------------------------------------------
%%% Copyright (c) 2006 Scott Parish
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
%%% @author Scott Parish
%%% @copyright 2007 
%%% @doc
%%%   Uses backtracing to find the actual deps required.
%%% @end
%%%-------------------------------------------------------------------
-module(ewr_deps_engine).

-include("eunit.hrl").

-export([init/2, deps/2]).

-record(state, {ver_resolver,
                dep_resolver,
                visited_pkgs = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  init the state for the dependency engine.
%% @spec init(VersionResolver, DepsResolver) -> ok
%% @end
%%--------------------------------------------------------------------
init(VersionResolver, DepsResolver) ->
    #state{ver_resolver=VersionResolver,
           dep_resolver=DepsResolver}.

%%--------------------------------------------------------------------
%% @doc 
%%  Given the initial list of deps queries the repo, or more 
%%  specifically the resolver for further dependency information.
%% @spec deps(State, PackageList) -> DepList
%% @end
%%--------------------------------------------------------------------
deps(State, PackageList) ->
    Limits = lists:foldl(fun(Info = {_, _}, Acc) ->
                                 add_dep_to_limit(Acc, Info)
                         end, new_limits(), PackageList),
    Pkgs = lists:map(fun dep_pkg/1, PackageList),
    all_deps(State, Pkgs, Limits).


%%====================================================================
%% Internal Functions 
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  given a Pkg | {Pkg, Ver} | {Pkg, Ver, gte} return Pkg  
%% @spec dep_pkg(PackageVersionPair) -> Pkg
%% @end
%% @private
%%--------------------------------------------------------------------
dep_pkg(Pkg) when is_atom(Pkg) -> 
    Pkg;
dep_pkg({Pkg, _Ver}) -> 
    Pkg;
dep_pkg({Pkg, _Ver, gte}) -> 
    Pkg.

%%--------------------------------------------------------------------
%% @doc 
%% limits is an associated list keeping track of all the limits that have
%% been placed on a package:
%%    [{PkgA, LimitA}, {PkgB, LimitB}, ..]
%%
%% a limit is either:
%%   {}
%%   {Version}
%%   {gte, Version}
%% @spec new_limits() -> []
%% @end
%% @private
%%--------------------------------------------------------------------
new_limits() -> [].

%%--------------------------------------------------------------------
%% @doc 
%%  add a dep to the limit.
%% @spec add_dep_to_limit(PkgsLimits, Dep) -> DepList
%% @end
%% @private
%%--------------------------------------------------------------------
add_dep_to_limit(PkgsLimits, Dep) ->
    Pkg = dep_pkg(Dep),
    Limit = get_limits(PkgsLimits, Pkg),
    [{Pkg, increase_limit(Dep, Limit)} | lists:keydelete(Pkg, 1, PkgsLimits)].


%%--------------------------------------------------------------------
%% @doc 
%% Increase the current limit.
%% @spec increase_limit(Dep, Limit)  -> limit
%% @end
%% @private
%%--------------------------------------------------------------------
increase_limit(Dep, Limit) when is_atom(Dep) ->
    Limit;
increase_limit({_DPkg, Ver}, {}) ->
    {Ver};
increase_limit({_DPkg, DVer}, {LVer}) ->
    case lists:prefix(DVer, LVer) of
        true -> {LVer};
        false -> case lists:prefix(LVer, DVer) of
                     true -> {DVer};
                     false ->throw(fail)
                 end
    end;
increase_limit({_DPkg, DVer}, {gte, LVer}) when DVer >= LVer ->
    {DVer};
increase_limit({_DPkg, DVer}, {gte, LVer}) when DVer < LVer ->
    throw(fail);
increase_limit({_DPkg, DVer, gte}, {}) -> 
    {gte, DVer};
increase_limit({_DPkg, DVer, gte}, {LVer}) when DVer =< LVer ->
    {LVer};
increase_limit({_DPkg, DVer, gte}, {LVer}) when DVer > LVer ->
    throw(fail);
increase_limit({_DPkg, DVer, gte}, {gte, LVer}) when DVer =< LVer ->
    {gte, LVer};
increase_limit({_DPkg, DVer, gte}, {gte, LVer}) when DVer > LVer ->
    {gte, DVer}.

%%--------------------------------------------------------------------
%% @doc 
%%   get the current limits for a package.
%% @spec get_limits(PkgsLimits, Pkg) ->  Limits
%% @end
%% @private
%%--------------------------------------------------------------------
get_limits(PkgsLimits, Pkg) ->
    case lists:keysearch(Pkg, 1, PkgsLimits) of
        false -> 
            {};
        {value, {Pkg, Limits}} -> 
            Limits
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  extend the limits on the current package.
%% @spec extend_limits(PkgsLimits, Deps) -> NewDeps
%% @end
%% @private
%%--------------------------------------------------------------------
extend_limits(PkgsLimits, Deps) ->
    lists:foldl(fun (Dep, PLs) -> 
                        add_dep_to_limit(PLs, Dep) 
                end,
                PkgsLimits, Deps).

%%--------------------------------------------------------------------
%% @doc 
%%   Get the current limited packages versions.
%% @spec limited_package_versions(State, Pkg, PkgsLimits) -> NVersions
%% @end
%% @private
%%--------------------------------------------------------------------
limited_package_versions(State, Pkg, PkgsLimits) ->
    Versions = lists:reverse(lists:sort((State#state.ver_resolver)(Pkg))),
    lists:filter(fun (Ver) ->
                         Limit = get_limits(PkgsLimits, Pkg),
                         is_version_within_limit(Ver, Limit)
                 end, Versions).

%%--------------------------------------------------------------------
%% @doc 
%%  Check to see if the version is within the current limits.
%% @spec is_version_within_limit(Ver, Limit) -> true | NewLimit
%% @end
%% @private
%%--------------------------------------------------------------------
is_version_within_limit(_Ver, {}) ->
    true;
is_version_within_limit(Ver, {LVer}) -> 
    lists:prefix(LVer, Ver);
is_version_within_limit(Ver, {gte, LVer}) when Ver >= LVer -> 
    true;
is_version_within_limit(_Ver, _Pkg) -> 
    false.
    
%%--------------------------------------------------------------------
%% @doc 
%%  Check that all of the deps are available.
%% @spec all_deps(State, List, Limits) -> NewDeps
%% @end
%% @private
%%--------------------------------------------------------------------
all_deps(_State, [], Limits) ->
    PkgsVers = 
        lists:map(fun({Pkg, {Ver}}) -> 
                          {Pkg, Ver} 
                  end, Limits),
    {ok, PkgsVers};
all_deps(#state{visited_pkgs = Visited} = State, [Pkg | Pkgs], Limits) ->
    case lists_contains(Visited, Pkg) of
        true -> 
            all_deps(State, Pkgs, Limits);
        false ->
            deps(State, Pkg, Limits, Pkgs)
    end.


%%--------------------------------------------------------------------
%% @doc 
%%  Check that all of the deps are available.
%% @spec deps(State, Pkg, Limits, OtherPkgs) -> fail | DepList
%% @end
%% @private
%%--------------------------------------------------------------------
deps(#state{visited_pkgs = Visited} = State, Pkg, Limits, OtherPkgs) ->
    F = fun (Ver) ->
                Deps = (State#state.dep_resolver)(Pkg, Ver),
                ULimits = extend_limits(Limits, [{Pkg, Ver} | Deps]),
                DepPkgs = lists:map(fun dep_pkg/1, Deps),
                NewState = State#state{visited_pkgs = [Pkg | Visited]},
                all_deps(NewState, DepPkgs ++ OtherPkgs, ULimits)
        end,
    lists_some(F, limited_package_versions(State, Pkg, Limits), fail).

%%--------------------------------------------------------------------
%% @doc 
%% return the first Res = F(El) for El in List that doesn't throw False
%% see also hyperspec
%% @spec lists_some(F, List, False) -> Versions
%% @end
%% @private
%%--------------------------------------------------------------------
lists_some(_, [], False) ->
    False;
lists_some(F, [H | T], False) ->
    try
        F(H)
    catch
        False -> lists_some(F, T, False)
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  check to see if the list contains the value.
%% @spec lists_contains(List, Value) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
lists_contains([], _) ->
    false;
lists_contains([El | _T], El) ->
    true;
lists_contains([H | T], El) when H /= El ->
    lists_contains(T, El).
    

%%====================================================================
%% Tests
%%====================================================================
first_set_test() ->
    Pr = fun(app1) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]];
            (app2) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]];
            (app3) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]]
         end,
    Dr = fun(app1, [0, 1]) ->
                 [{app2, [0, 2]},
                  {app3, [0, 2], gte}];
            (app2, [0, 2]) ->
                 [{app3, [0, 3]}];
            (_, _) ->
                 []
         end,

    NM = ewr_deps_engine:init(Pr, Dr),
    
    X = ewr_deps_engine:deps(NM, [{app1, [0, 1]}]),
    ?assertMatch({ok, [{app3,[0,3]},
                       {app2,[0,2]},
                       {app1,[0,1]}]},
                 X).

second_set_fail_test() ->
    Pr = fun(app1) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]];
            (app2) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]];
            (app3) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]]
         end,
    Dr = fun(app1, [0, 1]) ->
                 [{app2, [0, 2]},
                  {app3, [0, 2], gte}];
            (app2, [0, 2]) ->
                 [{app3, [0, 1]}];
            (_, _) ->
                 []
         end,

    NM = ewr_deps_engine:init(Pr, Dr),
    
    X = ewr_deps_engine:deps(NM, [{app1, [0, 1]}]),
    ?assertMatch(fail, X).


third_set_test() ->
    Pr = fun(app1) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]];
            (app2) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]];
            (app3) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]];
            (app4) ->
                 [[0, 1],
                  [0, 2],
                  [0, 3]]
         end,
    Dr = fun(app1, [0, 1]) ->
                 [{app2, [0, 1], gte},
                  {app4, [0, 2]},
                  {app3, [0, 2], gte}];
            (app2, [0, 1]) ->
                 [{app3, [0, 2], gte}];
            (app2, [0, 2]) ->
                 [{app3, [0, 2], gte}];
            (app2, [0, 3]) ->
                 [{app3, [0, 2], gte}];
            (app3, [0, 1]) ->
                 [{app4, [0, 2], gte}];
            (app3, [0, 2]) ->
                 [{app4, [0, 2]}];
            (app4, [0, 2]) ->
                 [{app2, [0, 2], gte},
                  {app3, [0, 3]}];
            (_, _) ->
                 []
         end,

    NM = ewr_deps_engine:init(Pr, Dr),
    X = ewr_deps_engine:deps(NM, [{app1, [0, 1]}, 
                                  {app2, [0, 3]}]),
    ?assertMatch({ok,[{app3,[0,3]},{app2,[0,3]},{app4,[0,2]},{app1,[0,1]}]},
                 X).


fith_test() ->
    Pr = fun(a) -> [[1]];
            (b) -> [[1]];
            (c) -> [[1]]
         end,
    Dr = fun(a, [1]) -> [b];
            (b, [1]) -> [c];
            (c, [1]) -> [a];
            (_, _) -> []
         end,

    NM = ewr_deps_engine:init(Pr, Dr),
    X = ewr_deps_engine:deps(NM, [{a, [1]}]),
    ?assertMatch({ok,[{a,[1]},{c,[1]},{b,[1]}]},
                 X).


sixth_test() ->
    Pr = fun(a) -> [[1], [2]];
            (b) -> [[1]];
            (c) -> [[1]]
         end,
    Dr = fun(a, [1]) -> [b];
            (a, [2]) -> [b];
            (b, [1]) -> [c];
            (c, [1]) -> [{a, [2]}];
            (_, _) -> []
         end,

    NM = ewr_deps_engine:init(Pr, Dr),
    X = ewr_deps_engine:deps(NM, [{a, [1]}]),
    ?assertMatch(fail, X).

