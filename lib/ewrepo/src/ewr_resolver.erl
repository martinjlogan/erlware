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
%%% @doc
%%%  Resolves individual items for the dependency engine.
%%% @copyright Erlware 2007
%%% @end
%%%-------------------------------------------------------------------
-module(ewr_resolver).

-behaviour(gen_server).

-include("eunit.hrl").



%% API
-export([start_link/3,
         shutdown/0,
         package_versions/1,
         package_dependencies/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {urls, 
                version_list = [],
                dep_list = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Urls, InitialDeps, InitialVsns) -> {ok,Pid} | ignore | {error,Error}
%% 
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(Urls, InitialDeps, InitialVsns) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
                          [Urls, InitialDeps, InitialVsns], []).

%%--------------------------------------------------------------------
%% @doc 
%%  Shut down the process.
%% @spec shutdown() -> ok
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    gen_server:cast(?SERVER, shutdown).

%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of versions available for the specified package.
%% 
%% @spec package_versions(Package) -> VersionList | Error
%% @end
%%--------------------------------------------------------------------
package_versions(Package) ->
    gen_server:call(?SERVER, {package_versions, Package}, infinity).

%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of dependencies for the specified package and the
%%  specified version.
%% 
%% @spec package_dependencies(Package, Version) -> Deps | Error
%% @end
%%--------------------------------------------------------------------
package_dependencies(Package, Version) ->
    gen_server:call(?SERVER, {package_dependencies, Package, Version}, 
                    infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% 
%% @doc 
%% Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([Urls, InitialDeps, InitialVsns]) ->
    ibrowse:start(),
    {ok, #state{urls=Urls, dep_list=InitialDeps, version_list=InitialVsns}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% 
%% @doc 
%% Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call({package_versions, Package}, _From, State) ->
    try package_versions(State, Package) of     
        {Versions, NState} ->
            {reply, Versions, NState}
    catch 
        throw:_ ->
            {reply, [], State}
    end;
handle_call({package_dependencies, Package, Version}, _From, State) ->
    try package_dependencies(State, Package, Version) of
        {Deps, NState} ->
            {reply, Deps, NState}
    catch 
        throw:_ ->
            {reply, [], State}
    end.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(shutdown, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% 
%% @doc 
%% Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% 
%% @doc 
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% 
%% @doc 
%% Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of versions available for the specified package.
%% 
%% @spec package_versions(Urls, Package) -> VersionList | Error
%% @end
%%--------------------------------------------------------------------
package_versions(State, Package) when is_atom(Package) ->
    VList = State#state.version_list,
    case lists:keysearch(Package, 1, VList) of
        {value, {Package, Versions}} ->
            {Versions, State};
        false ->
            Suffix = ewr_util:gen_metadata_stub_suffix(Package),
            Versions = try_get_package_versions(State#state.urls, Suffix),
            {Versions, State#state{version_list=[{Package, Versions} | VList]}}
    end;
package_versions(_, _) ->
    throw({error, "Package name must be an atom"}).



%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of dependencies for the specified package and the
%%  specified version.
%% 
%% @spec package_dependencies(Urls, Package, Version) -> Deps | Error
%% @end
%%--------------------------------------------------------------------
package_dependencies(State, Package, Version) ->
    Deps = State#state.dep_list,
    Key = {Package, Version},
    case lists:keysearch(Key, 1, Deps) of
        {value, {_, Value}} ->
            {Value, State};
        false ->
            NPackage = atom_to_list(Package),
            NVersion = ewr_util:version_to_string(Version),
            Suffix = ewr_util:gen_metadata_suffix(NPackage, NVersion),
            NDeps = try_get_package_dependencies(State#state.urls, 
                                                 NPackage, Suffix),    
            {NDeps, State#state{dep_list=[{Key, NDeps} | Deps]}}
    end.


%%--------------------------------------------------------------------
%% @doc 
%%  Try to get the list of package dependencies
%%
%% @spec try_get_package_dependencies(Urls, Package, Suffix) -> DepList | {error, Reason}
%% @end
%%--------------------------------------------------------------------
try_get_package_dependencies([Url | T], Package, Suffix) ->
    NUrl = lists:flatten([Url, Suffix, Package, ".app"]),
    case ewr_util:consult_url(NUrl) of
       {ok, Term} ->
           handle_parse_output(Term);
       {error, _} ->
           try_get_package_dependencies(T, Package, Suffix)
   end;
try_get_package_dependencies(_, Package, _) ->
    error_logger:warning_msg("Package ~w is not in repository! unable to "
                             "resolve dependencies.~n",
                             [Package]),
    throw({error, "Unable to find package dependecies"}).

%%--------------------------------------------------------------------
%% @doc 
%%  Try to get the list of package versions from the system.
%%
%% @spec try_get_package_versions(Urls, Suffix) -> Term | {error, Reason}
%% @end
%%--------------------------------------------------------------------
try_get_package_versions([Url | T], Suffix) ->
    NUrl = lists:flatten([Url, Suffix]),
    case ibrowse:send_req(NUrl, [{"Connection", "TE"}, 
                                {"TE", "trailers"}, 
                                {"Depth", "1"}, 
                                {"Content-Type", "application/xml"}], 
                          propfind, "") of
        {ok, "207", _, Body} -> 
            parse_out_package_version(Body);
        {ok, _, _, _} -> 
            try_get_package_versions(T, Suffix);
        {error, _} ->
            try_get_package_versions(T, Suffix)
    end;
try_get_package_versions([], Suffix) ->
    error_logger:warning_msg("Unable to find version list at ~s. Package probably "
                             "isn't in any of the available repositories~n", [Suffix]),
    throw({error, "Unable to find version list"}).


%%--------------------------------------------------------------------
%% @doc 
%%  Parse out the package version list from the returned string.
%% @spec parse_out_package_version(Body) -> VersionList
%% @end
%%--------------------------------------------------------------------
parse_out_package_version(Body) ->
    case xmerl_scan:string(Body, []) of
        {Elem, _} ->
            get_package_versions(Elem);
        Err ->
            throw(Err)
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of items from the parsed xml.
%% @spec get_package_versions(Elem) -> Vn
%% @end
%%--------------------------------------------------------------------
get_package_versions(Elem) ->
    case lists:sort(xmerl_xs:value_of(xmerl_xs:select("//D:href", Elem))) of
        [H | T] ->
            gather_versions(H, T);
        [] ->
            [];
        Else ->
            throw({error, {got_from_url, Else}})
    end.

%%--------------------------------------------------------------------
%% @doc 
%%  Parse out the version from the full url.
%% @spec gather_versions(Base, Rest) -> VersionList
%% @end
%%--------------------------------------------------------------------
gather_versions(Base, Rest) ->
    gather_versions(length(Base), Rest, []).

gather_versions(Base, [H | T], Acc) ->
    gather_versions(Base, T,
	[ewr_util:parse_version(string:strip(lists:nthtail(Base, H), both, $/)) | Acc]);
gather_versions(_Base, [], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc 
%%  get the version the deps and the versioned deps from an *.app
%%  term.
%%
%% @spec handle_parse_output(AppTerm) -> {Vsn, VersionedDeps, Deps} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
handle_parse_output({application, _, Ops}) ->
    VDeps = get_vdeps(Ops),
    Deps = lists:umerge(lists:sort(get_deps(Ops)),
                        lists:sort(get_ideps(Ops))),
    ewr_util:merge_per_app_deps(Deps, VDeps);
handle_parse_output(_) ->
   throw({error, "Invalid dependency info"}).


%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of versioned dependencies from the op list.
%% @spec get_vdeps(OppList) -> VersionedDependencies
%% @end
%% @private
%%--------------------------------------------------------------------
get_vdeps([{versioned_dependencies, V} | _T]) ->
    process_vdeps(V, []);
get_vdeps([_ | T]) ->
    get_vdeps(T);
get_vdeps([]) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc 
%%  Process the list of versioned dependencies.
%% @spec process_vdeps(VList, Acc) -> VersionedList
%% @end
%% @private
%%--------------------------------------------------------------------
process_vdeps([{Pkg, Vsn} | T], Acc) ->
    process_vdeps(T, [{Pkg, ewr_util:parse_version(Vsn)} | Acc]);
process_vdeps([{Pkg, Vsn, Misc} | T], Acc) ->
    process_vdeps(T, [{Pkg, ewr_util:parse_version(Vsn), Misc} | Acc]);
process_vdeps([], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of non-versioned dependencies from the oplist. This
%%  is specifed in the applications entry.
%% @spec get_deps(OpList) -> Dependencies
%% @end
%% @private
%%--------------------------------------------------------------------
get_deps([{applications, List} | _T]) ->
    List;
get_deps([_ | T]) ->
    get_deps(T);
get_deps([]) ->
    [].


%%--------------------------------------------------------------------
%% @doc 
%%  Get the list of included applications.
%% @spec get_ideps(OpList) -> IncludedDependencies
%% @end
%% @private
%%--------------------------------------------------------------------
get_ideps([{included_applications, List} | _T]) ->
    List;
get_ideps([_ | T]) ->
    get_ideps(T);
get_ideps([]) ->
    [].



%%====================================================================
%% tests
%%====================================================================
handle_parse_output_test() ->
    Data = {application, testapp,
            [{vsn, "0.1.0"},
             {description, "test test test"},
             {versioned_dependencies,
              [{app1, "0.1.0"}, {app2, "0.33.1", gte},
               {app3, "33.11.3"}]},
             {applications, [app1, app2, app3, app4, app5]}]},
    ?assertMatch([{app3, [33, 11, 3]}, {app2, [0, 33, 1],gte}, 
                  {app1, [0, 1, 0]}, app5, app4],
                 handle_parse_output(Data)).

    
