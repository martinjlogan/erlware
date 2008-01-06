%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Support for generating repository documentation
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 17 Jul 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------

main([MetaDir, OutDir]) ->
    {ok, DirNames} = file:list_dir(MetaDir),
    write_erts_list(OutDir, lists:sort(DirNames)),
    process_each_erts(MetaDir, OutDir, DirNames).


process_each_erts(MetaDir, OutDir, [Erts | Rest]) ->
    MetaLoc = filename:join([MetaDir, Erts, "Meta"]),
    Apps = filelib:fold_files(MetaLoc, "^.+\.app$", true, 
                              fun(AppFile, Dict) ->
				process_app_file(AppFile, Dict)
			      end, dict:new()),
    {ok, Dev} = file:open(filename:join([OutDir, Erts ++ ".txt"]),
                         [write]),
    write_app_header(Dev),
    ListV = lists:sort(dict:to_list(Apps)),
    lists:map(fun({Key, Value}) ->                     
                     AppName = string:to_lower(atom_to_list(Key)),
		     file:write(Dev, lists:flatten(["=====", AppName, 
                                                    "=====\n"])),
                     print_application_info(Dev, Erts, OutDir,
                                              AppName, Value)
             end, ListV),
    file:close(Dev),
    process_each_erts(MetaDir, OutDir, Rest);
process_each_erts(_, _, []) ->
    ok.

process_app_file(AppFile, Dict) ->
    {ok, [Contents = {_, Name, _}]} = file:consult(AppFile),
    dict:append_list(Name, [Contents], Dict).

write_app_header(Dev) ->
    file:write(Dev, ["====== Resident Applications ======\n\n"]).


print_application_info(Dev, Erts, OutDir, SAppName, [{_, _, Body} | Rest]) ->
    {value, {vsn, Version}} = lists:keysearch(vsn, 1, Body),
    file:write(Dev, lists:flatten(["    * [[", Erts, "/", 
                                   SAppName, "/", Version, " | ",
                                   SAppName, "-", Version, "]]\n"])),
    FileName = lists:flatten([Erts, "_", SAppName, "_", Version]),
    {ok, Dev2} = file:open(filename:join([OutDir, FileName ++ ".txt"]), 
                           [write]),
    file:write(Dev2, ["======", SAppName, " ", Version, "======\n\n"]),
    write_description(Dev2, lists:keysearch(description, 1, Body)),
    write_main_module(Dev2, lists:keysearch(mod, 1, Body)),
    write_vsn_deps(Dev2, 
                   lists:keysearch(versioned_dependencies, 1, Body), Erts),
    write_deps(Dev2, lists:keysearch(applications, 1, Body), Erts),
    write_registered(Dev2, lists:keysearch(registered, 1, Body)),
    write_modules(Dev2, lists:keysearch(modules, 1, Body)),
    file:close(Dev2),
    print_application_info(Dev, Erts, OutDir, SAppName, Rest);
print_application_info(_, _, _, _, []) ->
    ok.

write_main_module(Dev, false) ->
    file:write(Dev, "===== Main Module =====\n\n" ++
               "**LIBRARY APP - NO MAIN MODULE**\n\n");    
write_main_module(Dev, {value, {mod, {Name, _}}}) ->
    SAppName = atom_to_list(Name),
    file:write(Dev, "===== Main Module =====\n\n" ++
               "**" ++ SAppName ++ "**\n\n"). 


write_registered(_, false) ->
    ok;
write_registered(Dev, {value, {registered, List}}) ->
    file:write(Dev, "===== Registered Names =====\n\n"),
    lists:map(fun(ModuleName) ->
                      SModuleName = atom_to_list(ModuleName),
                      file:write(Dev, "  * " ++ SModuleName ++ "\n")
              end, List).

write_modules(_, false) ->
    ok;
write_modules(Dev, {value, {modules, List}}) ->
    file:write(Dev, "===== Modules =====\n\n"),
    lists:map(fun(ModuleName) ->
                      SModuleName = atom_to_list(ModuleName),
                      file:write(Dev, "  * " ++ SModuleName ++ "\n")
              end, List).



write_deps(_, false, _) ->
    ok;
write_deps(Dev, {value, {applications, List}}, Erts) ->
    file:write(Dev, "===== Dependencies =====\n\n"),
    lists:map(fun(AppName) ->
                      SAppName = atom_to_list(AppName),
                      file:write(Dev, "  *[[" ++ Erts ++ 
                                 "#" ++ SAppName ++ " | " ++ SAppName ++ "]]\n")
              end, List).

   
write_vsn_deps(_, false, _) ->
    ok;
write_vsn_deps(Dev, {value, {versioned_dependencies, Deps}}, Erts) ->
    file:write(Dev, "===== Versioned Dependencies =====\n\n"),
    write_vsn_dep(Dev, Deps, Erts).


write_vsn_dep(Dev, [{Name, Vsn} | Rest], Erts) ->
    SAppName = atom_to_list(Name),
    file:write(Dev, "  *[[" ++ Erts ++ "_" ++ 
               SAppName ++ "_" ++ Vsn ++ "]]\n\n"),
    write_vsn_dep(Dev, Rest, Erts);
write_vsn_dep(Dev, [{Name, Vsn, Gte} | Rest], Erts) ->
    SAppName = atom_to_list(Name),
    SOpt = atom_to_list(Gte),
    file:write(Dev, "  *[[" ++ Erts ++ "#" ++ SAppName ++ 
               " | " ++ SAppName ++ " " ++ Vsn ++ 
               " (" ++ SOpt ++ ")" ++ "]]\n\n"),
    write_vsn_dep(Dev, Rest, Erts);
write_vsn_dep(_, [], _) ->
    ok.

write_description(Dev, false) ->
    file:write(Dev, "  NO DESCRIPTION \n\n");
write_description(Dev, {value, {description, Desc}}) ->
    file:write(Dev, Desc),
    file:write(Dev, "\n\n").


write_erts_list(OutDir, Names) ->
    {ok, Dev} = file:open(filename:join([OutDir, "applications.txt"]), [write]),
    file:write(Dev, "====== Erlang Runtime Version ======\n\n"),
    write_versions(Dev, Names),
    file:close(Dev).

write_versions(Dev, [Name | Rest]) ->
    file:write(Dev, "  *  [[" ++ Name ++ "]]\n"),
    write_versions(Dev, Rest);
write_versions(_Dev, []) ->
    ok.

