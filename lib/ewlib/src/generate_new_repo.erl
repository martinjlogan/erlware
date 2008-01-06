main(Args) ->
    Source = lists:nth(1, Args),
    io:format("Using source ~s~n", [Source]),
    Target = lists:nth(2, Args),
    License = lists:nth(3, Args),
    {ok, Files} = file:list_dir(Source),
    lists:foreach(fun(Value) ->
                          handle_creation(Source, Target, License, Value)
                  end, Files).


handle_creation(Source, Target, License, Value) ->
    {Name, Version} = split_name(Value, []),
    IsNative = filelib:is_dir(filename:join([Source, Value, "c_src"])), 
    copy_meta(Source, Target, License, Value, Name, Version, IsNative),
    TargetDir = case IsNative of 
                    true ->
                        filename:join([Target, "i686-pc-linux-gnu-glibc-2.4", Name, Version]);
                    false ->
                        filename:join([Target, "Generic", Name, Version])
                end,
    SourceDir = filename:join([Target, "Sources", Name, Version]),
    filelib:ensure_dir(filename:join([TargetDir, "tmp"])),
    filelib:ensure_dir(filename:join([SourceDir, "tmp"])),
    NewTarget = filename:join([TargetDir, lists:flatten([Name, ".tar.gz"])]),
    {ok, TarDesc} = erl_tar:open(NewTarget, [compressed, write]),
    erl_tar:add(TarDesc, filename:join([Source, Value]), Value, [dereference]),
    erl_tar:close(TarDesc),
    SourceTarget = filename:join([SourceDir, lists:flatten([Name, ".tar.gz"])]),
    {ok, TarDesc2} = erl_tar:open(SourceTarget, [compressed, write]),
    erl_tar:add(TarDesc2, filename:join([Source, Value]), Name, [dereference]),
    erl_tar:close(TarDesc2).



copy_meta(Source, Target, License, Value, Name, Version, IsNative) ->
    Meta = filename:join([Target, "Meta", Name, Version]),
    DotApp = lists:flatten([Name, ".app"]),
    NLicense = filename:join([Meta, "LICENSE.txt"]),
    filelib:ensure_dir(filename:join([Meta, "tmp"])),
    file:copy(filename:join([Source, Value, "ebin", DotApp]), 
              filename:join([Meta, DotApp])),
    file:copy(License, NLicense),
    case IsNative of
        true ->
            file:write_file(filename:join([Meta, "native"]), <<>>);
        _ ->
            ok
    end.


split_name([$- | T], Acc) ->
    {lists:reverse(Acc), T};
split_name([H | T], Acc) ->
    split_name(T, [H | Acc]);
split_name([], Acc) ->
    Acc.
