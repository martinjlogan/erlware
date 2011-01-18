%%%-------------------------------------------------------------------
%%% @author  Martin J. Logan
%%%
%%% @doc
%%%  Functions to aid in common file system operations that are not supplied in
%%%  the erlang stdlib.
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(ewl_file).

-export([
	 join_paths/2,
	 find/2,
	 md5/1,
	 md5_checksum/1,
	 delete_dir/1,
	 remove/2,
	 is_symlink/1,
	 copy_dir/2,
	 copy_file/2,
	 create_tmp_dir/1,
	 make_tmp_dir/0,
	 mkdir_p/1,
	 compress/2,
	 compress/3,
	 uncompress/1,
	 uncompress/2,
	 replace_file_contents/3,
	 gsub_file/3
	]).

-export_type([path/0]).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Types
%%============================================================================
-type path() :: string().

%%============================================================================
%% External functions
%%============================================================================


%%--------------------------------------------------------------------
%% @depricated
%% @doc delete a non empty directory.
%% @spec delete_dir(Path) -> ok
-spec delete_dir(path()) -> ok.
delete_dir(Path) ->
    remove(Path, [recursive]).

%%--------------------------------------------------------------------
%% @doc delete a file. Use the recursive option for directories.
%% <pre>
%% Example: remove("./tmp_dir", [recursive]).
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec remove(Path::string(), Options::list()) -> ok | {error, Reason::term()}.
remove(Path, Options) ->
    case lists:member(recursive, Options) of
	false -> file:delete(Path);
	true  -> remove_recursive(Path, Options)
    end.


%% @doc indicates witha boolean if the path supplied referes to
%%      symlink.
-spec is_symlink(Path::string()) -> boolean().
is_symlink(Path) ->
    case catch file:read_link_info(Path) of
	{ok, #file_info{type = symlink}} ->
	    true;
	_ ->
	    false
    end.


%% @doc return an md5 checksum string or a binary.

-spec md5_checksum(string() | binary()) -> string().
md5_checksum(Value) ->
    MD5 = md5(Value),
    list_to_binary(io_lib:fwrite("~s", [MD5])).

%% @doc
%% return the hex encoded md5 string for a binary
%% @end
-spec md5(string() | binary()) -> string().
md5(Value) ->
    hex(binary_to_list(erlang:md5(Value))).


%% @doc
%% copy an entire directory to another location.
%% @end
-spec copy_dir(path(), path()) -> ok.
copy_dir(From, To) ->
    case filelib:is_dir(From) of
	false ->
	    copy_file(From, To);
	true ->
	    case filelib:is_dir(To) of
		true  -> ok;
		false -> ok = mkdir_p(To)
	    end,
	    lists:foreach(fun(ChildFrom) ->
				  copy_dir(ChildFrom,
					   filename:join([To,
							  filename:basename(ChildFrom)]))
			  end, filelib:wildcard(filename:join(From, "*")))
    end.

%% @doc
%%  copy a file including timestamps,ownership and mode etc.
%% @end
-spec copy_file(From::string(), To::string()) -> ok.
copy_file(From, To) ->
    {ok, _} = file:copy(From, To),
    {ok, FileInfo} = file:read_file_info(From),
    file:write_file_info(To, FileInfo).


%% @deprecated Please use the function {@link make_tmp_dir} instead.
%% @doc create a unique temorory directory.
-spec create_tmp_dir(Prefix::path()) ->
    {ok, TmpDirPath::path()} | {error, term()}.
create_tmp_dir(Prefix) ->
    TmpDirPath = filename:join([Prefix, ".ewl_tmp-" ++
				integer_to_list(element(3, now()))]) ,
    case mkdir_p(TmpDirPath) of
	ok    -> {ok, TmpDirPath};
	Error -> Error
    end.

%% @doc make a unique temorory directory.
-spec make_tmp_dir() -> TmpDirPath::path().
make_tmp_dir() ->
    TmpDirPath =
	filename:join([tmp(),
		       lists:flatten([".tmp_dir",
				      integer_to_list(element(3, now()))])]),
    try
	ok = mkdir_p(TmpDirPath),
	TmpDirPath
    catch
	_C:E -> throw({make_tmp_dir_failed, E})
    end.


%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_p(path()) -> ok.
mkdir_p(Path) ->
    % We are exploiting a feature of ensuredir that that creates all
    % directories up to the last element in the filename, then ignores
    % that last element. This way we ensure that the dir is created
    % and don't have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).

%% @doc
%% Compress a file or directory using the native os compression
%% system. For linux/unix it is tar. The semantics of this function
%% are very straight forward. Indicate where you want the tar file
%% to be placed and indicate what file you want to tar up. This will
%% do that. It will tar the target file as if it was doing it from
%% directory that contains it.
%% <pre>
%% Variables:
%%  TarFilePath - The name or path to the file to be produced as a
%%  result of the tar command.
%%  TargetFilePath - The path to the directory or file to be compressed.
%%
%% Examples:
%%  compress("foo.tar.gz", "tmp/foo")
%%  % Will put foo.tar.gz into /home/martinjlogan
%%  compress("/home/martinjlogan/foo.tar.gz", "tmp/foo")
%% </pre>
%% @end
-spec compress(TarFilePath::path(), TargetFilePath::path()) -> ok.
compress(TarFilePath, TargetFilePath) ->
    % Wrapping this just in case we have to go back to os specific code - I am
    % tired of changing this all over the place :)
    TargetFileName = filename:basename(TargetFilePath),
    BaseDir = filename:dirname(TargetFilePath),
    compress(TarFilePath, [TargetFileName],
	     [compressed, verbose, {cd, BaseDir}]).

%% @doc
%% Just like erl tar but gives you the cd option to run the tar
%%      command from a particular directory.
%% <pre>
%% Variables:
%%  TarFilePath - The name or path to the file to be produced as a
%%  result of the tar command.
%%  FileList - The list of files to add to the tar.
%%  Options - Tar options
%%
%% Examples:
%%  compress("foo.tar.gz", ["erlware/foo"], [{cd, "/usr/local/"}])
%%   This will compress from the directory /usr/local
%%
%%  % Will put foo.tar.gz into /home/martinjlogan
%%  compress("/home/martinjlogan/foo.tar.gz", ["tmp/foo"], [])
%% </pre>
%% where
%%  OptionsList = RegularErlTarOpts | {cd, Path}
%% @end
-spec compress(TarFilePath::path(), [path()], OptionsList::list()) -> ok.
compress(TarFilePath, FileList, OptionsList) ->
    TarFileName = filename:basename(TarFilePath),
    BaseDir = get_base_dir(OptionsList),

    Fun = fun() ->
		  erl_tar:create(TarFileName, FileList,
				 lists:keydelete(cd, 1,
						 OptionsList)),
		  file:rename(TarFileName, TarFilePath)
	  end,
    run_in_location(Fun, BaseDir).

%% @doc Uncompress a file or directory using the native os compression
%% system. For linux/unix it is tar.
%% <pre>
%% Variables:
%%  TarFileName - The path and name of the tar file to be untarred.
%%  TargetDirPath - The directory to untar into.
%% </pre>

-spec uncompress(TarFilePath::path(), TargetDirPath::path()) -> ok.
uncompress(TarFilePath, TargetDirPath) ->
    % Wrapping this just in case we have to go back to os specific code - I am
    % tired of changing this all over the place :)
    TarFileName = filename:basename(TarFilePath),
    RelocatedTarFilePath = filename:join(TargetDirPath, TarFileName),
    case TarFilePath == RelocatedTarFilePath of
	false -> file:rename(TarFilePath, RelocatedTarFilePath);
	true  -> ok
    end,
    Fun = fun() ->
		  ok = erl_tar:extract(TarFileName, [compressed]),
		  case TarFilePath == RelocatedTarFilePath of
		      true  -> file:delete(TarFileName);
		      false -> ok
		  end
	  end,
    run_in_location(Fun, TargetDirPath).


%% @equiv uncompress(TarFilePath, CurrentWorkingDirectory)
-spec uncompress(TarFilePath::path()) -> ok.
uncompress(TarFilePath) ->
    {ok, CWD} = file:get_cwd(),
    uncompress(TarFilePath, CWD).


%% @doc Finds files and directories that match the regexp supplied in
%%  the TargetPattern regexp.

-spec find(FromDir::path(), TargetPattern::string()) -> list().
find([], _) ->
    [];
find(FromDir, TargetPattern) ->
    case filelib:is_dir(FromDir) of
	false ->
	    case re:run(FromDir, TargetPattern) of
		{match, _} -> [FromDir];
		_             -> []
	    end;
	true ->
	    FoundDir = case re:run(FromDir, TargetPattern) of
		{match, _} -> [FromDir];
		_             -> []
	    end,
	    List =
		lists:foldl(fun(CheckFromDir, Acc)
			       when CheckFromDir == FromDir ->
				    Acc;
			       (ChildFromDir, Acc) ->
				    case find(ChildFromDir, TargetPattern) of
					[]  -> Acc;
					Res -> Res ++ Acc
				    end
			    end,
			    [],
			    filelib:wildcard(filename:join(FromDir, "*"))),
	    FoundDir ++ List
    end.



%% @doc
%% Concatinate two parts of a path and make sure they join correctly.
%% <pre>
%% Example: join_paths("/usr/local/", "/lib/erlang/") -> "/usr/local/lib/erlang"
%% </pre>
%% @end
-spec join_paths(path(), path()) -> NewPath::path().
join_paths(Path1, Path2) ->
    remove_trailing_slash(lists:flatten([remove_trailing_slash(Path1),
					 ensure_leading_slash(Path2)])).


%% @deprecated Please use the function {@link replace_file_contents}
%% @doc
%% alter the contents of a file with regexp:gsub.
%% @end
-spec gsub_file(FilePath::path(), RegExp::string(), New::string()) ->
    {ok, RepCount::number()} | {error, term()}.
gsub_file(FilePath, RegExp, New) ->
    {ok, BinaryContents} =file:read_file(FilePath),
    Contents = binary_to_list(BinaryContents),
    case regexp:gsub(Contents, RegExp, New) of
	{ok, NewContents, RepCount} ->
	    {ok, IOD} = file:open(FilePath, [write]),
	    ok = io:fwrite(IOD, "~s", [NewContents]),
	    {ok, RepCount};
	Error ->
	    Error
    end.

%% @doc
%% alter the contents of a file with re:replace.
%% @end
-spec replace_file_contents(FilePath::path(), Re::string(), New::string()) -> ok.
replace_file_contents(FilePath, RegExp, New) ->
    {ok, BinaryContents} = file:read_file(FilePath),
    Contents = binary_to_list(BinaryContents),
    NewContents = re:replace(Contents, RegExp, New),
    {ok, IOD} = file:open(FilePath, [write]),
    ok = io:fwrite(IOD, "~s", [NewContents]).


%%============================================================================
%% Internal functions
%%============================================================================
-spec remove_recursive(path(), Options::list()) -> ok.
remove_recursive(Path, Options) ->
    case filelib:is_dir(Path) of
	false ->
	    file:delete(Path);
	true ->
	    lists:foreach(fun(ChildPath) ->
				  remove_recursive(ChildPath, Options)
			  end, filelib:wildcard(filename:join(Path, "*"))),
	    ok = file:del_dir(Path)
    end.


%% @doc
%% ensure that the string provided contains a single leading slash.
%% @end
-spec ensure_leading_slash(string()) -> string().
ensure_leading_slash(String) ->
    lists:flatten(["/", string:strip(String, left, $/)]).

%% @doc
%% ensure that the string provided does not contain a trailing slash
%% @end
-spec remove_trailing_slash(string()) -> string().
remove_trailing_slash(String) ->
    string:strip(String, right, $/).

%% @doc
%% run the fun provided in the directory provided.
%% where
%%  Paths = string()
%% @end
-spec run_in_location(function(), path()) -> term().
run_in_location(Fun, Path) ->
    {ok, CWD} = file:get_cwd(),
    ok = file:set_cwd(Path),
    try
	Fun()
    catch
	_C:E ->
	    throw({failed_to_run_command_in, Path, E})
    after
	file:set_cwd(CWD)
    end.


hex(L) when is_list (L) ->
    lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
    [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I)               ->
    [$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I)  -> $0 + I.

get_base_dir(OptionsList) ->
    case lists:keyfind(cd, 1, OptionsList) of
	false ->
	    {ok, CWD} = file:get_cwd(),
	    CWD;
	{cd, Path} ->
	    Path
    end.

tmp() ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    throw(tmp_dir_not_supported_on_windows);
	_SysArch ->
	    "/tmp"
    end.


%%%====================================================================
%%% Unit Tests
%%%====================================================================

ensure_leading_slash_test() ->
    ?assertMatch("/blah/", ensure_leading_slash("/blah/")),
    ?assertMatch("/blah/",  ensure_leading_slash("/blah/")),
    ?assertMatch("/blah",   ensure_leading_slash("/blah")),
    ?assertMatch("/blah", ensure_leading_slash("/blah")).

remove_trailing_slash_test() ->
    ?assertMatch("/blah", remove_trailing_slash("/blah/")),
    ?assertMatch("/blah",  remove_trailing_slash("/blah")),
    ?assertMatch("blah",    remove_trailing_slash("blah")).


setup_base_and_target() ->
    {ok, BaseDir} = ewl_file:create_tmp_dir("/tmp"),
    DummyContents = <<"This should be deleted">>,
    SourceDir = filename:join([BaseDir, "source"]),
    ok = file:make_dir(SourceDir),
    Name1 = filename:join([SourceDir, "fileone"]),
    Name2 = filename:join([SourceDir, "filetwo"]),
    Name3 = filename:join([SourceDir, "filethree"]),
    NoName = filename:join([SourceDir, "noname"]),

    ok = file:write_file(Name1, DummyContents),
    ok = file:write_file(Name2, DummyContents),
    ok = file:write_file(Name3, DummyContents),
    ok = file:write_file(NoName, DummyContents),
    {BaseDir, SourceDir, {Name1, Name2, Name3, NoName}}.

find_test() ->
    % Create a directory in /tmp for the test. Clean everything afterwards

    {setup,
     fun setup_base_and_target/0,
     fun ({BaseDir, _, _}) ->
	     ewl_file:delete_dir(BaseDir)
     end,
     fun ({BaseDir, _, {Name1, Name2, Name3, _}}) ->
	      ?assertMatch([Name2,
			    Name3,
			    Name1],
			   ewl_file:find(BaseDir, "file[a-z]+\$"))
      end}.

replace_contents_test() ->
    {setup,
     fun setup_base_and_target/0,
     fun ({BaseDir, _, _}) ->
	     ewl_file:delete_dir(BaseDir)
     end,
     fun ({_, _, {Name1, _, _, _}}) ->
	     ewl_file:replace_file_contents(Name1, "deleted\$", "CHANGED"),
	     {ok, Contents} = file:read_file(Name1),
	     ?assertMatch(<<"This should be CHANGED">>, Contents)
     end}.
