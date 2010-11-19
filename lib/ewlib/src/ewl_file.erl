%%%-------------------------------------------------------------------
%%% Copyright (c) 2006 Erlware
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
%%% @author  Martin J. Logan 
%%%
%%% @doc
%%%  Functions to aid in common file system operations that are not supplied in the erlang stdlib. 
%%% @end
%%%-------------------------------------------------------------------
-module(ewl_file).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 join_paths/2,
	 find/2,
	 md5/1,
	 md5_checksum/1,
	 delete_dir/1,
	 copy_dir/2,
	 copy_file/2,
	 create_tmp_dir/1,
	 make_tmp_dir/0,
	 mkdir_p/1,
	 compress/2,
	 compress/3,
	 uncompress/1,
	 uncompress/2,
	 gsub_file/3
        ]).

%%--------------------------------------------------------------------
%% Include Files
%%--------------------------------------------------------------------
-include("eunit.hrl").

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc delete a non empty directory.
%% @spec delete_dir(Path) -> ok 
%% @end
%%--------------------------------------------------------------------
delete_dir(Path) ->
    case filelib:is_dir(Path) of
	false ->
	    case filelib:is_file(Path) of
		false -> 
		    case file:read_link_info(Path) of
			{ok, LinkInfo} ->
			    %% XXX Exploiting the structure of a record, tisk, tisk, should probably include the proper .hrl file.
			    symlink = element(3, LinkInfo),
			    ok = file:delete(Path);
			_ ->
			    ok
		    end;
		true -> 
		    ok = file:delete(Path)
	    end;
	true ->
	    lists:foreach(fun(ChildPath) -> delete_dir(ChildPath) end, filelib:wildcard(filename:join(Path, "*"))),
	    ok = file:del_dir(Path)
    end.


%%-------------------------------------------------------------------
%% @doc return an md5 checksum string of a binary.
%% @spec (Bin) -> string()
%% @end
%%-------------------------------------------------------------------
md5_checksum(Bin) ->
    MD5 = md5(Bin),
    list_to_binary(io_lib:fwrite("~s", [MD5])).

%%-------------------------------------------------------------------
%% @doc return the hex encoded md5 string for a binary
%% @spec (Bin) -> string()
%% @end
%%-------------------------------------------------------------------
md5(Bin) -> hex(binary_to_list(erlang:md5(Bin))).

hex(L) when is_list (L) -> lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f -> [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I)               -> [$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.

%%--------------------------------------------------------------------
%% @doc copy an entire directory to another location. 
%% @spec copy_dir(From, To) -> ok
%% @end
%%--------------------------------------------------------------------
copy_dir(From, To) ->
    case filelib:is_dir(From) of
	false ->
	    case copy_file(From, To) of
		ok              -> ok;
		{error, enoent} -> throw({error, {enoent, From, To}})
	    end;
	true ->
	    case filelib:is_dir(To) of
		true  -> ok;
		false -> ok = mkdir_p(To)
	    end,
	    lists:foreach(fun(ChildFrom) -> 
				  copy_dir(ChildFrom, lists:flatten([To, "/", filename:basename(ChildFrom)]))
			  end, filelib:wildcard(filename:join(From, "*")))
    end.

%%--------------------------------------------------------------------
%% @doc copy a file including timestamps,ownership and mode etc.
%% @spec copy_file(From::string(), To::string()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
copy_file(From, To) ->
    {ok, _} = file:copy(From, To),
    {ok, FileInfo} = file:read_file_info(From),
    file:write_file_info(To, FileInfo). 

%%--------------------------------------------------------------------
%% @deprecated Please use the function {@link make_tmp_dir} instead.
%% @doc create a unique temorory directory.
%% @spec create_tmp_dir(Prefix::string()) -> {ok, TmpDirPath} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
create_tmp_dir(Prefix) ->
    TmpDirPath = lists:flatten([Prefix, "/.ewl_tmp-", integer_to_list(element(3, now())), "/"]) ,
    case mkdir_p(TmpDirPath) of
	ok    -> {ok, TmpDirPath};
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc make a unique temorory directory.
%% @spec make_tmp_dir() -> TmpDirPath
%% @end
%%--------------------------------------------------------------------
make_tmp_dir() ->
    TmpDirPath = filename:join([tmp(), lists:flatten([".tmp_dir", integer_to_list(element(3, now()))])]),
    try
	ok = mkdir_p(TmpDirPath),
	TmpDirPath
    catch
	_C:E -> throw({make_tmp_dir_failed, E})
    end.
	     
tmp() ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    throw(tmp_dir_not_supported_on_windows);
	_SysArch ->
	    "/tmp"
    end.

%%-------------------------------------------------------------------
%% @doc
%% Makes a directory including parent dirs if they are missing. 
%% @spec mkdir_p(Path) -> ok | exit()
%% @end
%%-------------------------------------------------------------------
mkdir_p(Path) ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    filelib:ensure_dir(lists:flatten([filename:absname(Path), "\\"]));
	_SysArch ->
	    filelib:ensure_dir(lists:flatten([filename:absname(Path), "/"]))  
    end.

%%-------------------------------------------------------------------
%% @doc Compress a file or directory using the native os compression
%% system. For linux/unix it is tar. The semantics of this function
%% are very straight forward. Indicate where you want the tar file
%% to be placed and indicate what file you want to tar up. This will
%% do that. It will tar the target file as if it was doing it from
%% directory that contains it. 
%% <pre>
%% Variables:
%%  TarFilePath - The name or path to the file to be produced as a result of the tar command.
%%  TargetFilePath - The path to the directory or file to be compressed.
%%
%% Examples:
%%  compress("foo.tar.gz", "tmp/foo")
%%  compress("/home/martinjlogan/foo.tar.gz", "tmp/foo") % Will put foo.tar.gz into /home/martinjlogan
%% </pre>
%% @spec compress(TarFilePath::string(), TargetFilePath::string()) -> ok | exit()
%% @end
%%-------------------------------------------------------------------
compress(TarFilePath, TargetFilePath) ->
    %% Wrapping this just in case we have to go back to os specific code - I am tired of changing this all over the place :) 
    TargetFileName = filename:basename(TargetFilePath),
    BaseDir = filename:dirname(TargetFilePath),
    compress(TarFilePath, [TargetFileName], [compressed, verbose, {cd, BaseDir}]).

%%-------------------------------------------------------------------
%% @doc Just like erl tar but gives you the cd option to run the tar
%%      command from a particular directory.
%% <pre>
%% Variables:
%%  TarFilePath - The name or path to the file to be produced as a result of the tar command.
%%  FileList - The list of files to add to the tar.
%%  Options - Tar options
%%
%% Examples:
%%  compress("foo.tar.gz", ["erlware/foo"], [{cd, "/usr/local/"}])  
%%   This will compress from the directory /usr/local 
%%
%%  compress("/home/martinjlogan/foo.tar.gz", ["tmp/foo"], []) % Will put foo.tar.gz into /home/martinjlogan
%% </pre>
%% @spec compress(TarFilePath::string(), FileList::list(), OptionsList) -> ok | exit()
%% where
%%  OptionsList = RegularErlTarOpts | {cd, Path} 
%% @end
%%-------------------------------------------------------------------
compress(TarFilePath, FileList, OptionsList) ->
    TarFileName = filename:basename(TarFilePath),
    BaseDir = get_base_dir(OptionsList),

    Fun = fun() ->
		  erl_tar:create(TarFileName, FileList, lists:keydelete(cd, 1, OptionsList)),
		  file:rename(TarFileName, TarFilePath)
	  end,
    run_in_location(Fun, BaseDir).

get_base_dir(OptionsList) ->
    case lists:keyfind(cd, 1, OptionsList) of
	false ->
	    {ok, CWD} = file:get_cwd(),
	    CWD;
	{cd, Path} ->
	    Path
    end.

%%-------------------------------------------------------------------
%% @doc Uncompress a file or directory using the native os compression system. For linux/unix it is tar. 
%% <pre>
%% Variables:
%%  TarFileName - The path and name of the tar file to be untarred.
%%  TargetDirPath - The directory to untar into.
%% </pre>
%% @spec uncompress(TarFilePath::string(), TargetDirPath::string()) -> ok | exit()
%% @end
%%-------------------------------------------------------------------
uncompress(TarFilePath, TargetDirPath) ->
    %% Wrapping this just in case we have to go back to os specific code - I am tired of changing this all over the place :) 
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
    
    
%% @spec uncompress(TarFilePath::string()) -> ok | exit()
%% @equiv uncompress(TarFilePath, CurrentWorkingDirectory) 
uncompress(TarFilePath) ->
    {ok, CWD} = file:get_cwd(),
    uncompress(TarFilePath, CWD).
    
    
%%-------------------------------------------------------------------
%% @doc
%%  Finds files and directories that match the regexp supplied in the TargetPattern regexp.
%%
%% @spec find(FromDir, TargetPattern) -> list()
%% @end
%%-------------------------------------------------------------------
find([], _) ->
    [];
find(FromDir, TargetPattern) ->
    case filelib:is_dir(FromDir) of
	false ->
	    case regexp:match(FromDir, TargetPattern) of 
		{match, _, _} -> [FromDir]; 
		_             -> []
	    end;
	true ->
	    FoundDir = case regexp:match(FromDir, TargetPattern) of 
		{match, _, _} -> [FromDir]; 
		_             -> []
	    end,
	    List = lists:foldl(fun(CheckFromDir, Acc) when CheckFromDir == FromDir -> 
				Acc;
			   (ChildFromDir, Acc) -> 
				case find(ChildFromDir, TargetPattern) of
				    []  -> Acc;
				    Res -> Res ++ Acc
				end
			end, [], filelib:wildcard(filename:join(FromDir, "*"))),
	    FoundDir ++ List
    end.



%%--------------------------------------------------------------------
%% @doc Concatinate two parts of a path and make sure they join correctly.
%% <pre>
%% Example: join_paths("/usr/local/", "/lib/erlang/") -> "/usr/local/lib/erlang"
%% </pre>
%% @spec join_paths(Path1, Path2) -> Url
%% @end
%%--------------------------------------------------------------------
join_paths(Path1, Path2) ->
    remove_trailing_slash(lists:flatten([remove_trailing_slash(Path1), ensure_leading_slash(Path2)])).


%%--------------------------------------------------------------------
%% @doc alter the contents of a file with regexp:gsub.
%% @spec gsub_file(FilePath, RegExp, New) -> {ok, RepCount} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
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

%%%====================================================================
%%% Internal functions
%%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc ensure that the string provided contains a single leading slash.
%% @spec ensure_leading_slash(String) -> string()
%% @end
%%--------------------------------------------------------------------
ensure_leading_slash(String) ->
    lists:flatten(["/", string:strip(String, left, $/)]).
    
%%--------------------------------------------------------------------
%% @private
%% @doc ensure that the string provided does not contain a trailing slash
%% @spec remove_trailing_slash(String) -> string()
%% @end
%%--------------------------------------------------------------------
remove_trailing_slash(String) ->
    string:strip(String, right, $/).

%%--------------------------------------------------------------------
%% @doc run the fun provided in the directory provided.
%% @spec run_in_location(Fun, Path) -> term()
%% where
%%  Paths = string()
%% @end
%%--------------------------------------------------------------------
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
