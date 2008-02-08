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
	 delete_dir/1,
	 copy_dir/2,
	 create_tmp_dir/1,
	 mkdir_p/1,
	 compress/2,
	 uncompress/1,
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
			    error_logger:info_msg("delete_dir/1 file does not exist ~p~n", [Path]), ok
		    end;
		true -> 
		    ok = file:delete(Path)
	    end;
	true ->
	    lists:foreach(fun(ChildPath) -> delete_dir(ChildPath) end, filelib:wildcard(Path ++ "/*")),
	    ok = file:del_dir(Path)
    end.

%%--------------------------------------------------------------------
%% @doc copy an entire directory to another location.
%% @spec copy_dir(From, To) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
copy_dir(From, To) ->
    case filelib:is_dir(From) of
	false ->
	    {ok, _} = file:copy(From, To),
	    ok;
	true ->
	    case filelib:is_dir(To) of
		true  -> ok;
		false -> ok = file:make_dir(To)
	    end,
	    lists:foreach(fun(ChildFrom) -> 
				  copy_dir(ChildFrom, lists:flatten([To, "/", filename:basename(ChildFrom)]))
			  end, filelib:wildcard(From ++ "/*"))
    end.

%%--------------------------------------------------------------------
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
%% @doc Compress a file or directory using the native os compression system. For linux/unix it is tar. 
%% <pre>
%% Variables:
%%  Name - The name of the file to be produced as a result of the compression.
%%  Path - The path to the directory or file to be compressed.
%% </pre>
%% @spec compress(Name::string(), Path::string()) -> ok | exit()
%% @end
%%-------------------------------------------------------------------
compress(Name, Path) ->
    %% Wrapping this just in case we have to go back to os specific code - I am tired of changing this all over the place :) 
    ok = erl_tar:create(Name, [Path], [compressed, verbose]).
    
%%-------------------------------------------------------------------
%% @doc Uncompress a file or directory using the native os compression system. For linux/unix it is tar. 
%% <pre>
%% Variables:
%%  Name - The name of the file to be produced as a result of the compression.
%% </pre>
%% @spec uncompress(Name::string()) -> ok | exit()
%% @end
%%-------------------------------------------------------------------
uncompress(Name) ->
    %% Wrapping this just in case we have to go back to os specific code - I am tired of changing this all over the place :) 
    ok = erl_tar:extract(Name, [compressed, verbose]).
    
    
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
			end, [], filelib:wildcard(FromDir ++ "/*")),
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
    {ok, BinaryContents} =file:read_file("FilePath"),
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

%%%====================================================================
%%% Unit Tests
%%%====================================================================

ensure_leading_slash_test() ->
    ?assertMatch("/blah/", ensure_leading_slash("/blah/")),
    ?assertMatch("blah/",  ensure_leading_slash("/blah/")),
    ?assertMatch("blah",   ensure_leading_slash("/blah")),
    ?assertMatch("//blah", ensure_leading_slash("/blah")).

remove_trailing_slash_test() ->
    ?assertMatch("/blah//", remove_trailing_slash("/blah")),
    ?assertMatch("/blah/",  remove_trailing_slash("/blah")),
    ?assertMatch("blah",    remove_trailing_slash("blah")).
