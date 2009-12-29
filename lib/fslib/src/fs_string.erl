%% Copyright (C) 1997, Ericsson Telecom AB
%% File:    fs_string.erl
%% Author:  Eric Newhuis
%%
%% @doc
%% String handling functions.
%% @end

-module (fs_string).
-vsn('1.0').

-export ([stringize/1,
	  lowercase/1,
	  uppercase/1,
	  integer_to_hex/1,
	  byte_to_hex/1,
	  hex_to_integer/1,
	  hex_to_string/1,
	  string_to_hex/1,
	  bash_encode/1,
	  binary_to_hex/1
	 ]).

%%------------------------------------------------------------------------------
%% @doc Change a string so that integer lists appear as quoted strings.
%%      For example, [65,66,67] becomes "ABC".
%%      Bug: There is no check that the integers are in the printable ranges.
%% <pre>
%%
%% Types:
%%  RawStr = string()
%%
%% </pre>
%% @spec stringize (RawStr) -> string()
%% @end
%%------------------------------------------------------------------------------
stringize (RawStr) -> ints_to_strings (RawStr, false).
ints_to_strings (Str, true ) -> Str;
ints_to_strings (Str, false) ->
    case int_to_string (Str) of
	ok     -> ints_to_strings (Str   , true );
	NewStr -> ints_to_strings (NewStr, false)
    end.

int_to_string (Str) ->
    {ok, IntStrRE} = regexp:parse ("\\[[0-9,]+\\]"),
    case regexp:first_match (Str, IntStrRE) of
	{match, Start, Length} ->
	    IntStrList = string:substr (Str, Start+1, Length-2),
	    IntegerStrings = string:tokens (IntStrList, ","),
	    String = lists:map (fun(I)-> {ok,[Char],[]} = io_lib:fread("~d",I), Char end, IntegerStrings),
	    string_replace (Str, Start, Length, "\"" ++ String ++ "\"");
	_ ->
	    ok
    end.

string_replace (Str, Start, Length, Replacement) ->
    FirstPart = string:substr (Str, 1, Start-1),
    ThirdPart = string:substr (Str, Start+Length),
    FirstPart ++ Replacement ++ ThirdPart.

%%------------------------------------------------------------------------------
%% @doc Converts all characters in a string to lowercase.
%% <pre>
%%
%% Types:
%%  String = string()
%%
%% </pre>
%% @spec lowercase(String) -> string()
%% @end
%%------------------------------------------------------------------------------
lowercase ([C|S]) -> [lowercase(C)|S];
lowercase (C) when C>=$A, C=<$Z -> C+32;
lowercase (C) -> C.

%%------------------------------------------------------------------------------
%% @doc Converts all characters in a string to uppercase.
%% <pre>
%%
%% Types:
%%  String = string()
%%
%% </pre>
%% @spec uppercase(String) -> string()
%% @end
%%------------------------------------------------------------------------------
uppercase ([C|S]) -> [uppercase(C)|S];
uppercase (C) when C>=$a, C=<$z -> C-32;
uppercase (C) -> C.

%%------------------------------------------------------------------------------
%% @doc Converts an integer to a hex-code character list (string).
%% <pre>
%%
%% Types:
%%  Integer = integer()
%%
%% </pre>
%% @spec integer_to_hex(Integer) -> string()
%% @end
%%------------------------------------------------------------------------------
integer_to_hex (I) when I <  10 -> integer_to_list (I);
integer_to_hex (I) when I <  16 -> [I - 10 + $a];
integer_to_hex (I) when I >= 16 -> N = I div 16, integer_to_hex (N) ++ integer_to_hex (I rem 16).

%%------------------------------------------------------------------------------
%% @doc Converts a byte to a two-character hex-code (character list) taking care
%%      to prepend ("0") if necessary.
%%
%% <pre>
%%
%% Types:
%%  Byte = integer()
%%  TwoCharacterString = string()
%%
%% </pre>
%%
%% @spec byte_to_hex (Byte) -> TwoCharacterString
%% @end
%%------------------------------------------------------------------------------

byte_to_hex (I) when I >= 0, I =< 255 ->
    case length (HexByte = integer_to_hex (I)) of
	2 -> HexByte;
	1 -> [$0 | HexByte]
    end.
	    

%%------------------------------------------------------------------------------
%% @doc Converts a hex-code character list (string) to an integer.
%% <pre>
%%
%% Types:
%%  Hex = string()
%%
%% </pre>
%% @spec hex_to_integer(Hex) -> integer()
%% @end
%%------------------------------------------------------------------------------
hex_to_integer (Hex) ->
    lists:foldl (fun (E, Acc) -> Acc * 16 + dehex (E) end, 0, Hex).

%%------------------------------------------------------------------------------
%% @doc Converts a character list into a character list that represents the
%% hex codes of each character in the input list.  The resulting list will
%% always have twice the length of the input list.
%% Example: string_to_hex("0123") -> "30313233"
%% <pre>
%%
%% Types:
%%  String = string()
%%
%% </pre>
%% @spec string_to_hex(String) -> string()
%% @end
%%------------------------------------------------------------------------------
string_to_hex (String) ->
    lists:foldr (fun (E, Acc) -> [hexc (E div 16), hexc (E rem 16) | Acc] end, [], String).

%%------------------------------------------------------------------------------
%% @doc Converts an asci hex code character list into the character list
%% represented by the input.  The resulting list will always be half the length
%% of the input list.  Example: hex_to_string("30313233") -> "0123"
%% <pre>
%%
%% Types:
%%  String = string()
%%
%% </pre>
%% @spec hex_to_string(String) -> string()
%% @end
%%------------------------------------------------------------------------------
hex_to_string (Hex) ->
    {String, _} = lists:foldr (fun (E, {Acc, nolow}) ->
				       {Acc, dehex (E)};
				   (E, {Acc, LO})  ->
				       {[dehex (E) * 16 + LO | Acc], nolow} end, {[], nolow}, Hex),
    String.


%%------------------------------------------------------------------------------
%% @doc Encode a string that can be passed as a bash command-line argument.
%%
%% <pre>
%%
%% Types:
%%  String = string()
%%
%% </pre>
%% @spec bash_encode(String) -> string()
%% @end
%%------------------------------------------------------------------------------

bash_encode ([         ]) -> [                            ];
bash_encode ([${ | Tail]) -> [$\\, ${ | bash_encode (Tail)];
bash_encode ([$} | Tail]) -> [$\\, $} | bash_encode (Tail)];
bash_encode ([$. | Tail]) -> [$\\, $. | bash_encode (Tail)];
bash_encode ([$' | Tail]) -> [$\\, $' | bash_encode (Tail)];
bash_encode ([ C | Tail]) -> [      C | bash_encode (Tail)].

%%------------------------------------------------------------------------------
%% @doc Encodes a binary as an ASCII hex string.
%%
%% @spec binary_to_hex (Binary) -> string()
%% @end
%%------------------------------------------------------------------------------

binary_to_hex (Binary) when is_binary (Binary) ->
    lists:flatten (lists:map (fun byte_to_hex/1, binary_to_list (Binary))).

%%------------------------------------------------------------------------------
%% Private Functions
%%------------------------------------------------------------------------------

dehex (H) when H >= $a, H =< $f -> H - $a + 10;
dehex (H) when H >= $A, H =< $F -> H - $A + 10;
dehex (H) when H >= $0, H =< $9 -> H - $0.

hexc (D) when D > 9 -> $a + D - 10;
hexc (D)            -> $0 + D.


