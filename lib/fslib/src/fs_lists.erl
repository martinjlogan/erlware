%%%-------------------------------------------------------------------
%%% File    : fs_lists.erl
%%% Author  : Martin J. Logan <martin@localhost.localdomain>
%%%
%%% @doc
%%%  Functions to manipulate lists.
%%% @end
%%%
%%% Created :  9 Nov 2002 by Martin J. Logan <martin@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(fs_lists).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 lastn/2,
	 index/2, 
	 t_qsort/2, 
         find_smallest/2, 
         find_largest/2, 
         find_all_smallest/2, 
         find_all_largest/2, 
         do_until/3, 
         do_until/2, 
         separate/2,
         index_tokens/3, 
         separate_by_token/2,
         non_destructive_separate_by_token/2,
         is_string/1, 
	 make_list_if_not/1,
         flatten_term/1,
	 get_val/3,
	 get_val/2,
         keysearch/4,
         keysearch/3
        ]).

%%--------------------------------------------------------------------
%% Include Files
%%--------------------------------------------------------------------
%-include("eunit.hrl").

%%====================================================================
%% External functions
%%====================================================================

%%----------------------------------------------------------------------------
%% @doc Returns the last N elements of List.
%% <pre>
%% Return a list made up of the last N elements from the input list.
%% If N is less or equal to zero, return an empty list.
%% If N is greater than the number of elements in the input list, return 
%% the entire input list.
%% </pre>
%% @spec lastn(N, List) -> List
%% @end
%%----------------------------------------------------------------------------
lastn(N, List) ->
    LenList = length (List),
    if N < 1 -> [];
       N > LenList -> List;
       true -> lists:nthtail (LenList-N, List)
    end.
 
%%----------------------------------------------------------------------------
%% @doc Returns the index of the Element in List.
%% <pre>
%% Note* C/Java programmers the first element in a list is 1 not 0 as in a C 
%% style array 
%% A return value of 0 indicates that the element is not present in the list.
%%
%% Variables:
%%  Element - The element in the list that the index is to be determined for.
%%
%% Types:
%%  Element = term()
%% </pre>
%% @spec index(Element, list()) -> integer()
%% @end
%%----------------------------------------------------------------------------
index(Element, List) -> index(Element, List, 1).

index(Element, [Element|T], Count) -> Count;
index(Element, [_|T], Count)       -> index(Element, T, Count + 1);
index(Element, [], Count)          -> 0.
    

%%----------------------------------------------------------------------------
%% @doc Applies a fun to all elements of a list until getting Return.
%% <pre>
%% This takes a fun, its expected return on 'success', and a list. The fun 
%% is applied to each element of the list untill the expected return is 
%% generated. If the expected return is generated from the application of 
%% the fun on an element of the list do_until halts and returns with Return. 
%% If not then the return value from the Fun being applied to the final 
%% element in the list is returned. If the list supplied is
%% empty then the return will be false. 
%% </pre>
%% @spec do_until(Fun, Return, list()) -> Return | Other | false
%% @end
%%----------------------------------------------------------------------------
do_until(_F, _, []) ->
    false;
do_until(F, _, [Last]) ->
    F(Last);
do_until(F, Return, [H|T]) ->
    case F(H) of
	Return -> Return;
	_      -> do_until(F, Return, T)
    end.
    
%% @spec do_until(Fun, list()) -> bool()
%% @equiv do_until(Fun, true, list())
do_until(F, List) ->
    do_until(F, true, List).

             
%%--------------------------------------------------------------------
%% @doc Separates two lists.
%% <pre>
%% This behaves as if it were defined in the following way:
%%
%%  lists:filter(Fun, List), lists:filter(not Fun, List)}.
%%
%% Variables:
%%  Fun - a fun() that returns bool()
%%  List - The input list
%%
%% Types:
%%  Fun = fun() 
%%  List = List1 = List2 = list()
%% </pre>
%% @spec
%%  separate(Fun, List) -> {List1, List2}
%% @end
%%--------------------------------------------------------------------
separate(Fun, List) ->
    separate(Fun, List, {[], []}).

separate(Fun, [H|T], Acc) ->  
    separate(Fun, T, t_or_f(Fun(H), H, Acc));
separate(Fun, [], Acc) -> Acc.

t_or_f(true, H, {TL, FL})  -> {[H|TL], FL}; 
t_or_f(false, H, {TL, FL}) -> {TL, [H|FL]}.


%%--------------------------------------------------------------------------
%% @doc A quick sort variant that sorts tuples on a particular element.
%% <pre>
%%  Right now it uses a naive pivot selection that degenerates into the
%%  worst case it the list is sorted allready.
%% </pre>
%% @spec t_qsort(list(), Element) -> list()
%% @end
%%--------------------------------------------------------------------------
t_qsort([], _) -> [];
t_qsort(List, Element) ->
    t_qsort(List, [], Element).

t_qsort([Pivot|Rest], Tail, Element) ->
    {Smaller, Bigger} = split(Pivot, Rest, Element),
    t_qsort(Smaller, [Pivot|t_qsort(Bigger, Tail, Element)]).

split(Pivot, List, Element) ->
    split(Pivot, List, [], [], Element).

split(Pivot, [], Smaller, Bigger, Element) ->
    {Smaller, Bigger};
split(Pivot, [H|T], Smaller, Bigger, Element) when element(H, Element) < Pivot ->
    split(Pivot, T, [H|Smaller], Bigger, Element);
split(Pivot, [H|T], Smaller, Bigger, Element) when element(H, Element) > Pivot ->
    split(Pivot, T, Smaller, [H|Bigger], Element).



%%-----------------------------------------------------------------------
%% @doc Finds the tuple with the smallest element and the position 
%%      specified by element.
%% <pre>
%% Variables:
%%  TupleList - The list of tuples to examine
%%  Element - The element to be compared
%%  Smallest - A tuple from input or an empty list if thats what was passed in.
%%
%% Types:
%%  TupleList = [tuple()]
%%  Element = integer()
%%  Smallest = tuple() | []
%% </pre>
%% @spec find_smallest(TupleList, Element) -> Smallest
%% @end
%%-----------------------------------------------------------------------
find_smallest([], _) ->
    [];
find_smallest([H|T], Element) ->
    find_smallest(T, H, Element).

find_smallest([], Smallest, Element) ->
    Smallest;
find_smallest([H|T], Smallest, Element) when element(Element, H) < element(Element, Smallest) ->
    find_smallest(T, H, Element);
find_smallest([H|T], Smallest, Element) ->
    find_smallest(T, Smallest, Element).

%%-----------------------------------------------------------------------
%% @doc Finds the tuple with the largest element in the position 
%%      specified by element.
%% <pre>
%% Variables:
%%  TupleList - The list of tuples to examine
%%  Element - The element to be compared
%%  Largest - A tuple from input or an empty list if thats what  was passed in.
%%
%% Types:
%%  TupleList = [tuple()]
%%  Element = integer()
%%  Largest = tuple() | []
%% </pre>
%% @spec find_largest(TupleList, Element) -> Largest
%% @end
%%-----------------------------------------------------------------------
find_largest([], _) ->
    [];
find_largest([H|T], Element) ->
    find_largest(T, H, Element).

find_largest([], Largest, Element) ->
    Largest;
find_largest([H|T], Largest, Element) when element(Element, H) 
                                          > element(Element, Largest) ->
    find_largest(T, H, Element);
find_largest([H|T], Largest, Element) ->
    find_largest(T, Largest, Element).



%%-----------------------------------------------------------------------
%% @doc Finds all the tuples with the smallest element in the position 
%%      specified by element.
%% <pre>
%% Variables:
%%  TupleList - The list of tuples to examine
%%  Element - The element to be compared
%%  Smallest - A tuple from input or an empty list if thats what was passed in.
%%
%% Types:
%%  TupleList = [tuple()]
%%  Element = integer()
%%  Smallest = [tuple()] | []
%% </pre>
%% @spec find_all_smallest(TupleList, Element) -> Smallest
%% @end
%%-----------------------------------------------------------------------
find_all_smallest([], _) ->
    [];
find_all_smallest([H|T], Element) ->
    find_all_smallest(T, [H], Element).

find_all_smallest([], Smallest, Element) ->
    Smallest;
find_all_smallest([H|T], Smallest, Element) when element(Element, H) =:= element(Element, hd(Smallest)) ->
    find_all_smallest(T, [H|Smallest], Element);
find_all_smallest([H|T], Smallest, Element) when element(Element, H) < element(Element, hd(Smallest)) ->
    find_all_smallest(T, [H], Element);
find_all_smallest([H|T], Smallest, Element) ->
    find_all_smallest(T, Smallest, Element).



%%-----------------------------------------------------------------------
%% @doc Finds all the tuples with the largest element in the position specified by element.
%% <pre>
%% Variables:
%%  TupleList - The list of tuples to examine
%%  Element - The element to be compared
%%  Smallest - A tuple from input or an empty list if thats what was passed in.
%%
%% Types:
%%  TupleList = [tuple()]
%%  Element = integer()
%%  Largest = [tuple()] | []
%% </pre>
%% @spec find_all_largest(TupleList, Element) -> Largest
%% @end
%%-----------------------------------------------------------------------
find_all_largest([], _) ->
    [];
find_all_largest([H|T], Element) ->
    find_all_largest(T, [H], Element).

find_all_largest([], Largest, Element) ->
    Largest;
find_all_largest([H|T], Largest, Element) when element(Element, H) =:= element(Element, hd(Largest)) ->
    find_all_largest(T, [H|Largest], Element);
find_all_largest([H|T], Largest, Element) when element(Element, H) > element(Element, hd(Largest)) ->
    find_all_largest(T, [H], Element);
find_all_largest([H|T], Largest, Element) ->
    find_all_largest(T, Largest, Element).


%%------------------------------------------------------------------------------
%% @doc Grabs the Index number of words from a string.
%% <pre>
%% Tokens is a list of charachters i.e " |@". This function traverses a
%% string accumulating words delimeted by one of the tokens for
%% a number of iterations specified by index.
%%
%% Expects:
%%  String - The string to be separated.
%%  Index - The number of words to grab.
%%  Tokens - A list of tokens to be separated on.
%%
%% Types:
%%  String = string()
%%  Index - integer()
%%  Tokens = list()
%% </pre>
%% @spec index_tokens(String, Index, Tokens) -> {ok, {Token, Rest}}
%% @end
%%------------------------------------------------------------------------------
index_tokens(String, Index, Tokens) ->
    index_tokens(String, Index, Tokens, []).
                                                                                   
index_tokens(String, 0, Tokens, Tokenized) ->
    {ok, {lists:reverse(Tokenized), String}};
index_tokens(String, Index, Tokens, Tokenized) ->
    case separate_by_token(String, Tokens, []) of
        {ok, {[], Rest}} ->
            {ok, {Tokenized, Rest}};
        {ok, {NewTokenized, Rest}} ->
            index_tokens(Rest, Index -1, Tokens, [NewTokenized|Tokenized])
    end.
                                                                                   

%%-----------------------------------------------------------------------------
%% @doc Grabs the first word from a string that is delimeted by a Token.
%% <pre>
%% Tokens is a list of charachters i.e " |@". This function traverses a
%% string accumulating elements until it reaches one of the tokens.
%% Upon reaching a token it returns the accumulated elements and
%% the rest of the string.
%%
%% Expects:
%%  String - The string to be separated.
%%  Tokens - A list of tokens to be separated on.
%%
%% Types:
%%  String = string()
%%  Tokens = list()
%% </pre>
%% @spec separate_by_token(String, Tokens) -> {ok, {Token, Rest}}
%% @end
%%-----------------------------------------------------------------------------
separate_by_token(String, Tokens) ->
    separate_by_token(String, Tokens, []).
                                                                                   
separate_by_token([H|T], Tokens, Word) ->
    case is_token(H, Tokens) of
        true  -> {ok, {lists:reverse(Word), T}};
        false -> separate_by_token(T, Tokens, [H|Word])
    end;
separate_by_token([], Tokens, Word) ->
    {ok, {[], lists:reverse(Word)}}.
                                                                                   

%%------------------------------------------------------------------------------
%% @doc Grabs the first word from a string that is delimeted by a Token 
%%      but leaves the token on the rest of the string.
%% <pre>
%% Tokens is a list of charachters i.e " |@". This function traverses a
%% string accumulating elements until it reaches one of the tokens.
%% Upon reaching a token it returns the accumulated elements and
%% the rest of the string.
%%
%% Expects:
%%  String - The string to be separated.
%%  Tokens - A list of tokens to be separated on.
%%
%% Types:
%%  String = string()
%%  Tokens = list()
%% </pre>
%% @spec non_destructive_separate_by_token(String, Tokens) -> {ok, {Token, Rest}}
%% @end
%%------------------------------------------------------------------------------
non_destructive_separate_by_token(String, Tokens) ->
    non_destructive_separate_by_token(String, Tokens, []).
                                                                                   
non_destructive_separate_by_token([H|T] = Rest, Tokens, Word) ->
    case is_token(H, Tokens) of
        true  -> {ok, {lists:reverse(Word), Rest}};
        false -> non_destructive_separate_by_token(T, Tokens, [H|Word])
    end;
non_destructive_separate_by_token([], Tokens, Word) ->
    {ok, {[], lists:reverse(Word)}}.
                                                                                   
%%----------------------------------------------------------------------------
%% @doc Flattens a complex term into a single string.
%% Note* References are not handled they will appear as reference
%% @spec flatten_term(term()) -> string()
%% @end
%%----------------------------------------------------------------------------
flatten_term(Terms) ->
    lists:concat(flattening(Terms)).
                                                                                   
flattening(Atom) when is_atom(Atom) ->
    [atom_to_list(Atom)];
flattening(Float) when is_float(Float) ->
    [float_to_list(Float)];
flattening(Integer) when is_integer(Integer) ->
    [integer_to_list(Integer)];
flattening(Binary) when binary(Binary) ->
    [binary_to_list(Binary)];
flattening(Pid) when pid(Pid) ->
    [pid_to_list(Pid)];
flattening(Ref) when is_reference(Ref) ->
    ["<<reference>>"];
flattening(Tuple) when tuple(Tuple) ->
    Terms = tuple_to_list(Tuple),
    Fun   = fun(Term, Acc) -> acc_check(Term, Acc) end,
    ["{"] ++ lists:foldl(Fun, [], Terms) ++ ["}"];
flattening(Terms) when list(Terms) ->
    case is_string(Terms) of
        true  -> 
            ["\"" ++ Terms ++ "\""];
        false ->
            Fun = fun(Term, Acc) -> acc_check(Term, Acc) end,
            ["["] ++ lists:foldl(Fun, [], Terms) ++ ["]"]
    end.
                                                                                   
acc_check(Term, [])  -> flattening(Term);
acc_check(Term, Acc) -> Acc ++ [", "] ++ flattening(Term).


%%----------------------------------------------------------------------------
%% @doc If the argument is not a list takes the single element and wraps
%%      it in a list. This applies to strings as well. If your list is a string
%%      it will come out wrapped in a list.  "hello" = ["hello"]
%% @spec make_list_if_not(Arg) -> list()
%% where Arg = term()
%% @end
%%----------------------------------------------------------------------------
make_list_if_not(Element) when not is_list(Element) ->
    [Element];
make_list_if_not([H|_] = Element) when is_integer(H), H >= $\s, H < 255 ->
    [Element];
make_list_if_not(List) ->
    List.


%%----------------------------------------------------------------------------
%% @doc Checks to see if a list is a string.
%% @spec is_string(List) -> bool()
%% @end
%%----------------------------------------------------------------------------
is_string([])   -> false;
is_string(Term) -> lists:any(fun(Element) -> string_p1(Element) end, Term).
                                                                                   
string_p1(H) when integer(H), H >= $\s, H < 255 -> true;
string_p1($\n)                                  -> true;
string_p1($\r)                                  -> true;
string_p1($\t)                                  -> true;
string_p1($\v)                                  -> true;
string_p1($\b)                                  -> true;
string_p1($\f)                                  -> true;
string_p1($\e)                                  -> true;
string_p1(_)                                    -> false.

%%----------------------------------------------------------------------------
%% @doc look for a tuple in a list with a Key at position 1 and a Value at position 2. 
%%  If it is found then return the value. If not return the DefaultValue.
%% <pre>
%% Variables:
%%  TwoTupleList - A list of tuples and terms() to be searched for the key Key
%%  DefaultValue - Returned if the key does not exist in the TwoTupleList.
%%
%% Types:
%%  TwoTupleList = [Element]
%%   Element = {Key, Value} | term()
%%    Key = Value = DefaultValue = term()
%% </pre>
%% @spec get_val(Key, TwoTupleList, DefaultValue) -> Value | DefaultValue
%% @end
%%----------------------------------------------------------------------------
get_val(Key, [{Key, Value}|_], DefaultValue) -> Value;
get_val(Key, [_|T],            DefaultValue) -> get_val(Key, T, DefaultValue);
get_val(Key, [],               DefaultValue) -> DefaultValue.

%% @spec get_val(Key, TwoTupleList) -> Value | DefaultValue
%% @equiv get_val(Key, TwoTupleList, undefined)
get_val(Key, TwoTupleList) -> get_val(Key, TwoTupleList, undefined).


%%----------------------------------------------------------------------------
%% @doc This function behaves much like lists:keysearch/3 but allows for a default value to be supplied if the key is not found.
%% <pre>
%% Example:
%%  key_search(hello, 1, undefined, [wert, {hello, world, hello}]) -> {hello, world, hello}
%%  key_search(martin, 1, undefined, [wert, {hello, world}]) -> undefined
%%
%% Variables:
%%  Key - The key to search for at tuple position index Element.
%%  Element - The location in the tuple to look for Key.
%%  ObjectList - A list of terms() to be searched for a tuple containing Key 
%%  DefaultObject - Returned as the object if the key does not exist in the TwoTupleList.
%%
%% Types:
%%  ObjectList = [term()]
%%  Element = integer()
%%  Key = Value = DefaultObject = term()
%% </pre>
%% @spec keysearch(Key, Element, ObjectList, DefaultObject) -> Object | DefaultObject
%% @end
%%----------------------------------------------------------------------------
keysearch(Key, Element, [Object|T], DefaultObject) when is_tuple(Object), size(Object) >= Element -> 
    case element(Element, Object) of
        Key -> Object;
        _   -> keysearch(Key, Element, T, DefaultObject)
    end;
keysearch(Key, Element, [Object|T], DefaultObject) -> 
    keysearch(Key, Element, T, DefaultObject);
keysearch(Key, Element, [], DefaultObject) -> 
    DefaultObject.

%% @spec keysearch(Key, Element, ObjectList) -> Value | DefaultObject
%% @equiv keysearch(Key,  Element, ObjectList, undefined)
keysearch(Key, Element, ObjectList) -> keysearch(Key, Element, ObjectList, undefined).


%%====================================================================
%% Internal Functions
%%====================================================================

is_token(Target, [Target|T]) -> true;
is_token(Target, [H|T])      -> is_token(Target, T);
is_token(Target, [])         -> false.

%%====================================================================
%% Test Functions
%%====================================================================
%do_util_test() ->
    %?assertMatch(true, do_until(fun(E) -> E == 3 end, true, [1,2,3,4,5])),
    %?assertMatch(false, do_until(fun(E) -> E == 6 end, true, [1,2,3,4,5])),
    %?assertMatch(true, do_until(fun(E) -> E == 3 end, [1,2,3,4,5])),
    %?assertMatch(false, do_until(fun(E) -> E == 6 end, [1,2,3,4,5])).
