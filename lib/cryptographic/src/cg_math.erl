%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@sixfoe>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Mathematical functions needed for cryptograpic functions but not supplied by Erlang standard libs.
%%% @end
%%% Created : 25 Mar 2008 by Martin Logan <martinjlogan@sixfoe>
%%%-------------------------------------------------------------------
-module(cg_math).

%% API
-export([
	 prime/1, 
	 floor/1,
	 gcd/2,
	 small_coprime/1,
	 coprime/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns all prime numbers from the first prime number to N
%% @spec primes(N) -> [integer()]
%% @end
%%--------------------------------------------------------------------
prime(N) when N < 2 ->
    [];
prime(2) ->
    [2];
prime(N) ->
    S         = lists:seq(3, N, 2),
    MRoot     = math:sqrt(N),
    Half      = length(S),
    primeit(S, MRoot, Half).

primeit(S, MRoot, Half) ->
    primeit(3, 0, array:from_list(S, 0), MRoot, Half).

primeit(M, _I, S, MRoot, _Half) when M > MRoot ->
    [2|array:sparse_to_list(S)];
primeit(M, I, S, MRoot, Half) ->
    NewI = I + 1,
    case array:get(I, S) of
	0 ->
	    primeit(2 * NewI + 3, NewI, S, MRoot, Half);
	_Int ->
	    J    = floor((M * M - 3) / 2),
	    NS   = vacumeit(array:set(J, 0, S), M, J, Half),
	    primeit(2 * NewI + 3, NewI, NS, MRoot, Half)
    end.
	    
vacumeit(S, _M, J, Half) when J >= Half ->
    S;
vacumeit(S, M, J, Half) ->
    vacumeit(array:set(J, 0, S), M, J + M, Half).

%%--------------------------------------------------------------------
%% @doc Returns the highest integer less than or equal to the number N.
%% @spec primes(N) -> [integer()]
%% @end
%%--------------------------------------------------------------------
floor(N) ->
    case round(N) of
	RN when RN =< N -> RN;
	RN -> RN - 1
    end.
	    
%%--------------------------------------------------------------------
%% @doc Find the smallest coprime number less than N.
%% @spec small_coprime(N) -> integer()
%% @end
%%--------------------------------------------------------------------
small_coprime(N) ->    
    coprime(N, 2).    
			
%%--------------------------------------------------------------------
%% @doc Find a coprime number less than N and greater than E.
%% @spec coprime(N, E) -> integer()
%% @end
%%--------------------------------------------------------------------
coprime(N, E) ->    
    case gcd(N, E) of
	1 -> E;
	_ -> coprime(N, E + 1)
    end.

%%--------------------------------------------------------------------
%% @doc Greatest common divisor
%% @spec gcd(A, B) -> integer()
%% @end
%%--------------------------------------------------------------------
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

%%%===================================================================
%%% Internal functions
%%%===================================================================
