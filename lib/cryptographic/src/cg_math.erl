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
	 small_coprime/1
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns all prime numbers from 1 to N
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
    primeit(3, 1, array:from_list(S), MRoot, Half).

primeit(M, _I, S, MRoot, _Half) when M > MRoot ->
    [2|array:sparse_to_list(S)];
primeit(M, I, S, MRoot, Half) ->
    case array:get(I, S) of
	Undef when Undef == undefined; Undef == 0 ->
	    primeit(2 * I + 4, I + 1, S, MRoot, Half);
	_Int ->
	    J = floor((M * M - 3) / 2),
	    NS = vacumeit(array:set(J, undefined, S), M, J, Half),
	    primeit(2 * I + 4, I + 1, NS, MRoot, Half)
    end.
	    
vacumeit(S, _M, J, Half) when J >= Half ->
    S;
vacumeit(S, M, J, Half) ->
    vacumeit(array:set(J, undefined, S), M, J + M, Half).

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
%% @doc Find a random coprime number less than N.
%% @spec coprime_less_than(N) -> integer()
%% @end
%%--------------------------------------------------------------------
small_coprime(N) ->    
    small_coprime(N, 2).    
			
small_coprime(N, E) ->    
    case gcd(N, E) of
	1 -> E;
	_ -> small_coprime(N, E + 1)
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
