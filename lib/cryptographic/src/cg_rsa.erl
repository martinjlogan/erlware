%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Erlware
%%% @doc
%%%  A pure Erlang version of the RSA public key cryptography algorithm.
%%% @end
%%% Created : 25 Mar 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(cg_rsa).

%% API
-export([key_gen/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generate rsa public and private keys
%% @spec key_gen(StartVal) -> {ok, {{public_key, {N, E}}, {private_key, {N, D}}}}
%% @end
%%--------------------------------------------------------------------
key_gen(StartVal) ->
    Primes = cg_math:prime(StartVal),
    P = lists:nth(random:uniform(length(Primes)), Primes),
    Q = lists:nth(random:uniform(length(Primes)), Primes),
    io:format("P ~p~nQ ~p~n", [P, Q]),
    N = P * Q,
    % Compute the Eulers Totient of two primes
    TN = (P - 1) * (Q - 1),
    io:format("Eulers Totient; TN is ~p~n", [TN]),
    E = cg_math:small_coprime(TN),
    D = find_congruency(E, TN),
    io:format("E = ~p~n D = ~p~n", [E, D]),
    {ok, {{public_key, {N, E}}, {private_key, {N, D}}}}.
    

	    
%%%===================================================================
%%% Internal functions
%%%===================================================================

find_congruency(E, TN) ->
    find_congruency(E, TN, 1). 

find_congruency(E, TN, D) ->
    case (D * E - 1) rem TN of
	0 -> D;
	_ -> find_congruency(E, TN, D + 1)
    end.
