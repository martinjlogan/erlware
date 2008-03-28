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
-export([
	 key_gen/1,
	 encrypt/3,
	 decrypt/3
	 ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generate rsa public and private keys
%% @spec key_gen(StartVal::integer()) -> {ok, {{public_key, {N, E}}, {private_key, {N, D}}}}
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
    
%%--------------------------------------------------------------------
%% @doc Encrypt a number. 
%% @spec encrypt(Msg, N, E) -> integer()
%% where
%%  Msg = integer()
%%  N = integer()
%%  E = integer()
%% @end
%%--------------------------------------------------------------------
encrypt(Msg, N, E) ->
    round(math:pow(Msg, E)) rem N. 
    
%%--------------------------------------------------------------------
%% @doc Decrypt a number. 
%% @spec encrypt(Msg, N, D) -> integer()
%% where
%%  Msg = integer()
%%  N = integer()
%%  D = integer()
%% @end
%%--------------------------------------------------------------------
decrypt(Msg, N, D) ->
    decrypt1(Msg, N, D) rem N. 

decrypt1(Msg, _N, 1) ->
    Msg;
decrypt1(Msg, N, D) ->
    io:format("Msg = ~p, N = ~p, D = ~p~n", [Msg, N, D]),
    case D rem 2 of
	0 ->
	    decrypt(round(math:pow(Msg, 2)) rem N, N, round(D/2));
	1 ->
	    Msg * decrypt(Msg, N, D - 1)
    end.
	    
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_test() ->
    %Code = cg_rsa:encrypt(4, 6097, 7).
    %?assertMatch(Code, cg_rsa:decrypt(4190, 6097, 4243)).
    ok.

