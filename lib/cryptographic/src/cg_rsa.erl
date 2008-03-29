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
	 padded_encrypt/3,
	 decrypt/3,
	 padded_decrypt/3
	 ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generate rsa public and private keys
%% @spec key_gen(StartVal::integer()) -> {ok, {{public_key, {N, E}}, {private_key, {N, D}}, {max_message_size, Bytes}}}
%% @end
%%--------------------------------------------------------------------
key_gen(StartVal) ->
    %% @todo XXX this is in place to get rid of small primes which lead to low max message size. Makes the prime selection
    %%       more predicable though and thus the algorithm less secure.  How do others handle this?
    AllPrimes = cg_math:prime(StartVal),
    Primes    = lists:nthtail(round(length(AllPrimes) / 2), AllPrimes),
    P = lists:nth(random:uniform(length(Primes)), Primes),
    Q = lists:nth(random:uniform(length(Primes)), Primes),
    N = P * Q,
    % Compute the Eulers Totient of two primes
    TN = (P - 1) * (Q - 1),
    E = larger_coprime(TN),
    D = find_congruency(E, TN),
    {ok, {{public_key, {N, E}}, {private_key, {N, D}}, {max_message_size, lists:min([P, Q])}}}.

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
    apply_key(Msg, N, E).

%%--------------------------------------------------------------------
%% @doc Encrypt a number with padding. 
%% @spec padded_encrypt(Msg, N, E) -> integer()
%% where
%%  Msg = integer()
%%  N = integer()
%%  E = integer()
%% @end
%%--------------------------------------------------------------------
padded_encrypt(RawMsg, N, E) ->
    Pad = 9 + random:uniform(90),
    Msg = list_to_integer(lists:flatten([integer_to_list(RawMsg), integer_to_list(Pad)])),
    apply_key(Msg, N, E).
    
%%--------------------------------------------------------------------
%% @doc Decrypt a number. 
%% @spec decrypt(Msg, N, D) -> integer()
%% where
%%  Msg = integer()
%%  N = integer()
%%  D = integer()
%% @end
%%--------------------------------------------------------------------
decrypt(Msg, N, D) ->
    apply_key(Msg, N, D). 

%%--------------------------------------------------------------------
%% @doc Decrypt a number that has been padded. 
%% @spec padded_decrypt(Msg, N, D) -> integer()
%% where
%%  Msg = integer()
%%  N = integer()
%%  D = integer()
%% @end
%%--------------------------------------------------------------------
padded_decrypt(Msg, N, D) ->
    PaddingLength = 2,
    list_to_integer(
      lists:reverse(
	lop_off(
	  lists:reverse(integer_to_list(apply_key(Msg, N, D))),
	  PaddingLength
	 ))).

	    
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

%% @private
%% @todo make this function more strategic in terms of where bounds are set. Also research to see if the fact that 5
%%       is predictable when starting at 2 is a problem in terms of the overall algorithm - perhaps this is not
%%       needed.  
%% @doc Find a random coprime number less than N.
%% @spec coprime_less_than(N) -> integer()
%% @end
larger_coprime(N) when N < 100 ->    
    cg_math:small_coprime(N);
larger_coprime(N) ->    
    cg_math:coprime(N, 20).    
			

lop_off(List, 0) ->
    List;
lop_off([_|T], Count) ->
    lop_off(T, Count - 1).

%% @private
%% @doc Apply an RSA key pair to a value. 
%% @spec apply_key(Msg, N, D) -> integer()
%% where
%%  Msg = integer()
%%  N = integer()
%%  D = integer()
%% @end
apply_key(Msg, N, D) ->
    apply_key1(Msg, N, D) rem N.

apply_key1(Msg, _N, 1) ->
    Msg;
apply_key1(Msg, N, D) ->
    case D rem 2 of
	0 -> apply_key1(round(math:pow(Msg, 2)) rem N, N, round(D/2));
	1 -> Msg * apply_key1(Msg, N, D - 1)
    end.

%%%===================================================================
%%% Test functions
%%%===================================================================
%decrypt_test() ->
    %Code = cg_rsa:encrypt(4, 6097, 7).
    %?assertMatch(4, cg_rsa:decrypt(Code, 6097, 4243)).

%full_padded_rsa_test() ->
%    {ok, {{_, {N, E}}, {_, {N, D}}, _}} = cg_rsa:key_gen(1000),
%    Code = cg_rsa:padded_encrypt(1, N, E),
%    ?assertMatch(1, cg_rsa:padded_decrypt(Code, N, D)).


%full_rsa_test() ->
%    {ok, {{_, {N, E}}, {_, {N, D}}, _}} = cg_rsa:key_gen(500),
%    Code = cg_rsa:encrypt(1, N, E),
%    ?assertMatch(1, cg_rsa:decrypt(Code, N, D)).
