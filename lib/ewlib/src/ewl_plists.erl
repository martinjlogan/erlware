%%%-------------------------------------------------------------------
%%% @author Eric Merritt <>
%%% @copyright (C) 2011, Eric Merritt
%%% @doc
%%% simple parrallel map. Originally provided by Joe Armstrong
%%% on the erlang questions mailing list.
%%%
%%% Addition by Tristan: Added timeout to map and added filter
%%% with timeout.
%%%
%%% @end
%%% Created :  4 Jan 2011 by Eric Merritt <>
%%%-------------------------------------------------------------------
-module(plists).

-export([map/2,
         map/3,
         filter/2,
         filter/3]).

-include_lib("eunit/include/eunit.hrl").
%%=============================================================================
%% Public API
%%=============================================================================
%%--------------------------------------------------------------------
%% @doc Takes a function and produces a list of the result of the function
%%      applied to each element of the argument list. A timeout is optional.
%%      In the event of a timeout the resultant element is the atom timeout.
%%--------------------------------------------------------------------
-spec map(fun(), [any()]) -> [any()].
map(Fun, List) ->
    map(Fun, List, infinity).
-spec map(fun(), [any()], integer()) -> [any()].
map(Fun, List, Timeout) ->
    run_list_fun_in_parallel(map, Fun, List, Timeout).

%%--------------------------------------------------------------------
%% @doc Returns a list of the elements in the supplied list which
%%      the function Fun returns true. A timeout is optional. In the
%%      event of a timeout the element is treated as if it did not
%%      return true, and is thus not in the resultant list.
%%--------------------------------------------------------------------
-spec filter(fun(), [any()]) -> [any()].
filter(Fun, List) ->
    filter(Fun, List, infinity).
-spec filter(fun(), [any()], integer()) -> [any()].
filter(Fun, List, Timeout) ->
    run_list_fun_in_parallel(filter, Fun, List, Timeout).

%%=============================================================================
%% Internal API
%%=============================================================================
-spec run_list_fun_in_parallel(atom(), fun(), [any()], integer()) -> [any()].
run_list_fun_in_parallel(ListFun, Fun, List, Timeout) ->
    LocalPid = self(),
    Pids = lists:map(fun(I) ->
                           spawn(fun() ->
                                         wait(LocalPid, Fun, I, Timeout)
                                 end)
                   end, List),
    gather(ListFun, Pids).

-spec wait(pid(), fun(), any(), integer()) -> any().
wait(Parent, Fun, I, Timeout) ->
    WaitPid = self(),
    Child = spawn(fun() ->
                          do_f(WaitPid, Fun, I)
                  end),

    wait(Parent, Child, Timeout).

-spec wait(pid(), pid(), integer()) -> any().
wait(Parent, Child, Timeout) ->
    receive
        {Child, Ret} ->
            Parent ! {self(), Ret}
    after Timeout ->
            exit(Child, timeout),
            Parent ! {self(), timeout}
    end.

-spec gather(atom(), [any()]) -> [any()].
gather(map, PidList) ->
    map_gather(PidList);
gather(filter, PidList) ->
    filter_gather(PidList).

-spec map_gather([pid()]) -> [any()].
map_gather([Pid | Rest]) ->
    receive
        {Pid, Ret} -> [Ret|map_gather(Rest)]
    end;
map_gather([]) ->
    [].

-spec filter_gather([pid()]) -> [any()].
filter_gather([Pid | Rest]) ->
    receive
        {Pid, false} -> filter_gather(Rest);
        {Pid, timeout} -> filter_gather(Rest);
        {Pid, Ret} -> [Ret|filter_gather(Rest)]
    end;
filter_gather([]) ->
    [].

-spec do_f(pid(), fun(), any())  -> none().
do_f(Parent, F, I) ->
    try
        Result = F(I),
        Parent ! {self(), Result}
    catch
        ErrType:Error ->
            Parent ! {self(), {error, ErrType, Error}}
    end.

%%=============================================================================
%% Tests
%%=============================================================================
pmap_good_test() ->
    Results = map(fun(_) ->
                          ok
                  end,
                  lists:seq(1, 5), infinity),
    ?assertMatch([ok, ok, ok, ok, ok],
                 Results).

pfilter_good_test() ->
    Results = filter(fun(X) ->
                             X
                     end,
                     [true, false, true], infinity),
    ?assertMatch([true, true],
                 Results).

pfilter_timeout_test() ->
    Results = filter(fun(X) ->
                             timer:sleep(X),
                             true
                     end,
                     [100, 1], 10),
    ?assertMatch([true],
                 Results).

pmap_bad_test() ->
    Results = map(fun(_) ->
                          erlang:throw(test_exception)
                  end,
                  lists:seq(1, 5), infinity),
    ?assertMatch([{error, throw, test_exception},
                  {error, throw, test_exception},
                  {error, throw, test_exception},
                  {error, throw, test_exception},
                  {error, throw, test_exception}],
                 Results).
