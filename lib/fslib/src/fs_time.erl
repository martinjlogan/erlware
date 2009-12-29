%%%-------------------------------------------------------------------
%%% File    : fs_time.erl
%%% Author  : Martin J. Logan <martin@erlware.com>
%%%
%%% @doc
%%%  Functions for manipulating time values.
%%% @end
%%%
%%% Created :  9 Nov 2002 by Martin J. Logan <martin@erlware.com>
%%%-------------------------------------------------------------------
-module(fs_time).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         epoch_to_sec/1, 
         epoch_to_milli/1, 
         epoch_to_micro/1, 

         hms_to_sec/1, 
         hms_to_milli/1, 
         hms_to_micro/1, 

         hms_diff/2, 
         epoch_diff/2, 

         hms_elapsed/2, 
         epoch_elapsed/2,

         time_triple_is_newest/2,
         date_time_is_newest/2,
         date_time_to_string/2,
         date_time_to_string/1,

         is_date/1,
         is_older_than/2,
         
         epochseconds_to_gregorianseconds/1,

         poll_until/4,
         poll_until/3
        ]).

%%====================================================================
%% External functions
%%====================================================================
%%------------------------------------------------------------------------------
%% @doc Converts the return of now() to seconds.
%% <pre>
%% Variables:
%%  EpochTime - The now() tuple to be converted.
%%
%% Types:
%%  EpochTime = {MegaSeconds, Seconds, MicroSeconds}
%%  MegaSeconds = Seconds = MicroSeconds = EpochSeconds = integer()
%% </pre>
%% @spec epoch_to_sec({MegaSec, Sec, MicroSec}) -> EpochSeconds
%% @end
%%------------------------------------------------------------------------------
epoch_to_sec({MegaSec, Sec, MicroSec}) ->
        1000000 * MegaSec + Sec.

%%------------------------------------------------------------------------------
%% @doc Converts the return of now to micro-seconds.
%% <pre>
%% Variables:
%%  EpochTime - The now() tuple to be converted.
%%
%% Types:
%%  EpochTime = {MegaSeconds, Seconds, MicroSeconds}
%%   MegaSeconds = Seconds = MicroSeconds = EpochMilliSeconds = integer()
%% </pre>
%% @spec epoch_to_milli({MegaSec, Sec, MicroSec}) -> EpochMilliSeconds
%% @end
%%------------------------------------------------------------------------------
epoch_to_milli({MegaSec, Sec, MicroSec}) ->
        1000000000 * MegaSec + 1000 * Sec.

%%------------------------------------------------------------------------------
%% @doc Converts the return of now to microseconds.
%% <pre>
%% Variables
%%  EpochTime - The now() tuple to be converted.
%%
%% Types:
%%  EpochTime = {MegaSeconds, Seconds, MicroSeconds}
%%   MegaSeconds = integer()
%%   Seconds = integer()
%%   MicroSeconds = integer()
%%   EpochMicroSeconds = integer()
%% </pre>
%% @spec epoch_to_micro({MegaSec, Sec, MicroSec}) -> EpochMicroSeconds
%% @end
%%------------------------------------------------------------------------------
epoch_to_micro({MegaSec, Sec, MicroSec}) ->
        1000000000000 * MegaSec + 1000000 * Sec + MicroSec.

      
%%------------------------------------------------------------------------------
%% Function: hms_to_micro/1 
%%
%% @doc  This function converts a time() tuple to microseconds.
%% <pre>
%% Variables
%%  Time - The time() tuple to be converted.
%%
%% Types:
%%  Time = {Hour, Minute, Second}
%%   Hour = integer()
%%   Minute = integer()
%%   Second = integer()
%%   MicroSeconds = integer()
%% </pre>
%% @spec hms_to_micro(Time) -> MicroSeconds
%% @end
%%------------------------------------------------------------------------------
hms_to_micro(Time) ->
        3600000000*element(1, Time) + 60000000*element(2, Time) +
                1000000*element(3, Time).

%%------------------------------------------------------------------------------
%% @doc  This function converts a time() tuple to milliseconds.
%% <pre>
%% Variables
%%  Time - The time() tuple to be converted.
%%
%% Types:
%%  Time = {Hour, Minute, Second}
%%   Hour = integer()
%%   Minute = integer()
%%   Second = integer()
%%   MiliSeconds = integer()
%% </pre>
%% @spec hms_to_milli(Time) -> MiliSeconds
%% @end
%%------------------------------------------------------------------------------
hms_to_milli(Time) ->
        3600000*element(1, Time) + 60000*element(2, Time) +
                1000*element(3, Time).

%%------------------------------------------------------------------------------
%% Function: hms_to_sec/1 
%%
%% @doc  This function converts a time() tuple to seconds.
%% <pre>
%% Variables
%%  Time - The time() tuple to be converted.
%%
%% Types:
%%  Time = {Hour, Minute, Second}
%%   Hour = integer()
%%   Minute = integer()
%%   Second = integer()
%%   Seconds = integer()
%% </pre>
%% @spec hms_to_sec(Time) -> Seconds
%% @end
%%------------------------------------------------------------------------------
hms_to_sec(Time) ->
        3600*element(1, Time) + 60*element(2, Time) + element(3, Time).

%%----------------------------------------------------------------------------
%% Function: hms_diff/2 
%% @doc  This function returns the difference in time between two time triples in the form of {hour, minute, second}.
%% <pre>
%% Note: If the initial timestamp is smaller that the later timestamp
%% it is assumed that the initial timestanp is from a previous date and 
%% it is adjusted by adding 24 hours to it. So if the time is from 2 days ago 
%% there is no way to tell and you only get a days differance.
%%
%% Variables
%%  Arg1: Then - The initial timestamp i.e the furthest past
%%  Arg2: Now -  The most recent timestamp
%%
%% Types:
%%  Hour = integer()
%%  Minute = integer()
%%  Second = integer()
%% </pre>
%% @spec hms_diff(Then, Now) -> {Hour, Minute, Second} | {error, negative_time}
%% @end
%%----------------------------------------------------------------------------
hms_diff({H1, M1, S1}, {H2, M2, S2}) when S2 - S1 < 0 ->
    hms_diff({H1, M1, S1}, {H2, M2 - 1, S2 + 60});

hms_diff({H1, M1, S1}, {H2, M2, S2}) when M2 - M1 < 0 ->
    hms_diff({H1, M1, S1}, {H2 - 1, M2 + 60, S2});

hms_diff({H1, M1, S1}, {H2, M2, S2}) when H2 - H1 < 0 ->
    hms_diff({H1, M1, S1}, {H2 + 24, M2, S2});

hms_diff({H1, M1, S1}, {H2, M2, S2}) ->
        {H2 - H1, M2 - M1, S2 - S1}.


%%----------------------------------------------------------------------------
%% @doc  This function returns the difference in time between two time triples in the form of {megasecs, secs, micro secs}.
%% <pre>
%% Variables
%%  EpochTimeThen - The earlyest now() tuple. 
%%  EpochTimeNow - The later now() tuple.
%%
%% Types:
%%  EpochTimeNow = integer()
%%  EpochTimeThen = integer()
%%  EpochTimeDiff = {MegaSeconds, Seconds, MicroSeconds}
%%   MegaSeconds = integer()
%%   Seconds = integer()
%%   MicroSeconds = integer()
%%  EpochTimeDiff = integer()
%% </pre>
%% @spec epoch_diff(EpochTimeThen, EpochTimeNow) -> EpochTimeDiff | {error, negative_time}
%% @end
%%----------------------------------------------------------------------------
epoch_diff({H1, M1, S1}, {H2, M2, S2}) when S2 - S1 < 0 ->
        epoch_diff({H1, M1, S1}, {H2, M2 - 1, S2 + 1000000});

epoch_diff({H1, M1, S1}, {H2, M2, S2}) when M2 - M1 < 0 ->
        epoch_diff({H1, M1, S1}, {H2 - 1, M2 + 1000000, S2});

epoch_diff({H1, M1, S1}, {H2, M2, S2}) when H2 - H1 < 0 ->
    {error, negative_time};

epoch_diff({H1, M1, S1}, {H2, M2, S2}) ->
        {H2 - H1, M2 - M1, S2 - S1}.


%%----------------------------------------------------------------------------
%% @doc  This finds the differance between two now() tuples and returns the differance in seconds.
%% <pre>
%% Variables
%%  TimeThen - The earlyest now() tuple. 
%%  TimeNow - The later now() tuple.
%%
%% Types:
%%  TimeNow = {Hours, Minutes, Seconds}
%%  TimeThen = {Hours, Minutes, Seconds}
%%   Hours = integer()
%%   Minutes = integer()
%%   Seconds = integer()
%%   TimeDiff = integer()
%% </pre>
%% @spec hms_elapsed(TimeThen, TimeNow) -> TimeDiff 
%% @end
%%----------------------------------------------------------------------------
hms_elapsed({H, M, S}, {H, M, S}) -> 0;

hms_elapsed({H, M, S2}, {H, M, S1}) ->
        S1 - S2;

hms_elapsed({H, M2, S2}, {H, M1, S1}) ->
        S1 - S2 + (60 * (M1 - M2));

hms_elapsed({H2, M2, S2}, {H1, M1, S1}) ->
        S1 - S2 + (60 * (M1 - M2)) + (3600 * (H1 - H2)).


%%----------------------------------------------------------------------------
%% @doc  Returns the number of microseconds elapsed between two times as returned by erlang:now() calls.
%% <pre>
%% Variables
%%  TimeThen - The earlyest time() tuple. 
%%  TimeNow - The later time() tuple.
%%
%% Types:
%%  TimeNow = {MegaSeconds, Seconds, MicroSeconds}
%%  TimeThen = {MegaSeconds, Seconds, MicroSeconds}
%% </pre>
%% @spec epoch_elapsed(TimeThen, TimeNow) -> integer()
%% @end
%%----------------------------------------------------------------------------
epoch_elapsed({MegaSecsThen, SecsThen, MicroSecsThen},
             {MegaSecsThen, SecsThen, MicroSecsThen}) ->
        0;

epoch_elapsed({MegaSecsThen, SecsThen, MicroSecsThen},
             {MegaSecsThen, SecsThen, MicroSecsNow}) ->
        MicroSecsNow - MicroSecsThen;

epoch_elapsed({MegaSecsThen, SecsThen, MicroSecsThen},
             {MegaSecsThen, SecsNow, MicroSecsNow}) ->
        MicroSecsThen - (MicroSecsNow + (1000000 * (SecsThen - SecsNow)));

epoch_elapsed({MegaSecsThen, SecsThen, MicroSecsThen},
             {MegaSecsNow, SecsNow, MicroSecsNow}) ->
        MicroSecsThen - 
        (MicroSecsNow + (1000000000000 * (MegaSecsThen - MegaSecsNow)) + 
         (1000000 * (SecsThen - SecsNow))).

%%----------------------------------------------------------------------------
%% @doc  Is the first datetime, DateTimeOne, newer than the second datetime, DateTimeTwo.
%% <pre>
%% Types:
%%  DateTimeOne = DateTimeTwo = {date(), time()}
%% </pre>
%% @spec date_time_is_newest(DateTimeOne, DateTimeTwo) -> bool()
%% @end
%%----------------------------------------------------------------------------
date_time_is_newest({Date, Time}, {Date, Time2}) -> 
    time_triple_is_newest(Time, Time2);
date_time_is_newest({Date, Time}, {Date2, Time2}) -> 
    time_triple_is_newest(Date, Date2).

%%----------------------------------------------------------------------------
%% @doc  Takes any time triple, date(), time(), now() and determines if the first triple is newer than the second. 
%% <pre>
%% Types:
%%  TripleOne = TripleTwo =  now() | date() | time()
%% </pre>
%% @spec time_triple_is_newest(TripleOne, TripleTwo) -> bool()
%% @end
%%----------------------------------------------------------------------------
time_triple_is_newest({Y, _, _}, {Y2, _, _}) when Y > Y2 -> true;
time_triple_is_newest({Y, _, _}, {Y2, _, _}) when Y < Y2 -> false;

time_triple_is_newest({_, M, _}, {_, M2, _}) when M > M2 -> true;
time_triple_is_newest({_, M, _}, {_, M2, _}) when M < M2 -> false;

time_triple_is_newest({_, _, D}, {_, _, D2}) when D > D2 -> true;
time_triple_is_newest({_, _, D}, {_, _, D2})             -> false.
    
    

%%----------------------------------------------------------------------------
%% @doc  This converts epoch seconds to gregorian time in seconds.
%% <pre>
%% Variables
%%  EpochSeconds - Seconds from the epoch.
%%
%% Types:
%%  EpochSeconds =  integer()
%%  GregorianSeconds = integer()
%% </pre>
%% @spec epochseconds_to_gregorianseconds(EpochSeconds) -> GregorianSeconds 
%% @end
%%----------------------------------------------------------------------------
epochseconds_to_gregorianseconds(EpochSeconds) -> 62167219200 + EpochSeconds.


%%--------------------------------------------------------------------
%% @doc This is a higher order function that allows for Iterations number of executions of Fun until Result is returned pausing for PauseMS after each execution.
%% <pre>
%% Variables:
%%  Fun - A fun to execute per iteration.
%%  Reply - If the fun returns this we return tr
%%  Iterations - The maximum number of iterations to try getting Reply out of Fun.  
%%  PauseMS - The number of miliseconds to wait inbetween each iteration.
%%  Return - What ever the fun returns.
%%
%% Types:
%%  Fun = fun()
%%  Reply = term()
%%  PauseMS = Iterations = integer()
%%  Return = term()
%% </pre>
%% @spec poll_until(Fun, Reply, Iterations, PauseMS) -> Return
%% @end
%%--------------------------------------------------------------------
poll_until(Fun, Reply, 0, PauseMS) ->
    Fun();
poll_until(Fun, Reply, Iterations, PauseMS) ->
    case Fun() of
        Reply -> 
            Reply;
        NotReply -> 
            timer:sleep(PauseMS),
            case Iterations of 
                infinity   -> poll_until(Fun, Reply, Iterations, PauseMS);
                Iterations -> poll_until(Fun, Reply, Iterations - 1, PauseMS)
            end
    end.
            
%% @spec poll_until(Fun, Iterations, PauseMS) -> Return
%% @equiv poll_until(Fun, true, Iterations, PauseMS)
poll_until (Fun, Iterations, PauseMS) ->
    poll_until (Fun, true, Iterations, PauseMS).

%%----------------------------------------------------------------------------
%% @doc Is this Timestamp older than X number of milliseconds.
%% <pre>
%% Variables:
%%  Timestamp - A timestamp to be checked for age. This is either a now() tuple or an integer() representing milliseconds since the epoch.
%%  Timelimit - an integer() containing a millisecond unit value. 
%%
%% Types:
%%  Timestamp = {MegaSeconds, Seconds, MicroSeconds} | MillisecondsSinceEpoch
%%  Timelimit = integer()
%%
%% Example:
%%  Is Timestamp older than ten seconds? 
%%   is_olderthan(now(), 10000) -> false
%% </pre>
%% @spec is_older_than(Timestamp, Timelimit) -> bool()
%% @end
%%----------------------------------------------------------------------------
is_older_than(Timestamp, Timelimit) when is_integer(Timestamp) -> 
    epoch_to_milli(now()) - Timelimit > Timestamp;
is_older_than({A, B, C} = Timestamp, Timelimit) when is_integer(A), is_integer(B), is_integer(C) -> 
    epoch_to_milli(now()) - Timelimit > epoch_to_milli(Timestamp).


%%----------------------------------------------------------------------------
%% @doc Is this a date. Day and month are checked but year is unchecked.
%% <pre>
%% Types:
%%  Date = {Year, Month, Day}
%%   Year = Month = Day = integer()
%% </pre>
%% @spec is_date(Date) -> bool()
%% @end
%%----------------------------------------------------------------------------
is_date({Year, 1,  Day}) when Day =< 31 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 2,  Day}) when Day =< 29 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 3,  Day}) when Day =< 31 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 4,  Day}) when Day =< 30 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 5,  Day}) when Day =< 31 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 6,  Day}) when Day =< 30 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 7,  Day}) when Day =< 31 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 8,  Day}) when Day =< 31 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 9,  Day}) when Day =< 30 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 10, Day}) when Day =< 31 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 11, Day}) when Day =< 30 , Day >= 1 , is_integer(Year) -> true;
is_date({Year, 12, Day}) when Day =< 31 , Day >= 1 , is_integer(Year) -> true;
is_date({_,    _,    _})                                                          -> false.
    

%%----------------------------------------------------------------------------
%% @doc  Convert a {date(), time()} tuple to a pretty human readable string.
%% <pre>
%% Types:
%%  Format = standard | syslog
%%  DateTime = {date(), time()}
%% 
%% Example:
%%  date_time_to_string(standard, {{2004,3,16}, {15,26,51}}) -> 5:26:51 pm on March 16, 2004
%% </pre>
%% @spec date_time_to_string(Format, DateTime) -> string()
%% @end
%%----------------------------------------------------------------------------
date_time_to_string(standard, {{Y, Mo, D}, {H, M, S}}) ->
    Year   = integer_to_list(Y),
    Month  = month_integer_to_name(Mo),
    Day    = integer_to_list(D),
    Minute = normalize_num_segment(integer_to_list(M), $0),
    Second = normalize_num_segment(integer_to_list(S), $0),

    {Hour, AMPM} = 
	case H of
	   H when H > 12 -> {integer_to_list(H - 12), "pm"};
	   12            -> {integer_to_list(H), "pm"};
	   H             -> {integer_to_list(H), "am"}
	end,
    Hour ++":" ++ Minute ++ ":" ++ Second ++ " " ++ AMPM ++ " on " ++ Month ++ " " ++ Day ++ ", " ++ Year;

date_time_to_string(syslog, {{_, Mo, D}, {H, M, S}}) ->
    Month  = month_integer_to_name(Mo),
    Day    = normalize_num_segment(integer_to_list(D), $ ),
    Hour   = normalize_num_segment(integer_to_list(H), $0),
    Minute = normalize_num_segment(integer_to_list(M), $0),
    Second = normalize_num_segment(integer_to_list(S), $0),

    Month ++ " " ++ Day ++ " " ++ Hour ++":" ++ Minute ++ ":" ++ Second ++ " ".

%% @spec date_time_to_string(DateTime) -> string()
%% @equiv date_time_to_string(standard, DateTime)
date_time_to_string({{Y, Mo, D}, {H, M, S}}) ->
    date_time_to_string(standard, {{Y, Mo, D}, {H, M, S}}).



month_integer_to_name(1)  -> "Jan";
month_integer_to_name(2)  -> "Feb";
month_integer_to_name(3)  -> "Mar";
month_integer_to_name(4)  -> "Apr";
month_integer_to_name(5)  -> "May";
month_integer_to_name(6)  -> "Jun";
month_integer_to_name(7)  -> "Jul";
month_integer_to_name(8)  -> "Aug";
month_integer_to_name(9)  -> "Sep";
month_integer_to_name(10) -> "Oct";
month_integer_to_name(11) -> "Nov";
month_integer_to_name(12) -> "Dec".

normalize_num_segment(NumString, Pad) when length(NumString) == 1 ->
    [Pad|NumString];
normalize_num_segment(NumString, Pad) ->
    NumString.
%%====================================================================
%% Internal functions
%%====================================================================















