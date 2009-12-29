%%%-------------------------------------------------------------------
%%% Author  : Jeffery Einhorn
%%%
%%% @doc
%%%  Functions to simplify working with fprof. 
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(fs_fprof_util).
                                                                                
%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	    take_sample/2,	    
	    take_n_samples/3,
	    take_n_samples/4,
	    test_take_sample/0,
	    analyse_all_samples/0,
	    create_analyse/2
	]).

%%--------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Example usage of the take_sample function. Writes to Sample.fprof
%%
%% @spec test_take_sample () -> Anything
%% @end
%%--------------------------------------------------------------------
test_take_sample () ->
    take_sample ("Sample.fprof", [self()]).

%%--------------------------------------------------------------------
%% @doc Given a filename and a pidlist profile the current process
%%      so far it profiles for 5 seconds.
%%
%% @spec take_sample (Filename, PidList) -> Anything
%% @end
%%--------------------------------------------------------------------
take_sample (Filename, PidList) ->
    fprof:trace([start, {file, Filename}, {procs, PidList}]),
    timer:sleep(5000),
    fprof:trace(stop).

%%--------------------------------------------------------------------
%% @doc Take serveral samples with a specified wait period between
%%      each sample and a list of pids to profile.
%%
%% @spec take_n_samples (0, PeriodToWaitBetweenSamples, PidList) -> Anything
%% @end
%%--------------------------------------------------------------------
take_n_samples (0, PeriodToWaitBetweenSamples, PidList) ->
    io:format ("Done with all samples.~n"),
    ok;

%% @spec take_n_samples (SampleNumber, PeriodToWaitBetweenSamples, PidList) -> Anything
%% @equiv take_n_samples (0, PeriodToWaitBetweenSamples, PidList) 
take_n_samples (SampleNumber, PeriodToWaitBetweenSamples, PidList) ->
    take_n_samples (SampleNumber, PeriodToWaitBetweenSamples, PidList, "test").

%% @spec take_n_samples (SampleNumber, PeriodToWaitBetweenSamples, PidList, BaseFilename) -> Anything
%% @equiv take_n_samples (SampleNumber, PeriodToWaitBetweenSamples, PidList) 
take_n_samples (SampleNumber, PeriodToWaitBetweenSamples, PidList, BaseFilename) ->
    io:format ("Executing sample: ~p~n", [SampleNumber]),
    take_sample(BaseFilename ++get_datetime_string() ++".fprof", PidList),
    timer:sleep(PeriodToWaitBetweenSamples),
    take_n_samples (SampleNumber - 1, PeriodToWaitBetweenSamples, PidList).

%%--------------------------------------------------------------------
%% @doc Utility function that looks for all files that end in .fprof and 
%%      creates an fprof analyse from them.
%%
%% @spec analyse_all_samples () -> Anything
%% @end
%%--------------------------------------------------------------------
analyse_all_samples () ->
    %% Get list of files and replace the fprof ending with analyse.
    lists:foreach(fun(ProfileFilename) -> 
    			create_analyse(ProfileFilename, get_analyse_filename(ProfileFilename)) end, 
			filelib:wildcard("*.fprof")).

%%--------------------------------------------------------------------
%% @doc Give a raw profile data file creates an analyse file with the 
%%      specified filename.
%%
%% @spec create_analyse(ProfileFilename, AnalyseFilename) -> Anything
%% @end
%%--------------------------------------------------------------------
create_analyse(ProfileFilename, AnalyseFilename) ->
    fprof:profile([{file, ProfileFilename}]),
    fprof:analyse([{dest, AnalyseFilename}]),
    io:format ("Analyse ~p created.~n",[AnalyseFilename]).



%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

get_analyse_filename (ProfileFilename) ->
    [Base | _] = string:tokens (ProfileFilename, "."),
    Base ++ ".analyse".

get_datetime_string() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    integer_to_list(Year) ++ integer_to_list(Month) ++ integer_to_list(Day) ++ "_" ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second).
    
