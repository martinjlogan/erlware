%% Useful project-wide macros.
%%

-import (error_logger).


-define (CURRENT_FUNCTION, tuple_to_list (hd (tl (tuple_to_list (process_info (self (), current_function)))))).

%% Usage: ?INFO_MSG("info message ~p~n", [Reason]),
%% INFO_MSG(Msg, Args) -> ok

-define (INFO_MSG (Msg, Args), error_logger:info_msg ("~p:~p/~p (line ~p) " ++ Msg, ?CURRENT_FUNCTION ++ [?LINE | Args])).


%% Usage: ?ERROR_MSG("error message ~p~n", [Reason]),
%% ERROR_MSG(Msg, Args) -> ok

-define (ERROR_MSG (Msg, Args), error_logger:error_msg ("~p:~p/~p (line ~p) " ++ Msg, ?CURRENT_FUNCTION ++ [?LINE | Args])).


%% ?FWRITE
%%
%% io:fwrite anything
%%
%% usage:  ?FWRITE ({ Something, Something2 ... })
%%
%% Cheap way to print out information during debugging.
%% Cheap because it saves typing.
%% Different from ?SHOW because it prints to console instead of error logger.

-define (FWRITE (Anything), io:fwrite ("~p~n", [Anything])).


%% ?SHOW
%%
%% show anything
%%
%% usage:  ?SHOW ({ Something, Something2 ... })
%%
%% Cheap way to print out information during debugging.
%% Cheap because it saves typing.

-define (SHOW (Anything), error_logger:info_report (Anything)).


%% ?LOG_INFO
%%
%% Generate standardized info log message with module name and line number.

-define (LOG_INFO (Reason), error_logger:info_report ({{module, ?MODULE}, {line, ?LINE}, Reason})).

%% ?LOG_ERROR
%%
%% Generate standardized error log message with module name and line number.

-define (LOG_ERROR (Reason), error_logger:error_report ({{module, ?MODULE}, {line, ?LINE}, Reason})).


%% ?CHECK_ARG
%%
%% parameter type checking
%%
%% usage:  ?CHECK_ARG (integer (ExpectedInteger))
%%
%% TBD: define a parse-transform that can print the value that was tested.

-define (CHECK_ARG (Guard), if Guard -> ok; true -> badmatch = {arg_check, ?MODULE, ?LINE} end).
					

%% ?UNEXPECTED_MESSAGE
%%
%% standard way to express an unexpected message to the error log
%%
%% usage:  receive
%%           ...
%%           Unexpected ->
%%             ?UNEXPECTED_MESSAGE (Unexpected)
%%         end

-define (UNEXPECTED_MESSAGE (Message), error_logger:error_report ({{module, ?MODULE}, {line, ?LINE}, unexpected_message, Message})).


%% ?HERE
%%
%% Attaches file and line info to argument.
%%
%% usage:  throw (?HERE(timeout))

-define (HERE (Anything), {{module, ?MODULE}, {line, ?LINE}, Anything}).



%% ?FAIL
%%
%% Generates badmatch error with module and line info.
%%
%% usage:  ?FAIL (Anything)

-define (FAIL (Anything), badmatch = {failure, ?HERE (Anything)}).

%% ?MATCH
%%
%% Generates badmatch error with module and line info if X does not match Y.
%%
%% usage:  ?MATCH (A,A)

-define (MATCH (X, Y), {?MODULE, ?LINE, X} = {?MODULE, ?LINE, Y}).

%% ?ASSERT
%%
%% Generates badmatch error with module and line info if the condition is false.
%%
%% usage:  ?ASSERT (1 < 2)

-define (ASSERT (Condition), case Condition of true -> ok; false -> badmatch = {assertion_failure, {module, ?MODULE}, {line, ?LINE}} end).

%% ?PORT
%%
%% Generates a port number to be used for unit tests in "user mode", i.e. when developing and testing
%% things that will eventually have well-known ports in production.  This allows N developers to share
%% the same development host and run the same tests at the same time.  This occurs often enough to
%% alwys use the symbolic port numbers.  Just pass in 0 through 10 for your port number and the macro
%% will pull the real port number from your environment variable named PORTn.  Company practice currently
%% requires developers to be assigned a unique range of port numbers that are kept in a .ports file sourced
%% before tests are run so that no two users will conflict.
%%
%% usage:  ?PORT (0)

-define (PORT (PortNumber), case os:getenv ("PORT" ++ integer_to_list (PortNumber)) of
				false -> 9000 + PortNumber;
				_ -> list_to_integer (os:getenv ("PORT" ++ integer_to_list (PortNumber)))
			    end).

%%Better than the port command.
-define (BPORT (PortNumber), 10000 + list_to_integer(string:strip(os:cmd("id -u"), both, $\n)) + PortNumber).
						      
