%% Simple messy module for testing advice.
-module(broken_module).

% Run each of these tests to examine different advisement policies.
-export([test1/0, test2/0, test3/0]).

%% @doc Tests will get advised by call_log which will show that it didn't get 
%%   any parameters. They calls one function which has parameters which will 
%%   get logged as well.
%% @end
test1() -> ok.
test2() -> internal_call( 1, "hi", false ).
test3() -> im_going_to_break().

%% @doc A simple internal call so logging can capture arguments and return 
%%   value.
%% @end
internal_call( A, B, C ) -> lists:member( 1, [A,B,C] ), ok.

%% @doc Causes an error to be thrown if called. This function if advised will 
%%   be safe to call.
%% @end
im_going_to_break() -> error({error, badarg}).

