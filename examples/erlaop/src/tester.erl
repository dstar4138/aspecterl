%% The src directory is the target for all advice found. So this module is fair
%% game for aspect application.
-module(tester).

-export([run/0, test1/2, test2/2]).

% Run two tests with constant values.
run() -> test1( 42, 24 ), test2( [1,2,3,4], [] ).

% Sum two numbers
test1( A, B ) -> A+B.

% Reverse a list
test2( [], L2 ) -> L2;
test2( [H|R], L2) -> test2(R, [H|L2] ).
