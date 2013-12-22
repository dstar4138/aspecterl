%% This module provides some advice functions using bare bones Erlang without 
%% including any AspectErl information. 
-module(myadvice).

-export([tickrun/2, finrun/2]).

%% Will print entry information about the function being called.
tickrun( _BeforeCall={Module, Func, Args, _}, _ ) ->
    print_function( "Entering -> ", Module, Func, Args ).

%% Will print exit information about the function that just finished.
finrun( _AfterCall={Module, Func, Args, _}, _ ) ->
    print_function("Exiting -> ", Module, Func, Args ).

%% Pretty Print a function definition.
print_function(Prefix, M,F,A) ->
    io:format(" ~s ~p:~p(~p)~n",[Prefix, M, F, A]).
