%% Testing the two other 'after' advice type 'after_return' and 'after_final'.
%%  - after_return is tested by running on all test calls, but fails on test3
%%    due to the error being thrown.
%%  - after_final is tested by running on all test calls, and even runs on 
%%    test3 despite throwing an error.
-module(ret_log_advice).
-include_lib("aspecterl/include/aspect.hrl").

-export([ret_final_advice/2, ret_return_advice/2]).

% All functions that start with 'test'.
-pointcut({testfun, [{func,"test\\w*"}]}).

% Test of 'on return' advice.
-advice([{type, 'after_return'}, {pointcuts, [testfun]}]).
ret_return_advice( {Module, Fun, _Args, _RealName}, AdviceArgs ) ->
    Return = get_return( AdviceArgs ),
    io:fwrite("===After Return (~p:~p) -> ~p~n", [Module, Fun, Return]).

% Test of 'on final' advice.
-advice([{type, 'after_final'}, {pointcuts, [testfun]}]).
ret_final_advice( {Module, Fun, _Args, _RealName}, _AdviceArgs ) ->
    io:fwrite("===After Final (~p:~p) -> No access to Err or Res~n", [Module, Fun]).

get_return( List ) ->
    case lists:keyfind( return, 1, List ) of
        {return, Ret} -> Ret;
        false -> {noreturn, badarg}
    end.

