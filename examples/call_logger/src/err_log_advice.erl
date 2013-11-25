%% Error logger, catch all test functions which throw errors.
-module(err_log_advice).
-include_lib("aspecterl/include/aspect.hrl").

-export([err_advice/2]).

% Check for all functions that start with 'test'.
-pointcut({testfun, [{func,"test\\w*"}]}).

-advice([{type,'after_throw'},{pointcuts,[testfun]}]).
err_advice( {Module, Fun, _Args, _RealName}, AdviceArgs ) ->
    {Err, Rsn} = get_error( AdviceArgs ),
    io:fwrite("===Got error from ~p:~p -> ~p:~p~n", [Module,Fun,Err,Rsn]).

get_error( List ) ->
    io:fwrite("DERP:~p~n",[List]),
    case lists:keyfind( throw, 1, List ) of
        {throw, Err} -> Err;
        false -> {noerr, badarg}
    end.
