%% Testing before advice for all functions that end in _call.
-module(call_log_advice).
-include_lib("aspecterl/include/aspect.hrl").

-export([call_advice/2]).

% All functions found that end in '_call'.
-pointcut({callfun, [{func,"\\w*_call"},{scope,'any'}]}).

% Advice function applied before all functions ending in '_call'.
-advice([{type,'before'},{pointcuts,[callfun]}]).
call_advice( {Module, Name, Args, _RealName}, _AdviceArgs ) ->
    io:fwrite("===Before Advice (~p:~p) got args (~p).~n",[Module, Name, Args]).

