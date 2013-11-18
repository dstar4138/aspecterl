% Force file to be parsed with the extractor.
-compile({parse_transform,aspecterl_extractor}).
% Tell AspectErl to not try and inject in this file.
-aspecterl([ exclude ]).

%%% Define some helpful macros for library user. %%%
%% Inject compiler errors via Advice.
-define(COMPILE_ERROR(M),% TODO: Make it an actual compile time error. 
            io:fwrite("ERROR(~p:~p): ~s\n", [?MODULE,?LINE,M])).
%% Proceed function call within an Around Advice.
-define(proceed(FuncCall), aspecterl_cb:proceed(FuncCall)).
