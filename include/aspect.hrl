% Tell AspectErl to not try and inject in this file, because its an advice file.
-aspecterl([ exclude, advice_file ]).

%%% Define some helpful macros for library user. %%%
%% Inject compiler errors via Advice.
-define(COMPILE_ERROR(M),% TODO: Make it an actual compile time error. 
            io:fwrite("ERROR(~p:~p): ~s\n", [?MODULE,?LINE,M])).
%% Proceed function call within an Around Advice.
-define(proceed(FuncCall), begin
            case FuncCall of
                {M,_,A,F} -> erlang:apply(M,F,A);
                _ -> throw("Proceed's parameter is not valid.")
            end
        end).
