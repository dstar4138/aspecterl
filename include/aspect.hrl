-compile({parse_transform,aspect}).
-include_lib("aspecterl/include/pointcut.hrl").
-include_lib("aspecterl/include/advice.hrl").
-include_lib("aspecterl/include/decorate.hrl").

%TODO: Get line number of aspect and offending portion of code.
-define(COMPILE_ERROR(M), io:fwrite("ERROR: ~s\n", [M])).
