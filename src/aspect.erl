%% Aspect.Erl -
%%
%%  This is the module which gets inserted into the pre-processor to strip out
%%  annotations for advice and pointcuts so that weeving can take place.
%%
%%  @author Alexander Dean
-module(aspect).

% The Compile-time hook function. See include/aspect.hrl for more information.
-export([parse_transform/2]).

parse_transform( AST, _Options ) -> 
    io:fwrite("AST = ~p\n",[AST]),
    AST.
