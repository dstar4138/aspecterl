-define(AspectErlAttr, aspecterl). %Use -aspecterl( [...] ).

-ifdef(noaspects). %Turn off aspects by: erlc -Dnoaspects *.erl
-define(AspectsOn, false).
-else.
-define(AspectsOn, true).
-endif.

% Transformation State, this is built from the compile options as well as 
% environment settings for the aspect application.
-record( aspect_pt, {
    file, 
    module,
    behaviours = [],
    inject_missing = false,
    exported = [],
    verbose = false,
    pct=[],
    adv=[]
}).


