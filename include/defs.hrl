-ifdef(noaspects). %Turn off aspects by: erlc -Dnoaspects *.erl
-define(AspectsOn, false).
-else.
-define(AspectsOn, true).
-endif.
