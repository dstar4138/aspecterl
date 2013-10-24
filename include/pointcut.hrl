%% Point-Cut Annotations can be added to Erlang files as long as you
%% include_lib: AspectErl/include/aspect.hrl
-record(pointcut, { 
          % Name of the pointcut that advice will be inserted based on.
          name :: atom(),

          % Using Perl Based Regular Expressions (PCRE).
          %       http://www.erlang.org/doc/man/re.html
          module    = nil :: string(),    % Regex on the module name.
          func      = nil :: string(),    % Regex on the function name.
          behaviour = nil :: string(),    % Regex on the Behaviours. 
          arity     = nil :: [integer()], % List of aritys that match.
        
          % Code weaving can happen to any functions, but default is
          % to only public functions. 
          scope     = 'public' :: 'public' | 'private' | 'any'
        }).
%%
%% Example pointcuts:
%%
%% The following pointcut triggers on all function calls into modules with the
%% gen_server behaviour:
%%      -pointcut({all_servers, [{ behaviour, "gen_server" }]}).
%%
%% The following pointcut triggers only on functions that are eunit test 
%% functions:
%%      -pointcut({eunit_tests, [{ func, "\\w*_test(_)?" }]).
%%
%% All supervisory modules, but only the init functions:
%%      -pointcut({init_sup, [{ module, "\\w*_sup" },
%%                            { func, "init" }]).
%%
