%% Function Decorators can be wrapped around functions as long as you
%% include_lib: AspectErl/include/aspect.hrl
-record(decorate, {
        name :: atom(),   %% Name of advice function
        args :: [term()], %% Additional arguments to pass to advice function.
        module :: atom()  %% Further specify advice function naming a module.
}).
%% Function Decorations can be applied like so:
%%                  -decorate({Name, Args}). 
%% Or like:
%%               -decorate({Module, Name, Args}).
%%
%% So for example:
%% -decorate({pool, [{name,"MyProcessPool"}]}).
%%      Will add the function's process to a particular process pool.
%%
%% If no 'pool' advice function is found, the system will issue a compiler 
%% error. Advice can also issue compiler errors if their function being 
%% applied to doesn't match their internal requirements. Namely:
%%
%%  -advice([{type,'around'}, ... ]).
%%  pool( AroundCall={Module, Func, _Args}, PassedToPool ) ->
%%      io:format("Adding ~p to pool: ~p\n",[Module,PassedToPool]),
%%      Pool = getPoolName( PassedToPool ),
%%      case Func of 
%%          start_link -> % Started, and linked to caller. 
%%              {ok, Pid} = ?proceed(AroundCall),
%%              add_to_pool(Pid,Pool),
%%              {ok, Pid};
%%          start ->      % Started, without a link.
%%              {ok, Pid} = ?proceed(AroundCall),
%%              add_to_pool(Pid, Pool),
%%              {ok, Pid};
%%          _ -> ?ERROR("Not a start function, cannot decorate with pool.")
%%      end.

