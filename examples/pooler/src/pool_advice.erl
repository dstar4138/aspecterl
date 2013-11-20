%% This module advises an example application where we want all gen_servers
%% who have to do with thing '_b' and not '_a' to join a the Pool "BServers".
-module(pool_advice).
-include_lib("aspecterl/include/aspect.hrl").

-export([pool/2]).

% Pointcut to B Servers.
-pointcut({bservers, [{behaviour,"gen_server"},
                      {module,"\\w*_b"}]}).

-advice([{pointcuts,[bservers]},
         {type,'around'},
         {args, [{default_pool,'bServers'}]}]).
pool( AroundCall={Module, Func, _Args,_NewName}, PassedToPool ) ->
    Pool = getPoolName( PassedToPool ),
    case Func of
        start_link -> % Started, and linked to caller. 
              io:fwrite("Adding ~p to pool: ~p\n",[Module,Pool]),
              {ok, Pid} = ?proceed(AroundCall),
              add_to_pool(Pid,Pool),
              {ok, Pid};
          start ->      % Started, without a link.
              io:fwrite("Adding ~p to pool: ~p\n",[Module,Pool]),
              {ok, Pid} = ?proceed(AroundCall),
              add_to_pool(Pid, Pool),
              {ok, Pid};
          _ -> ?proceed(AroundCall) %% Ignore if not a start function.
    end.

%% Adds a PID to a particular pool name.
add_to_pool( Pid, PoolName ) ->
    case catch pg:create( PoolName ) of
        _ -> pg:join( PoolName, Pid )
    end.

%% Grabs the name of the pool from a list of options
getPoolName( Args ) ->
    case lists:keysearch(name, 1, Args) of
        {value, {_,Name}} -> Name;
        false -> (case lists:keysearch(default_pool, 1, Args) of
                    {value, {_,Name}} -> Name;
                    false -> %NOT POSSIBLE AS ADVICE ADDS IT.
                        ?COMPILE_ERROR("UNREACHABLE, NO POOL NAME FOUND")
                  end)
    end.

