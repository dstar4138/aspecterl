%% This module advises an example application where we want all gen_servers
%% who have to do with thing '_b' and not '_a' to join a the Pool "BServers".
-module(pool_advice).
-include_lib("aspecterl/include/aspect.hrl").

-export([pool/2]).

% Pointcut to B Servers.
-pointcut({bservers, [{behaviour,"gen_server"},
                      {module,"\\w*_b"}]).

-advice([{pointcut,[bservers]},
         {type,'around'},
         {args, [{default_pool,"BServers"}]}]).
pool( AroundCall={Module, Func, _Args}, PassedToPool ) ->
    Pool = getPoolName( PassedToPool ),
    io:fwrite("Adding ~p to pool: ~p\n",[Module,Pool]),
    case Func of
        start_link -> % Started, and linked to caller. 
              {ok, Pid} = ?proceed(AroundCall),
              add_to_pool(Pid,Pool),
              {ok, Pid};
          start ->      % Started, without a link.
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

