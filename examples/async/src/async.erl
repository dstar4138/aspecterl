-module(async).
-behaviour( application ).
-behaviour( supervisor  ).

% Callbacks for application:
-export( [start/2] ).
% Callbacks for supervisor.
-export( [init/1] ).

% Will inject all needed functions for application or supervisor
-aspect( [inject_missing] ).

% Ease of use function for init.
-define(CHILD(Mod, Type, Args), {Mod, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

% Start self as a supervisor too.
start(_, Args) -> async:start_link( {local, ?MODULE}, ?MODULE, [Args] ).


% Easy to see that the Args from start/2 fall down to the supervisor's init/1.
init( _Args ) -> 
    {ok, {{ one_for_one, 5, 10}, [?CHILD( server_example, worker, [])]}}.
