-module( server_example ).
-behaviour( gen_server ).

% This is an Async Server which will only handle Casts, all other should 
% error out. AspectErl will handle the missing features. This is explicit
% so that readers of the code know how the implementation is handled.
-aspecterl( inject_missing ).
-export( [init/1, handle_cast/2 ] ).

%% Our State for this example is empty.
-record( state, {} ).
init( _Args ) -> {ok, #state{} }.

%% We will handle casts by logging them after displaying them to the screen.
handle_cast( Msg, State ) ->
    io:format( "CastMsg: ~p~n", [Msg] ),
    {noreply, State}.

