%% Advice Annotations can be given to functions as long as you 
%% include_lib: AspectErl/include/aspect.hrl
-record(advice, {
        type :: 'before'       % Before the function call, result is ignored
              | 'after_throw'  % After an exception is thrown  
              | 'after_return' % After the return statement
              | 'after_final'  % After function completes, result is not passed
              | 'around',      % Around the function call, call ?proceed/1 to continue.
        pointcut :: [atom()],  % List of pointcuts that if match insert the advice.

        %% The following are internal use only:
        name :: atom(),        % The name of the advice function.
        module :: atom(),      % Module the advice function appears in.
        args :: [term()]      % Parameters to pass to function.
}).        
%% Advice function spec:
%%  fun_name( FuncCall, Args )
%%
%%      where FuncCall can be:
%%          {Module :: atom(),  -- The Module getting called.
%%           Func :: atom(),    -- The Function getting called.
%%           Args :: [term()]}  -- The Args to the function.
%%      and Args is a list of whatever was passed into the decorator and if 
%%      the pointcut was after_throw, Args will contain a: {throw,{Err,Rz}}
%%      where
%%          Err -- The Error type.
%%          Rz  -- The reason.
%%      Additionally, if its a after_return, Args will contain a {result, R}
%%      where R is the return value of the function call you are advising.

%% Proceed function call within an Around Advice.
-define(proceed(FuncCall), aspecterl:proceed(FuncCall)).
