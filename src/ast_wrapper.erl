%% The AST Wrapping Module. 
%%
%% @doc 
%%  This module abstracts the usage of the Erlang Abstract Syntax Tree from
%%  the injection code. Please see the aspecterl_injector module for details.
%%  AspectErl only provides five methods of injection:
%%  <ul>
%%      <li> Before - Advice gets called before function execution, will get 
%%           access to the parameters passed in.</li>
%%  
%%      <li> OnReturn - Advice gets called after function execution, will get 
%%          access to parameters passed in and return value. Will not get to 
%%          edit it.</li>
%%      
%%      <li> OnThrow - Advice gets called if function throws an error, will get
%%          access to parameters passed in and error code in form 
%%          `{throw, {Er,Rz}}'.</li>
%%      
%%      <li> OnFinal - Advice gets called after function execution. If function
%%          throws an error, or error handling breaks, Advice still gets 
%%          executed. Will get access to parameters passed in.</li>
%%      
%%      <li> Around - Advice gets called in place of function. It is up to 
%%          advice to call the provided `?proceed(...)' macro to run the 
%%          function. </li>
%%  </ul>
%% @end
%% @see aspecterl_injector
%% @author Alexander Dean
-module(ast_wrapper).

% If Testing, then make sure all functions are exported for individual tests.
-ifdef(EUNIT).
-compile([debug_info, export_all]).
-endif.

% Functions to wrap a function call in/around/after a function definition.
% They all return: {ok, NewExports, NewForms}.
-export([ before/4, return/4, onthrow/4, final/4, around/4 ]).
% Function for Injecting missing function based on behaviours.
-export([ inject_error_fun/4 ]).

% Defaults for injected error functions
-define(DUMMY_LINE, 9001).
-define(INJECT_ERROR, "AspectErl Injected function (~p:~p/~p) called!").

%% @doc
%%   Modifies the Function forms passed in so that it calls the the Fun before
%%   each clause of the Function definition. In otherwords It converts:
%%   
%%   ``` f( X, Y, ... ) -> ...stuff... '''
%%
%%   to
%%
%%   ``` 
%%   f( P1, P2, ... ) -> 
%%       Before( {?MODULE, f, [P1,P2,...], f_@}, Args), 
%%       f_@( P1, P2, ... ).
%%   f_@( X, Y, ... ) -> ...stuff... 
%%   '''
%% @end
before( Before, {function, Line, Name, Arity, Clauses}, Module, AST ) -> 
    {NewFun, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses, AST ),
    NewFunction = 
    {function, Line, Name, Arity,
        [ % Arg List- pulled from clauses.
          {clause, Line, argslist( Arity, Line ),
            [], 
            [  
               % Before( {Module, Name, Args, NewFunc}, BeforeArgs ),
               % R = NewFunc( P1, P2, ... ).
               run_save_fun( false, Line, Before, Module, Name, Arity, NewFun ), 
               run_func( false, Line, NewFun, Arity )
            ]
          }
        ]
    },
    {ok, [{NewFun,Arity}],
         [NewFunction, RenamedFunc]}.

%% @doc
%%   Modifies the Function forms passed in so that it calls the Fun instead.
%%   It is up to the Around Function to call the ?proceed() macro. It converts:
%%
%%   ``` f( X, Y, ... ) -> ...stuff... '''
%%
%%   to
%%
%%   ```
%%    f( P1, P2, ... ) -> Around( {?MODULE, f, [P1,P2,..] f_@}, Args).
%%    f_@( X, Y, ... ) -> ...stuff...
%%   '''
%% @end
around( Around, {function, Line, Name, Arity, Clauses}, Module, AST ) ->
    {NewFun, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses, AST ),
    NewFunction = 
        {function, Line, Name, Arity, 
            [ % Arg List - Pulled from Clauses.
              {clause, Line, argslist( Arity, Line ),
               [],
               [
                % Around( {Module, Name, Args, NewFunc}, AroundArgs ).
                run_save_fun( false, Line, Around, Module, Name, Arity, NewFun )
                ]
              }
            ]
        },
    {ok, [{NewFun, Arity}], [NewFunction, RenamedFunc]}.

%% @doc
%%   Modifies the Function forms passed in so that it calls Fun after the
%%   original function finishes. The result of the old function is weaved around 
%%   Funs execution and returned. It converts:
%%
%%   ``` f( X, Y, ... ) -> ...stuff... '''
%%
%%   to
%%
%%   ```
%%    f( P1, P2, ... ) -> 
%%       R = f_@( P1, P2, ... ),
%%       OnReturn( {?MODULE, f, [P1,P2,...], f_@}, [{return,R}|Args] ),
%%       R.
%%    f_@( X, Y, ... ) -> ...stuff..
%%   '''
%% @end
return( OnReturn, {function, Line, Name, Arity, Clauses}, Module, AST ) -> 
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses, AST ),
    NewFunction = 
        {function, Line, Name, Arity,
            [ %Arg List - Pulled from Clauses.
               {clause, Line, argslist( Arity, Line ), 
                 [],
                 [
                  % R = M:F( A ),
                  % OnReturn( {Module,Name,Args,NewFun}, [{return,R}|RetArgs] ),
                  % R.
                  run_func( true, Line, NewFunc, Arity ),
                  run_save_fun_append( false, Line, OnReturn, Module, Name, 
                                                          Arity, NewFunc, 'R' ),
                  {var,Line,'R'}
                 ]
               }
            ]
        },
    {ok, [{NewFunc,Arity}], [NewFunction, RenamedFunc]}.

%% @doc
%%   Modifies the Function forms passed in so that it calls Fun only if the 
%%   original function throws an exception. The exception data will be passed
%%   to the advice function via the Args. It converts:
%%
%%   ``` f( X, Y, ... ) -> ...stuff... '''
%%
%%   to
%%
%%   ```
%%    f( P1, P2, ... ) ->
%%      try f_@( P1, P2, ... )
%%      catch Er:Rz ->
%%         Throw = {Er,Rz}
%%         OnThrow( {?MODULE, f, [P1,P2,...], f_@}, [{throw,Throw}|ThrowArgs])
%%      end.
%%    f_@( X, Y, ... ) -> ...stuff...
%%  '''
%% @end 
onthrow( OnThrow, {function, Line, Name, Arity, Clauses}, Module, AST ) ->
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses, AST ),
    NewFunction = 
        {function, Line, Name, Arity,
            [ %Arg List - Pulled from Clauses.
               {clause, Line, argslist( Arity, Line ), 
                 [],
                 [
                  % try M:F( P1, P2, ... ), 
                  % catch Er:Rz ->
                  %     Throw = {Er, Rz}, 
                  %     OnThrow( {Module, Name, Args, NewFunc}, 
                  %              [{throw,Throw}|ThrowArgs])
                  % end
                  {'try',Line,
                   [ run_func( false, Line, NewFunc, Arity ) ],
                   [],
                   [ {clause, Line, 
                      [{tuple, Line, [{var,Line,'Er'},
                                      {var, Line, 'Rz'},
                                      {var,Line,'_'}]}], 
                      [],
                      [ 
                       {match, Line, {var, Line, 'Throw'},
                                     {tuple, Line, [{atom, Line, 'throw'}, 
                                                    {tuple, Line, 
                                                        [{var,Line,'Er'},
                                                         {var,Line,'Rz'}]}
                                                   ]}},
                        run_save_fun_append( false, Line, OnThrow, Module, Name, 
                                                       Arity, NewFunc, 'Throw' ) 
                        ]}
                    ],
                   []}
                 ]
               }
            ]
        },
    {ok, [{NewFunc,Arity}], [NewFunction, RenamedFunc]}.

%% @doc
%%   Modifies the Function forms passed in so that it calls Fun at the end of
%%   the function even if an exception is thrown. This advice does not get 
%%   access to the exception. It converts:
%%
%%   ``` f( X, Y, ... ) -> ...stuff... '''
%%
%%   to
%%   
%%   ```
%%     f( P1, P2, ... ) -> 
%%        try f_@( P1, P2, ... )
%%        after OnFinal( {?MODULE, f, [P1,P2,...], f_@}, FinArgs )
%%        end.
%%     f_@( X, Y, ... ) -> ...stuff...
%%   '''   
%% @end
final( OnFinal, {function, Line, Name, Arity, Clauses}, Module, AST ) ->
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses, AST ),
    NewFunction = 
        {function, Line, Name, Arity,
            [ %Arg List - Pulled from Clauses.
               {clause, Line, argslist( Arity, Line ), 
                 [],
                 [
                  % try M:F( P1, P2, ... )
                  % after
                  %     OnFinal( {Module, Name, Args, NewFunc}, FinArgs)
                  % end
                  {'try',Line,
                   [ run_func( false, Line, NewFunc, Arity ) ],
                   [], [],
                   [ 
                      run_save_fun( false, Line, OnFinal, Module, 
                                                Name, Arity, NewFunc )
                   ]}
                 ]
               }
            ]
        },
    {ok, [{NewFunc,Arity}], [NewFunction, RenamedFunc]}.

%% @doc Injects a function into the AST that just throws an error when called.
%%   This is useful when a function is automatically added via the 
%%   'inject_missing' command AspectErl provides.
%% @end  
inject_error_fun( Module, Fun, Arity, AST ) ->
    ErrorMsg = lists:flatten(io_lib:format(?INJECT_ERROR, [Module,Fun,Arity])),
    FakeFun = {function, ?DUMMY_LINE, Fun, Arity,
                [{clause, ?DUMMY_LINE, argsdlist( Arity, ?DUMMY_LINE ), [],
                   [{call, ?DUMMY_LINE, {atom, ?DUMMY_LINE, error},
                       [{string, ?DUMMY_LINE, ErrorMsg}]}]}]},
    NewAST = AST ++ [ FakeFun ], % insert below.
    parse_trans:export_function( Fun, Arity, NewAST ). % Add as an export.


%%% =========================================================================
%%% Internal AST Builders
%%% =========================================================================

%% @private
%% @doc Builds the function tuple expected by the Proceed function.
build_proceed_func( Line, Module, Name, Arity, NewFunc ) ->
    mktuple( Line, [ mkatom(Line, Module),
                     mkatom(Line, Name),
                     mklist(Line, argslist(Arity, Line) ), 
                     mkatom(Line, NewFunc) ]).

%% @private
%% @doc Like the next function builds a runner function, but for the Advice
%%   function of the form:
%%              T = M:F( {Module, Name, Args}, [V|A] ).
%% @end
run_save_fun_append( Save, Line, {M,F,A}, Module, Name, Arity, NewName, Var ) ->
    Call = {call, Line, mkapply( Line, M, F ),
                [ build_proceed_func( Line, Module, Name, Arity, NewName ),
                  {cons, Line, {var, Line, Var}, term_to_forms(Line, A)}] },
    case Save of
        true -> {match, Line, {var, Line, 'T'}, Call};
        false -> Call
    end.

%% @private
%% @doc Like the previous function builds a runner function, but for the
%%   Advice function of the form:
%%               T = M:F( {Module, Name, Args}, A ).
%% @end
run_save_fun( Save, Line, {M,F,A}=_Before, Module, Name, Arity, NewName ) ->
    Call = {call, Line, mkapply( Line, M, F ),
                [ build_proceed_func( Line, Module, Name, Arity, NewName ),
                  term_to_forms(Line, A)] },
    case Save of
        true -> {match, Line, {var, Line, 'T'}, Call};
        false -> Call
    end.

%% @private
%% @doc Builds a running function definition:
%%           R = FuncName( P1, P2, ... ),
%% @end
run_func( Save, Line, FuncName, Arity ) ->
    Call = {call, Line, mkatom( Line, FuncName ),
                        argslist(Arity, Line)},
    case Save of
        true -> {match, Line, {var, Line, 'R'}, Call};
        false -> Call
    end.


%%% =========================================================================
%%% Utility Functions
%%% =========================================================================

%% @hidden
%% @doc Generates a new function Abstract Form given all of the pieces and the
%%   AST it's going into (so it can check if the name it needs to generate is
%%   already taken or not).
%% @end   
gen_mkfunc( Line, Name, Arity, Clauses, AST ) ->
    NewName = gen_newname( Name, Arity, AST ),
    Func = {function, Line, NewName, Arity, Clauses},
    {NewName, Func}.

%% @hidden
%% @doc Generates a new function name by appending a tag to the end of the name
%%   repeatedly until it doesnt exist.
gen_newname( Name, Arity, AST ) ->
    UpdatedName = list_to_atom( atom_to_list(Name) ++ "_@" ),
    case parse_trans:function_exists( UpdatedName, Arity, AST ) of
        true  -> gen_newname( UpdatedName, Arity, AST );
        false -> UpdatedName
    end.

%% @hidden
%% @doc Creates an argument list of a particular arity on a given line.
argslist( N, L ) -> argslist( N, L, [] ).
argslist( 0, _, R ) -> R;
argslist( N, L, R ) -> argslist( N-1, L, [ mkvar( L, N ) | R ] ).

%% @hidden
%% @doc Creates an argument list like argslist/3 but uses ignores, '_'.
argsdlist( N, L ) -> argsdlist( N, L, [] ).
argsdlist( 0, _, R ) -> R;
argsdlist( N, L, R ) -> argsdlist( N-1, L, [ {var,L,'_'} | R ] ).

%% @hidden
%% @doc Our temporary Parameters are called P# where # is its position starting 
%%   at 1. We can generate lists of them for doing tuples or cons lists.
%% @end
argname( N ) -> list_to_atom("P" ++ integer_to_list(N)).

%% @hidden
%% @doc Creates an AST representation of a variable.
%% @end
mkvar( Line, N ) -> {var, Line, argname(N)}.

%% @hidden
%% @doc Create an AST representation of a List at a particular line.
mklist( Line, [] ) -> {nil,Line};
mklist( Line, [A|R] ) -> {cons, Line, A, mklist(Line,R)}.

%% @hidden
%% @doc Create an atom at a particular line.
mkatom( Line, A ) -> {atom, Line, A}.

%% @hidden
%% @doc Create a tuple from a list of ASTs.
mktuple( Line, L ) -> {tuple, Line, L}.

%% @hidden
%% @doc Creates an function application AST.
mkapply( Line, M, F ) -> {remote, Line, {atom, Line, M},{atom, Line, F}}.

%% @hidden
%% @doc Create an AST representation out of an erlang Term.
term_to_forms( Line, Term ) ->
    update_lineno( Line, 
        erl_syntax:revert(erl_syntax:abstract(Term))).

%% @hidden
%% @doc Replace the Line Number in the AST.
update_lineno( L, T) -> ul(L,T).
ul( L, {cons, _, A,B} ) -> {cons, L, ul(L,A), ul(L,B)};
ul( L, {nil, _} )      -> {nil,L};
ul( L, {tuple, _, X} ) -> {tuple, L, lul(L,X)};
ul( L, {atom, _, A} )  -> {atom, L, A};
ul( L, {string, _,S} ) -> {string, L, S};
ul( L, {bin, _, B} ) -> {bin, L, B};
ul( L, {float,_,F} ) -> {float, L, F};
ul( L, {integer,_,I}) -> {integer, L, I};
ul( L, {remote, _, A,B} ) -> {remote, L, ul(L,A), ul(L,B)};
ul( _, UnknownForm) -> UnknownForm.
lul(L, List) -> lists:map(fun( X )->ul(L,X) end, List).


