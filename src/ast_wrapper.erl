%% AST Wrapping Module. 
%%  
%%  This module abstracts the usage of the Erlang Abstract Syntax Tree from
%%  the injection code. Please see the aspecterl_injector module for details.
%%  AspectErl only provides five methods of injection:
%%
%%      * Before - Advice gets called before function execution, will get access
%%          to the parameters passed in.
%%  
%%      * OnReturn - Advice gets called after function execution, will get 
%%          access to parameters passed in and return value. Will not get to 
%%          edit it.
%%      
%%      * OnThrow - Advice gets called if function throws an error, will get
%%          access to parameters passed in and error code in form 
%%          {throw, {Er,Rz}}.
%%      
%%      * OnFinal - Advice gets called after function execution. If function
%%          throws an error, or error handling breaks, Advice still gets 
%%          executed. Will get access to parameters passed in.
%%      
%%      * Around - Advice gets called in place of function. It is up to advice
%%          to call the provided ?proceed(...) macro to run the function. 
%%
%% @author Alexander Dean
-module(ast_wrapper).

% Functions to wrap a function call in/around/after a function definition.
% They all return: {ok, NewExports, NewForms}.
-export([ before/3, return/3, onthrow/3, final/3, around/3 ]).
% Function for Injecting missing function based on behaviours.
-export([ inject_error_fun/3 ]).

%% Modifies the Function forms passed in so that it calls the the Fun before
%% each clause of the Function definition. In otherwords It converts:
%%
%% f( X, Y, ... ) -> ...stuff...
%% 
%% to
%%
%% f( P1, P2, ... ) -> 
%%     Before( {?MODULE, f, [P1,P2,...], f_@}, Args), 
%%     f_@( P1, P2, ... ).
%% f_@( X, Y, ... ) -> ...stuff... 
%%
before( Before, {function, Line, Name, Arity, Clauses}, Module ) -> 
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses ),
    NewFunction = 
    {function, Line, Name, Arity,
        [ % Arg List- pulled from clauses.
          {clause, Line, argslist( Arity, Line ),
            [], 
            [  
               % Before( {Module, Name, Args, NewFunc}, BeforeArgs ),
               % R = NewFunc( P1, P2, ... ).
               run_save_fun( false, Line, Before, Module, Name, Arity, NewFunc ), 
               run_func( Line, NewFunc, Arity )
            ]
          }
        ]
    },
    {ok, [{NewFunc,Arity}],
         [NewFunction, RenamedFunc]}.

%% Modifies the Function forms passed in so that it calls the Fun instead.
%% It is up to the Around Function to call the ?proceed() macro. It converts:
%%
%% f( X, Y, ... ) -> ...stuff...
%%
%% to
%%
%% f( P1, P2, ... ) -> Around( {?MODULE, f, [P1,P2,..] f_@}, Args).
%% f_@( X, Y, ... ) -> ...stuff...
%%
around( Around, {function, Line, Name, Arity, Clauses}, Module ) ->
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses ),
    NewFunction = 
        {function, Line, Name, Arity, 
            [ % Arg List - Pulled from Clauses.
              {clause, Line, argslist( Arity, Line ),
               [],
               [
                % Around( {Module, Name, Args, NewFunc}, AroundArgs ).
                run_save_fun( false, Line, Around, Module, Name, Arity, NewFunc )
                ]
              }
            ]
        },
    {ok, [{NewFunc, Arity}], [NewFunction, RenamedFunc]}.

%% Modifies the Function forms passed in so that it calls Fun after the original
%% function finishes. The result of the old function is weaved around Funs
%% execution and returned. It converts:
%%
%% f( X, Y, ... ) -> ...stuff...
%%
%% to
%%
%% f( P1, P2, ... ) -> 
%%     R = f_@( P1, P2, ... ),
%%     OnReturn( {?MODULE, f, [P1,P2,...], f_@}, [{return,R}|Args] ),
%%     R.
%% f_@( X, Y, ... ) -> ...stuff..
%%
return( OnReturn, {function, Line, Name, Arity, Clauses}, Module ) -> 
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses ),
    NewFunction = 
        {function, Line, Name, Arity,
            [ %Arg List - Pulled from Clauses.
               {clause, Line, argslist( Arity, Line ), 
                 [],
                 [
                  % R = M:F( A ),
                  % OnReturn( {Module, Name, Args, NewFunc}, [{return,R}|RetArgs] ),
                  % R.
                  run_func( Line, NewFunc, Arity ),
                  run_save_fun_append( false, Line, OnReturn, Module, Name, Arity, NewFunc, 'R' ),
                  {var,Line,'R'}
                 ]
               }
            ]
        },
    {ok, [{NewFunc,Arity}], [NewFunction, RenamedFunc]}.

%% Modifies the Function forms passed in so that it calls Fun only if the 
%% original function throws an exception. The exception data will be passed
%% to the advice function via the Args. It converts:
%%
%% f( X, Y, ... ) -> ...stuff...
%%
%% to
%%
%% f( P1, P2, ... ) ->
%%     try f_@( P1, P2, ... )
%%     catch Er:Rz ->
%%         Throw = {Er,Rz}
%%         OnThrow( {?MODULE, f, [P1,P2,...], f_@}, [{throw,Throw}|ThrowArgs])
%%     end.
%% f_@( X, Y, ... ) -> ...stuff...
%%
onthrow( OnThrow, {function, Line, Name, Arity, Clauses}, Module ) ->
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses ),
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
                   [ run_func( Line, NewFunc, Arity ) ],
                   [],
                   [ {clause, Line, [{var,Line,'Er'},{var, Line, 'Rz'}], [],
                     [  {match, Line, {var, Line, 'Throw'}, {tuple, Line, [{var,Line,'Er'},{var,Line,'Rz'}]}},
                        run_save_fun_append( false, Line, OnThrow, Module, Name, Arity, NewFunc, 'Throw' ) 
                     ]}
                   ],[]}
                 ]
               }
            ]
        },
    {ok, [{NewFunc,Arity}], [NewFunction, RenamedFunc]}.

%% Modifies the Function forms passed in so that it calls Fun at the end of the
%% function even if an exception is thrown. This advice does not get access to
%% the exception. It converts:
%%
%% f( X, Y, ... ) -> ...stuff...
%%
%% to
%%
%% f( P1, P2, ... ) -> 
%%     try f_@( P1, P2, ... )
%%     after OnFinal( {?MODULE, f, [P1,P2,...], f_@}, FinArgs )
%%     end.
%% f_@( X, Y, ... ) -> ...stuff...
%%
final( OnFinal, {function, Line, Name, Arity, Clauses}, Module ) ->
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses ),
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
                   [ run_func( Line, NewFunc, Arity ) ],
                   [], [],
                   [ 
                      run_save_fun( false, Line, OnFinal, Module, Name, Arity, NewFunc )
                   ]}
                 ]
               }
            ]
        },
    {ok, [{NewFunc,Arity}], [NewFunction, RenamedFunc]}.

%% @doc Injects a function into the AST that just throws an error when called.
inject_error_fun( Fun, Arity, AST ) ->
    DUMMY_LINE = 9001,
    FakeFun = {function,DUMMY_LINE,Fun, Arity,
                [{clause, DUMMY_LINE, argslist( Arity, DUMMY_LINE ), [],
                   [{call, DUMMY_LINE, {atom, DUMMY_LINE, throw},
                      [{call, DUMMY_LINE, {remote, DUMMY_LINE, {atom, DUMMY_LINE, io_lib}, {atom,DUMMY_LINE,format}},
                                [ {string, DUMMY_LINE, "AspectErl Injected function called with parameters: ~p~n"},
                                  argslist( Arity, DUMMY_LINE ) ]}]}]}]},
    NewAST = parse_trans:do_insert_forms( below, FakeFun, AST, [] ),
    parse_trans:export_function( Fun, Arity, NewAST ).


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
run_func( Line, FuncName, Arity ) ->
    {match, Line, {var, Line, 'R'},
                  {call, Line, mkatom( Line, FuncName ),
                               mklist( Line, argnames(Arity))}}.


%%% =========================================================================
%%% Internal AST Manipulation
%%% =========================================================================

gen_mkfunc( Line, Name, Arity, Clauses ) ->
    NewName = list_to_atom( atom_to_list(Name) ++ "_@" ), %TAGGED.
    Func = {function, Line, NewName, Arity, Clauses},
    {NewName, Func}.

%% @hidden
%% @doc Creates an argument list of a particular arity on a given line.
argslist( N, L ) -> argslist( N, L, [] ).
argslist( 0, _, R ) -> R;
argslist( N, L, R ) -> argslist( N-1, L, [ mkvar( L, N ) | R ] ).

%% @hidden
%% @doc Our temporary Parameters are called P# where # is its position starting 
%%   at 1. We can generate lists of them for doing tuples or cons lists.
%% @end
argname( N ) -> list_to_atom("P" ++ integer_to_list(N)).
argnames( 0 ) -> [];
argnames( N ) -> argnames(N-1)++[argname(N)].

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
%% @doc replace the Line Number in the AST.
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


