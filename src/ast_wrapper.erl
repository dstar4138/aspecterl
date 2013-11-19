%% AST Wrapping Module. 
%%  
%%  This module abstracts the usage of the Erlang Abstract Syntax Tree from
%%  the injection code. Please see the aspecterl_injector module for details.
%%
%% @author Alexander Dean
-module(ast_wrapper).

% Functions to wrap a function call in/around/after a function definition.
-export([ before/3, return/3, onthrow/3, final/3, around/3 ]).
% They all return: {ok, NewExports, NewForms}.


%% Modifies the Function forms passed in so that it calls the the Fun before
%% each clause of the Function definition. In otherwords It converts:
%%
%% f( X, Y, Z ) -> stuff...
%% To:
%% f( X, Y, Z ) -> 
%%    Args = [A,B,C],
%%    M:F( {?Module, f, [X,Y,Z]}, A )
%%    erlang:apply( f_@ae, Args ).
%% f_@ae( A, B, C ) -> stuff... 
before( Before, {function, Line, Name, Arity, Clauses}, Module ) -> 
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses ),
    NewFunction = 
    {function, Line, Name, Arity,
        [ % Arg List- pulled from clauses.
          {clause, Line, argslist( Arity, Line ),
            [], 
            [  
               % Args = [ P1, P2, ... ],
               % T = Before( {Module, Name, Args, NewFunc}, BeforeArgs ),
               % R = NewFunc( P1, P2, ... ).
               set_args( Line, Arity ),
               run_save_fun( Line, Before, Module, Name, Arity, NewFunc ), 
               run_func( Line, NewFunc, Arity )
            ]
          }
        ]
    },
    {ok, [{NewFunc,Arity}],
         [NewFunction, RenamedFunc]}.

around( Around, {function, Line, Name, Arity, Clauses}, Module ) ->
    {NewFunc, RenamedFunc} = gen_mkfunc( Line, Name, Arity, Clauses ),
    NewFunction = 
        {function, Line, Name, Arity, 
            [ % Arg List - Pulled from Clauses.
              {clause, Line, argslist( Arity, Line ),
               [],
               [
                % Args = [ P1, P2, ... ],
                % R = Around( {Module, Name, Args, NewFunc}, AroundArgs ).
                set_args( Line, Arity ),
                run_save_fun( Line, Around, Module, Name, Arity, NewFunc )
                ]
              }
            ]
        },
    {ok, [{NewFunc, Arity}], [NewFunction, RenamedFunc]}.


%TODO: Implement Other types of advice.
return( Fun, Forms, Module ) -> {ok, [], [Forms]}.
onthrow( Fun, Forms, Module ) -> {ok, [], [Forms]}.
final( Fun, Forms, Module ) -> {ok, [], [Forms]}.

%% @doc Set the Args variable equal to the list of parameters.
set_args( Line, Arity ) ->
    Args = argnames( Arity ),
    {match, Line, {var, Line, 'Args'}, mklist( Line, Args )}.

%% @doc Builds the function tuple expected by the Proceed function.
build_proceed_func( Line, Module, Name, Arity, NewFunc ) ->
    mktuple( Line, [ mkatom(Line, Module),
                     mkatom(Line, Name),
                     mklist(Line, argnames( Arity) ), 
                     mkatom(Line, NewFunc) ]).


% T = M:F( {Module, Name, Args}, A ).
run_save_fun( Line, {M,F,A}=_Before, Module, Name, Arity, NewName ) ->
    {match, Line, {var, Line, 'T'},
                  {call, Line, 
                        mkapply( Line, M, F ),
                        [ build_proceed_func( Line, Module, Name, Arity, NewName ),
                          term_to_forms(Line, A)] }}.
% R = FuncName( P1, P2, ...),
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


