%% Aspecterl_injector.Erl - The Aspect Erl Advice Injector.
%%
%% This module walks through code and injects advice if there are matching 
%% pointcuts. This must be called after either erladf or aspecterl_extractor
%%
%% @author Alexander Dean
-module(aspecterl_injector).
-include("defs.hrl").
-include("advice.hrl").

-export([parse_transform/2]).

-record( aspect_pt, {
    file, 
    module,
    behaviours = [],
    inject_missing = false,
    exported = [],
    verbose = false
}).

parse_transform( AST, Options ) ->
    case ?AspectsOn of 
        true -> parse( AST, Options );
        false -> AST
    end.

parse( AST, Options ) ->
    {Nope, State} = parse_trans:get_module( AST ),
    case Nope of
        true  -> 
            Module = State#aspect_pt.module,
            io:fwrite("Ignoring file in injector = ~p\n",[Module]), 
            AST;
        false -> 
            injector( AST, State )
    end.

injector( AST, State ) ->
   case funloop( AST, State, [], [] ) of
       {ok, NewAST} -> NewAST;
       Error -> Error
    end.

% Loop over AST pulling out functions and checking them
funloop( [], _, Acc, E ) -> 
    AST = lists:reverse(Acc),
    NewAST = insert_exports( AST, E ),
    {ok, NewAST};
funloop( [{attribute,Line,decorate,Args}|Rest], State, Acc, E ) ->
    %TODO: Wrap next function with advice from Args.
    funloop( Rest, State, Acc, E );
funloop( [{function,Line,Name,Arity}=F|Rest], State, Acc, E ) ->
    Data = build_data( Name, Arity, State),
    case aspecterl:check_pointcut( Data ) of
        [] -> % No pointcuts apply to this function.
            funloop( Rest, State, [F|Acc], E );
        Pcts ->
            {Exports, Forms} = test_pointcuts( Pcts, F ),
            funloop( Rest, State, [Forms|Acc], [Exports|E] )
    end;
funloop( [H|R], State, Acc, E ) -> funloop( R, State, [H|Acc], E ).


build_data( Func, Arity, #aspect_pt{module=M,behaviours=B,exported=Es} ) ->
    Scope = case lists:member( {Func,Arity}, Es ) of
                true -> 'public'; false -> 'private'
            end,
    { B, M, Func, Arity, Scope }.

%% Runs through a list of applicable poincuts and applies them to the given 
%% function if there are advice that use the pointcuts. It will apply ALL advice
%% which means they will compound in possibly an unanticipated order.
test_pointcuts( P, F ) -> test_pointcuts( P, F, [] ).
test_pointcuts( [], F, E) -> {E, F};
test_pointcuts( [P|Ps], F, E ) -> 
    {Es, Fs} = apply_pct( P, [F] ),
    test_pointcuts( Ps, Fs++F, Es++E ).
apply_pct( P, Forms ) ->
    case aspecterl:get_advice( P ) of
        [] -> {[], [Forms]};
        Advice -> wrap_advice_list( Advice, Forms, [] )
    end.
wrap_advice_list( [], F, E ) -> {E,F};
wrap_advice_list( [A|Adv], F, E ) -> 
    {Es,Fs} = weave( A, hd(F) ),
    wrap_advice_list( Adv, Fs++F, Es++E ).

%% Weave Advice within an Erlang Forms
weave( #advice{ type=T, module=M, name=F, args=A }, Forms ) ->
    case T of
        'before'       -> ast_wrapper:before( {M,F,A}, Forms );
        'after_return' -> ast_wrapper:return( {M,F,A}, Forms );
        'after_throw'  -> ast_wrapper:onthrow( {M,F,A}, Forms );
        'after_final'  -> ast_wrapper:final( {M,F,A}, Forms );
        'around'       -> ast_wrapper:around( {M,F,A}, Forms )
    end.

insert_exports( AST, Exports ) ->
    AST. %TODO: Insert the exports [{Atom(), Arity()},...]

%%% =========================================================================
%%% Option Parsing
%%% =========================================================================
%%% TODO: Move to utility module for both transformers.

%% @hidden
%% @doc Verbosely display information about current transformations.
inform( #aspect_pt{verbose=true}, Msg, Args ) -> io:format( Msg, Args );
inform( #aspect_pt{verbose=rebar}, Msg, Args ) -> 
    rebar_log:log( debug, Msg, Args );
inform( _, _, _ ) -> ok.   

%% @hidden
%% @doc Default to rebar verbosity/debug level, otherwise look at applied
%%   compiler flags. This will build the default state for the parse
%%   transformer.
%% @end
check_options( AST, Options ) ->
    Attr       = [ Args || {attribute, _, ?AspectErlAttr, Args} <- AST],
    Behaviours = [B || {attribute, _, behaviour, B} <- AST],
    Exported   = [E || {attribute, _, export, EL} <- AST, E <- EL],
    Nope = lists:member(exclude, lists:flatten(Attr)),
    State = #aspect_pt{ 
        file = parse_trans:get_file( AST ),
        module = parse_trans:get_module( AST ),
        behaviours = Behaviours,
        inject_missing=lists:member(inject_missing, lists:flatten(Attr)),
        exported = Exported,
        verbose = check_verbosity( Options ) 
    },
    {Nope, State}.

%% @hidden 
%% @doc Check compile flags for verbosity. See check_options/1. Defaults to 
%%      rebar's value.
%% @end
check_verbosity( Options ) ->
    case application:get_env( rebar_global, verbose ) of
        undefined -> get_opt( verbose, Options );
        {ok, _ }  -> rebar
    end.

%% @hidden
%% @doc Check if a option is apart of the option list.
get_opt( Opt, Options ) -> lists:member( Opt, Options ).
     
