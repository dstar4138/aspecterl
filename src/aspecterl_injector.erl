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
    NewAST = aspecterl_extractor:parse_transform( AST, Options ),
    {Nope, State} = check_options( NewAST, Options ),
    case Nope of
        true  -> 
            Module = State#aspect_pt.module,
            inform(State, "Ignoring file in injector => ~p\n",[Module]), 
            NewAST;
        false -> 
            FinalAST = injector( NewAST, State ),
            case FinalAST of 
                NewAST -> inform(State,"Nothing Weaved in ~p.~n",[
                                                      State#aspect_pt.module]);
                _DifAST-> inform(State,"Final Weaving = ~p~n",[FinalAST])
            end,
            FinalAST
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
    {AdvMod,AdvFun,AdvArgs} = case Args of 
                                {F,A}   -> {nil,F,A};
                                {M,F,A} -> {M,F,A} 
                              end,
    case aspecterl:get_advice(AdvMod,AdvFun) of
        [] -> Error = {error,{Line, State#aspect_pt.module,"Decorator uses "++
                              "unknown advice function."}},
              funloop( Rest, State, [Error|Acc], E);
        Adv -> 
               [NextF | LRest] = Rest,
               case NextF of 
                   {function,_,_,_,_} -> 
                        NAdv = update_adv_args(Adv,AdvArgs),
                        {NF, Funs, Es} = 
                                wrap_advice_list(State, NAdv, NextF, [], []),
                        funloop( [NF|LRest], State, Funs++Acc, Es++E ); 
                   _ -> 
                        Error = {error,{Line,State#aspect_pt.module,
                                "Decorator must decorate a function."}},
                        funloop( Rest, State, [Error|Acc], E )
                end
    end;
funloop( [{function,_Line,Name,Arity,_Clauses}=F|Rest], State, Acc, E ) ->
    Data = build_data( Name, Arity, State),
    case aspecterl:check_pointcut( Data ) of
        [] -> % No pointcuts apply to this function.
            funloop( Rest, State, [F|Acc], E );
        Pcts ->
            inform( State, "Found function which matches pointcuts: ~p", [{Name, Arity}]),
            {Forms, Exports} = test_pointcuts( Pcts, F , State),
            funloop( Rest, State, Forms++Acc, Exports++E )
    end;
funloop( [H|R], State, Acc, E ) -> funloop( R, State, [H|Acc], E ).


build_data( Func, Arity, #aspect_pt{module=M,behaviours=B,exported=Es} ) ->
    Scope = case lists:member( {Func,Arity}, Es ) of
                true -> 'public'; false -> 'private'
            end,
    { B, M, Func, Arity, Scope }.

update_adv_args( Adv, Args ) -> 
    lists:foldl( fun( #advice{args=Old}=A, Acc ) ->
                        [A#advice{args=Args++Old}|Acc]
                 end, [], Adv ).

%% Runs through a list of applicable poincuts and applies them to the given 
%% function if there are advice that use the pointcuts. It will apply ALL advice
%% which means they will compound in possibly an unanticipated order.
test_pointcuts( Ps, F, State ) -> 
    {NewF, Fs, Es} = 
        lists:foldl( fun( Pct, {NewF, Fs, Es}=S ) ->
                         case aspecterl:get_advice( Pct ) of
                            []   -> S;
                            Advs -> wrap_advice_list( State, Advs, NewF, Fs, Es )
                          end
                     end, {F,[],[]}, Ps),
    {[NewF|Fs],Es}.

wrap_advice_list( _, [], Fun, Forms, Exports ) -> {Fun, Forms, Exports};
wrap_advice_list( State, [A|Adv], Fun, Fs, E ) -> 
    % Assumes Original function is on top.
    {ok, Es, [NewFun|NewForms]} = weave( A, Fun, State ),
    inform(State, "AspectErl weaved: ~p~n with ~p~n",[fun_name(State, Fun), 
                                                      adv_name(A)]),
    wrap_advice_list( State, Adv, NewFun, NewForms++Fs, Es++E ).

%% Weave single Advice into a single Erlang Form.
weave( #advice{ type=T, module=M, name=F, args=A }, Forms, State ) ->
    Module = State#aspect_pt.module,
    case T of
        'before'       -> ast_wrapper:before( {M,F,A}, Forms, Module );
        'after_return' -> ast_wrapper:return( {M,F,A}, Forms, Module );
        'after_throw'  -> ast_wrapper:onthrow( {M,F,A}, Forms, Module );
        'after_final'  -> ast_wrapper:final( {M,F,A}, Forms, Module );
        'around'       -> ast_wrapper:around( {M,F,A}, Forms, Module )
    end.

insert_exports( AST, Exports ) ->
    lists:foldl( fun( {Func,Arity}, Forms ) -> 
                         parse_trans:export_function( Func, Arity, Forms ) 
                 end, AST, Exports).

fun_name( #aspect_pt{module=M},
          {function, _line, Name, _Arity, _Clauses} ) -> {M, Name}.
adv_name( #advice{ module = Module, name = Name } ) -> {Module, Name}.


%%% =========================================================================
%%% Option Parsing
%%% =========================================================================
%%% TODO: Move to utility module for both transformers.

%% @hidden
%% @doc Verbosely display information about current transformations.
%inform( #aspect_pt{verbose=true}, Msg, Args ) -> io:format( Msg, Args );
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
     
