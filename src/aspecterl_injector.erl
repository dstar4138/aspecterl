%% Aspecterl_injector.Erl - The Aspect Erl Advice Injector.
%%
%% This module walks through code and injects advice if there are matching 
%% pointcuts. This must be called after either erladf or aspecterl_extractor
%%
%% @author Alexander Dean
-module(aspecterl_injector).
-include("defs.hrl").
-include("advice.hrl").

-import( aspecterl_util, [ inform/3, check_options/2 ] ).

-export([parse_transform/2]).

%% @doc Loops through the Abstract Syntax Tree and injects the advice based on
%%   pointcuts and decoration attributes. 
%% @end
parse_transform( AST, Options ) ->
    case ?AspectsOn of 
        true -> parse( AST, Options );
        false -> AST
    end.

parse( AST, Options ) ->
    NewAST = aspecterl_extractor:parse_transform( AST, Options ),%% XXX: CLEAN ME 
    {_, Extracted, State} = check_options( NewAST, Options ),
    NewerAST = update_if_missing( State, NewAST ),
    case Extracted of
        true  -> 
            Module = State#aspect_pt.module,
            inform(State, "Ignoring file in injector => ~p\n",[Module]), 
            NewerAST;
        false -> injector( NewerAST, State )
    end.

injector( AST, State ) ->
   case funloop( AST, State, [], [] ) of
       {ok, AST} -> 
           inform(State, "Nothing Weaved in ~p.~n",
                         [State#aspect_pt.module]),
           AST;
       {ok, NewAST} -> 
           inform(State,"Final Weaving = ~p~n",[NewAST]),
           NewAST;
       Error -> Error
    end.

%% @doc Loop over AST pulling out functions and checking them
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
    {ok, Es, [NewFun|NewForms]} = weave( A, Fun, State, Fs ),
    inform(State, "AspectErl weaved: ~p~n with ~p~n",[fun_name(State, Fun), 
                                                      adv_name(A)]),
    wrap_advice_list( State, Adv, NewFun, NewForms++Fs, Es++E ).

%% Weave single Advice into a single Erlang Form.
weave( #advice{ type=T, module=M, name=F, args=A }, Forms, State, CurAcc ) ->
    Module = State#aspect_pt.module,
    case T of
        'before'       -> ast_wrapper:before( {M,F,A}, Forms, Module, CurAcc );
        'after_return' -> ast_wrapper:return( {M,F,A}, Forms, Module, CurAcc );
        'after_throw'  -> ast_wrapper:onthrow( {M,F,A}, Forms, Module, CurAcc );
        'after_final'  -> ast_wrapper:final( {M,F,A}, Forms, Module, CurAcc );
        'around'       -> ast_wrapper:around( {M,F,A}, Forms, Module, CurAcc )
    end.

insert_exports( AST, Exports ) ->
    lists:foldl( fun( {Func,Arity}, Forms ) -> 
                         parse_trans:export_function( Func, Arity, Forms ) 
                 end, AST, Exports).

fun_name( #aspect_pt{module=M},
          {function, _line, Name, _Arity, _Clauses} ) -> {M, Name}.
adv_name( #advice{ module = Module, name = Name } ) -> {Module, Name}.

%%% =========================================================================
%%% Behaviour Exports checking
%%% =========================================================================

%% @hidden
%% @doc Check if we need to inject missing behaviours, and if we do, go ahead
%%   and do it. We need to first verify we have all the functions neccessary.
%% @end
update_if_missing(#aspect_pt{inject_missing=false}, AST) -> AST;
update_if_missing(#aspect_pt{module=Module,behaviours=Bhvs,exported=Expts}, AST ) ->
    lists:foldl( fun( {Fun, Arity}, AltAST ) ->
                         ast_wrapper:inject_error_fun( Module, Fun, Arity, AltAST )
                 end, AST, validate_missing( Bhvs, Expts )).

%% @hidden
%% @doc For each behaviour listed, check the export list to verify it's there.
%%   We remove the export from the temp list so that we can cause errors if two
%%   behaviours need the same name/arity function.
%% @end %TODO: should this be changed to allow for multi-behaviour overloading?
validate_missing( Behaviours, Exports ) -> 
    io:fwrite("Checking behaviours(~p) for missing exports in (~p)~n",
              [Behaviours, Exports]),
    validate_missing( Behaviours, Exports, [] ).
validate_missing( [], _, M ) -> 
    io:fwrite("Found Missing functions, now injecting: ~p~n",[M]),
    M;
validate_missing( [B|R], Es, Ms) -> 
    {Missing, NEs} =
      lists:foldl( fun( Item, {Missing, Expts} ) ->
                           case lists:member( Item, Expts ) of
                               true -> {Missing, lists:delete(Item, Expts)};
                               false -> {[Item|Missing], Expts}
                           end
                   end, {[], Es}, required_exports( B )),
    validate_missing( R, NEs, Missing++Ms ).


%% @hidden
%% @doc Check for the exports required by each behaviour. 
required_exports( application ) -> [ {start,2}, {stop,1} ];
required_exports( supervisor  ) -> [{init,1}];
required_exports( gen_server  ) -> [ {init,1}, {handle_call,3}, 
                                     {handle_cast,2}, {handle_info,2},
                                     {terminate,2}, {code_change,3} ];
required_exports( gen_fsm ) -> [ {init,1}, {handle_event,3}, 
                                 {handle_sync_event,4},{handle_info,3},
                                 {terminate,3}, {code_change,4} ];
required_exports( gen_event ) -> [ {init,1}, {handle_event,2},
                                   {handle_call,2}, {handle_info,2},
                                   {terminate,2}, {code_change,3} ];
required_exports( _Unknown ) -> []. 

