%% Aspect.Erl - The Aspect Extractor
%%
%%  This is the module which gets inserted into the pre-processor to strip out
%%  annotations for advice and pointcuts so that weeving can take place. This
%%  merely updates the global pointcut and advice tables (aspecterl.erl) so 
%%  that the AspectErl advice injector (aspecterl_injector.erl) has advice to 
%%  inject.
%%
%%  @author Alexander Dean
-module(aspecterl_extractor).
-include("defs.hrl").
-include("advice.hrl").
-include("pointcut.hrl").

-import( aspecterl_util, [ inform/3 ] ).

-export([run/2]).

%% @doc Loops through the Abstract Syntax Tree and extracts the portions of
%% interest then injects advice code where it's needed.
%% @end
run( AST, State ) ->
    {NewAST, Pcts} = extract_pointcuts( AST, State ),
    {FinalAST, Adv} = extract_advice( NewAST, State ),
    aspecterl:update_global_table( Pcts, Adv ),
    FinalAST.
 
%%% =========================================================================
%%% Parse Passes
%%% =========================================================================

%% @private
%% @doc Take in the initial state after parsing options, and run through the
%%   AST to retrieve all PointCuts. This will delete the attributes it 
%%   recognizes as it goes.
%% @end
extract_pointcuts( AST, State ) -> epts( AST, State, {[],[]} ).
epts( [], _, {RevAST, PCts} ) -> {lists:reverse( RevAST ), PCts};
epts( [{attribute, Line, pointcut, Args}|R], S, {A,P} ) ->
    case Args of
        { Name, Checks } when is_list(Checks) ->
            (case create_pointcut( Line, Name, Checks, S ) of
                  {ok, Pc} -> epts( R, S, {A,[Pc|P]} );
                  {error,Err} -> epts( R, S, {[Err|A], P} )
             end);
        _ ->
            Err = {error, {Line, S#aspect_pt.module, 
                             "Invalid pointcut definition, "++
                             "must be a tuple of the form: "++
                             "{Name, Checks}"}},
            epts( R, S, {[Err|A], P} )
    end;
epts( [H|R], State, {AST, P} ) -> epts( R, State, {[H|AST], P} ).
     
%% @hidden
%% @doc Takes the AST spit out from extract_pointcuts/2 and finds all Advice.
%%   This will delete the attributes it recognizes as it goes.
%% @end   
extract_advice( AST, State ) -> eadv( AST, State, {[],[]} ).
eadv( [], _, {RevAST, Adv} ) -> {lists:reverse(RevAST), Adv};
eadv( [{attribute, ALine, advice, Args},N|R], State, {AST,Adv} ) -> 
    case is_adv_func( N, ALine, State ) of
        {true, F} -> 
            (case create_advice( ALine, Args, F, State ) of
                 {ok, A} -> eadv( R, State, { [N|AST], [A|Adv] } );
                 {error, E} -> eadv( R, State, {[E|AST], Adv} )
             end);
        {false, E} -> eadv( [N|R], State, {[E|AST], Adv} )
    end;
eadv( [H|R], State, {AST, Adv} ) -> eadv( R, State, {[H|AST], Adv} ).

%%% =========================================================================
%%% Internal Functionality
%%% =========================================================================

%% @hidden 
%% @doc Creates a pointcut object from an attribute found on a particular line.
create_pointcut( Line, Name, Checks, S ) ->
    inform( S, "Found pointcut ~p: ~p", [Name, Checks] ),
    case check_pointcut_args( Checks, #pointcut{} ) of
        {ok, Pointcut} -> {ok, Pointcut#pointcut{name=Name}};
        {error, Reason} -> 
            ErrorMsg = {error, {Line, S#aspect_pt.module, Reason}},
            {error, ErrorMsg}
    end.
check_pointcut_args( [], Pc ) ->
    case % Check if there is a matcher on at least one of the categories
        lists:foldl( fun( X, A ) -> X/=nil orelse A end, false,
                     [Pc#pointcut.module, 
                      Pc#pointcut.func,
                      Pc#pointcut.behaviour,
                      Pc#pointcut.arity] )
    of
        true -> {ok, Pc};
        false -> {error, "Pointcut definition does not contain a location check."}
    end;
check_pointcut_args( [{V, Re}|R], Pc ) ->
    case lists:member(V, [module, func, behaviour, arity, scope]) of
        true -> (case check_re( V, Re ) of
                     true -> check_pointcut_args( R, set_pc(Pc, V, Re) );
                     {error, Reason} -> {error, Reason}
                 end);
        false -> {error, io_lib:format("Invalid pointcut argument '~p'", [V])}
    end;
check_pointcut_args( [A|_], _ ) ->
    {error, io_lib:format("Invalid pointcut argument '~p'",[A])}.

%% @hidden
%% @doc Utility function for testing regular expressions for each pointcut
%%   matcher name. Only 'scope' has particular restrictions.
%% @end  
check_re( scope, R ) ->
    case R of
        'public' -> true;
        'private' -> true;
        'any' -> true;
        _ -> false
    end;
check_re( _, "*") -> true;
check_re( _, nil) -> true;
check_re( _, R ) -> 
    case re:compile( R ) of
        {ok, _ } -> true;
        {error, Reason} -> {error, Reason}
    end.

%% @hidden
%% @doc Utility function for updating members of the pointcut class. 
set_pc( Pc, module, Re ) -> Pc#pointcut{module=Re};
set_pc( Pc, func, Re ) -> Pc#pointcut{func=Re};
set_pc( Pc, behaviour, Re ) -> Pc#pointcut{behaviour=Re};
set_pc( Pc, arity, Re ) -> Pc#pointcut{arity=Re};
set_pc( Pc, scope, Re ) -> Pc#pointcut{scope=Re}.

%% @hidden
%% @doc Checks if a Form is a function definition or not. Then checks if it is
%%   a valid advice function. If it is it will return a {Module, FuncName} 
%%   tuple for quick access.
%% @end
is_adv_func( {function, Line, Name, Arity, _}, _, State ) ->
    Module = State#aspect_pt.module,
    case Arity == 2 of
        false -> % Advice functions take two parameters: fun( MFA, Args )
            {false, {error, {Line, Module, 
                         "Advice function must only take two arguments."}}};
        true ->
            inform( State, "Found Advice function: ~p/~p", [Name,Arity] ),
            {true, {Module, Name}}
    end;
is_adv_func( _, Line, State ) -> 
    Module = State#aspect_pt.module,
    E = {error, {Line, Module, "Missing function for advice."}},
    {error, E}. 

%% @hidden
%% @doc Creates an Advice object for adding to the global table.
create_advice( ALine, Args, {M,F}, S ) ->
    inform( S, "Found advice ~p:~p => ~p",[M,F,Args]),
    case check_advice_args( Args, #advice{} ) of
        {ok, Advice} -> 
            {ok, Advice#advice{ module = M, name = F }};
        {error, Reason} ->
            ErrorMsg = {error, {ALine, M, Reason}},
            {error, ErrorMsg}
    end.
check_advice_args( [], A ) -> 
    case A#advice.type == undefined of
        true  -> {error, "Advice is missing 'type' argument."};
        false -> (case A#advice.pointcuts of
                      [] -> {error, "Advice has not pointcut assigned."};
                      _ -> {ok, A}
                   end)
    end;
check_advice_args( [{pointcuts, P}|R], A ) ->
    case
        lists:foldl( fun(X,F)-> is_atom(X) andalso F end, true, P )
    of
        true -> check_advice_args( R, A#advice{pointcuts=P} );
        false -> {error, "Invalid pointcuts listed in advice, must all be atoms."}
    end;
check_advice_args( [{type, T}|R], A ) ->
    case lists:member( T, ?ADVICE_TYPES ) of
        true -> check_advice_args( R, A#advice{type=T} );
        false -> {error, io_lib:format("Invalid advice type '~p'.",[T])}
    end;
check_advice_args( [{args, Args}|R], A ) ->
    check_advice_args( R, A#advice{args = Args} );
check_advice_args( [ Bad | _], _ ) ->
    {error, io_lib:format("Unknown argument '~p' to advice.",[Bad])}.

