%% Aspect.Erl - The Aspect Extractor
%%
%%  This is the module which gets inserted into the pre-processor to strip out
%%  annotations for advice and pointcuts so that weeving can take place. This
%%  merely updates the global pointcut and advice tables so that the AspectErl
%%  advice injector (aspecterl.erl) has advice to inject.
%%
%%  @author Alexander Dean
-module(aspecterl_extractor).
-include("defs.hrl").
-include("advice.hrl").
-include("pointcut.hrl").

% The Compile-time hook function. See include/aspect.hrl for more information.
-export([parse_transform/2]).

% Transformation State, this is built from the compile options as well as 
% environment settings for the aspect application.
-record( aspect_pt, {
           file, module, % File attributes for the current file we are on. 
           pct = [], % The pointcut table, for weeve calls look-ups
           adv = [], % Advice List, either from advice files or ADF configs.
           %% Compiler Settings %%
           verbose = false % Should we print warnings, such as unused advice?
           %,debug = false % Should the compiler explain where advice is applied?
           %% .. TODO: what other compiler flags can we work with?
}).

%% @doc Loops through the Abstract Syntax Tree and extracts the portions of
%% interest then injects advice code where it's needed.
%% @end
parse_transform( AST, Options ) ->
    case ?AspectsOn of
        true  -> parse( AST, Options );
        false -> AST
    end.

parse( AST, Options ) ->
    State = check_options( AST, Options ), 
    {NewAST, PCts} = extract_pointcuts( AST, State ),
    {FinalAST, Adv} = extract_advice( NewAST, State ),
    NewState = update_advice( State, PCts, Adv ),
    ok = update_global_table( NewState ),
    FinalAST.
 
%%% =========================================================================
%%% Parse Passes
%%% =========================================================================

%% @private
%% @doc Take in the initial state after parsing options, and run through the
%%   AST to retrieve all PointCuts and Advice. This will delete the attributes
%%   it recognizes as it goes.
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
    end.
                
extract_advice( AST, State ) -> eadv( AST, State, {[],[]} ).
eadv( [], _, {RevAST, Adv} ) -> {lists:reverse(RevAST), Adv};
eadv( [{attribute, ALine, advice, Args},N|R], State, {AST,Adv} ) -> 
    case is_adv_func( N, ALine, State ) of
        {true, F} -> 
            (case create_advice( ALine, Args, F ) of
                 {ok, A} -> eadv( R, State, { [N|AST], [A|Adv] } );
                 {error, E} -> eadv( R, State, {[E|AST], Adv} )
             end);
        {false, E} -> eadv( [N|R], State, {[E|AST], Adv} )
    end;
eadv( [H|R], State, {AST, Adv} ) -> eadv( R, State, {[H|AST], Adv} ).

%%% =========================================================================
%%% Internal Functionality
%%% =========================================================================

update_advice( State, Pcs, Adv ) ->
    %TODO: Check for conflicting pointcuts/advice?
    State#aspect_pt{pct=Pcs, adv=Adv}.


update_global_table( #aspect_pt{ pct=_Pcs, adv=_Adv } ) ->
    %TODO: somehow save Pcs. and Adv so that aspecterl_injector can use it.
    ok.


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
    case lists:member(V, [module, func, behaviour, arity]) of
        true -> (case check_re( Re ) of
                     ok -> check_pointcut_args( R, set_pc(Pc, V, Re) );
                     {error, Reason} -> {error, Reason}
                 end);
        false -> {error, io_lib:format("Invalid pointcut argument '~p'", [V])}
    end;
check_pointcut_args( [A|_], _ ) ->
    {error, io_lib:format("Invalid pointcut argument '~p'",[A])}.

check_re( _ ) -> true.

set_pc( Pc, module, Re ) -> Pc#pointcut{module=Re};
set_pc( Pc, func, Re ) -> Pc#pointcut{func=Re};
set_pc( Pc, behaiour, Re ) -> Pc#pointcut{behaviour=Re};
set_pc( Pc, arity, Re ) -> Pc#pointcut{arity=Re}.



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
create_advice( ALine, Args, {M,F} ) ->
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


%% @hidden
%% @doc Verbosely display information about current transformations.
inform( #aspect_pt{verbose=true}, Msg, Args ) -> io:format( Msg, Args );
inform( _, _, _ ) -> ok.   

%% @hidden
%% @doc Default to rebar verbosity/debug level, otherwise look at applied
%%   compiler flags. This will build the default state for the parse
%%   transformer.
%% @end
check_options( AST, Options ) ->
    #aspect_pt{ 
        file = parse_trans:get_file( AST ),
        module = parse_trans:get_module( AST ),
        verbose = check_verbosity( Options ) 
    }.
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
         
