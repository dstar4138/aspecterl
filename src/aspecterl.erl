-module(aspecterl).
-include("advice.hrl").
-include("pointcut.hrl").

-export([compile/1]).

% Internal Functionality for accessing the ETS table.
-export([ update_global_table/2, check_pointcut/1, 
          get_advice/1, get_advice/2 ]).

%Table Name definitions for ETS.
-define(PCT_Table, aspecterl_pct_table).
-define(ADV_Table, aspecterl_adv_table).
-define(AspectErl_TableOpts, [ public, duplicate_bag, 
                               {write_concurrency, false},
                               {read_concurrency, false}, 
                               {keypos, 1}, {heir, none} ]).



compile( _Dir ) ->
    ok. %TODO: Compile with ADF File.


%%% =========================================================================
%%% Global Aspect Table Functions
%%% =========================================================================


update_global_table( Adv, Pct ) ->
    {ok, P} = get_pct_table(),
    true = ets:insert(P, Pct),
    {ok, A} = get_adv_table(),
    true = ets:insert(A, Adv).

get_pct_table() -> get_table(?PCT_Table).
get_adv_table() -> get_table(?ADV_Table).
get_table( Table ) ->
    case ets:info( Table ) of
        undefined ->  ID = ets:new( Table, ?AspectErl_TableOpts ), {ok, ID};
        _ -> {ok, Table}
    end.

%% Given all the information about a function, return if any pointcuts match.
check_pointcut( Data ) ->
    {ok, T} = get_pct_table(),
    ets:foldl( fun( Row , Acc ) ->
                    case check_pct( Row, Data ) of
                        {ok, Name} -> [Name|Acc];
                        false      -> Acc
                    end
                end,
               [], T ).
check_pct( #pointcut{ name=N, module=M, func=F, behaviour=B, arity=A, scope=S },
           {Behaviours, Module, Function, Arity, Scope} ) ->
    case
        check_re( F, Function    ) andalso
        check_re( M, Module      ) andalso
        check_rel( B, Behaviours ) andalso
        check_val( S, Scope      ) andalso
        check_mem( A, Arity      ) 
    of
        true -> {ok, N};
        false -> false
    end.

check_rel( nil, [] ) -> true;
check_rel( RE, L ) when is_list(L) ->
    lists:foldl( fun ( B, Acc ) ->
                         Acc andalso check_re(RE, B) 
                  end, true, L );
check_rel( _, _)-> false.

check_re( nil, _ ) -> true;
check_re( RE, Val ) when is_atom(Val) ->
    check_re( RE, erlang:atom_to_binary( Val ) );
check_re( RE, Val ) ->
    case re:run( Val, RE ) of
        {match, _} -> true;
        _ -> false
    end.

check_val( 'any', _ ) -> true;
check_val( A, A ) -> true;
check_val( _, _ ) -> false.
    
check_mem( [], _ ) -> true;
check_mem( L, M) when is_list(L) ->
    lists:member( M, L ).

%% Get all advice which are triggered by the pointcut name,
get_advice( PctName ) ->
    {ok, T} = get_adv_table(),
    ets:foldl( fun( Row, Acc ) ->
                       case check_adv( Row, PctName ) of
                           true -> [Row|Acc];
                           false -> Acc
                        end
                end, [], T).
check_adv( #advice{pointcuts=Ps}, P ) ->
    lists:member( P, Ps ).

% Get advice by Module and Function name, such as when advice happens.
% if module is blank, pass in nil.
get_advice( Module, Function ) ->
    {ok, T} = get_adv_table(),
    ets:foldl( fun( Row, Acc ) ->
                       case check_adv( Row, Module, Function ) of
                           true -> [Row|Acc];
                           false -> Acc
                        end
                end, [], T ).
check_adv( #advice{module=M, name=N}, Mod, Func ) ->
    N =:= Func andalso (Mod == nil orelse M =:= Mod).

