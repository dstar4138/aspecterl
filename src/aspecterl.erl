%% AspectErl
%%  This module does two things: It provides access to the internal global
%%  pointcut and advice tables stored in ETS to the parse transformers. It also
%%  gives user and system callback functions such as compile/1.
%%
%% @author Alexander Dean
-module(aspecterl).
-include("advice.hrl").
-include("pointcut.hrl").

% User and System callbacks.
-export([compile/1]).

% Internal Functionality for accessing the ETS table.
-export([ update_global_table/2, check_pointcut/1, 
          get_advice/1, get_advice/2 ]).

%% INTERNAL EXPORTS ONLY, ETS NEEDS A HOST PROCESS. %%%%%%%%%%%%%%%
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Table Name definitions for ETS.
-define(PCT_Table, aspecterl_pct_table).
-define(ADV_Table, aspecterl_adv_table).
-define(AspectErl_TableOpts, [ public, duplicate_bag, named_table ]).

%%% =========================================================================
%%% User and System Callbacks.
%%% =========================================================================

compile( Dirs ) when is_list(Dirs) ->
    {S,A,E} = case Dirs of
        [Adv] -> % Assumes from adv dir that ../src and ../ebin exist.
            Src  = Adv++"../src",
            Ebin = Adv++"../ebin",
            {Src, Adv, Ebin};
        [Src, Adv] -> % Assumes from src dir that src/../ebin exists.
            Ebin = Src++"../ebin",
            {Src, Adv, Ebin};
        [Src, Adv, Ebin] -> {Src, Adv, Ebin};
        _ -> io:fwrite("Uknown compile inputs, need 1-3 inputs.~n",[])
    end,
    compile( S, A, E ).

compile( Src, Adv, Ebin ) ->
    case verify( [Src, Adv, Ebin] ) of
        false -> ok;
        true ->
            (case erladf:is_advice_dir( Adv ) of 
                 true -> build( Adv, Src, Ebin );
                 false -> 
                     io:fwrite("Advice Dir(~p) does not contain an ADF file!",
                               [Adv])
             end)
    end.

%%% =========================================================================
%%% Global Aspect Table Callbacks.
%%% =========================================================================

update_global_table( Pct, Adv ) ->
    {ok, _Pid} = check_ets_server(),
    send_records( Adv, Pct ).

check_ets_server() ->
    case whereis( ?MODULE ) of
        undefined -> start();
        Pid -> {ok, Pid}
    end.

send_records( Adv, Pct ) -> 
    check_ets_server(),
    gen_server:cast(?MODULE, {data, Adv, Pct}), ok.
check_pointcut( Data ) -> 
    check_ets_server(),
    gen_server:call( ?MODULE, {chk_pct, Data}).
get_advice( PctName ) -> 
    check_ets_server(),
    gen_server:call( ?MODULE, {get_adv, PctName}).
get_advice( Module, Function ) -> 
    check_ets_server(),
    gen_server:call(?MODULE, {get_adv, Module, Function}). 

%%% =========================================================================
%%% gen_server Callback functions
%%% =========================================================================

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

% Defaults. %%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([])->{ok, nil}.
handle_info(_I, S) -> {noreply, S}.
terminate(_R, _S) -> ok.
code_change(_,S,_)->{ok, S}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% We only care about casts from the extractor.
handle_cast( {data, Adv, Pct}, State ) ->
    insert_into_table( Adv, Pct ),
    {noreply, State};
handle_cast( _,  S ) -> {noreply, S}.


% We only care about a call from the injector.
handle_call({chk_pct, Data}, _F, S) ->
    Res = internal_check_pointcut( Data ),
    {reply, Res, S};
handle_call({get_adv, PctName}, _F, S)->
    Res = internal_get_advice( PctName ),
    {reply, Res, S};
handle_call({get_adv, M, Fun}, _F, S)->
    Res = internal_get_advice( M, Fun ),
    {reply, Res, S};
handle_call(_R,_F,S) -> {noreply, S}.


%%% =========================================================================
%%% Global Aspect Table Functions
%%% =========================================================================

insert_into_table( Adv, Pct ) ->
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
internal_check_pointcut( Data ) ->
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
    check_re( RE, atom_to_list( Val ) );
check_re( RE, Val ) ->
    case re:run( Val, RE ) of
        {match, _} -> true;
        _ -> false
    end.

check_val( 'any', _ ) -> true;
check_val( A, A ) -> true;
check_val( _, _ ) -> false.

check_mem( nil, _ ) -> true;
check_mem( [], _ )  -> true;
check_mem( L, M) when is_list(L) ->
    lists:member( M, L ).

%% Get all advice which are triggered by the pointcut name,
internal_get_advice( PctName ) ->
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
internal_get_advice( Module, Function ) ->
    {ok, T} = get_adv_table(),
    ets:foldl( fun( Row, Acc ) ->
                       case check_adv( Row, Module, Function ) of
                           true -> [Row|Acc];
                           false -> Acc
                        end
                end, [], T ).
check_adv( #advice{module=M, name=N}, Mod, Func ) ->
    N =:= Func andalso (Mod == nil orelse M =:= Mod).

%% ==========================================================================
%% Compilation Pipeline
%% ==========================================================================

%% @hidden
%% @doc Check if all strings in list are actually valid directory paths.
verify( [] ) -> true;
verify( [H|R] ) ->
    case filelib:is_dir( H ) of
        true -> verify( R );
        false -> 
            io:fwrite("Invalid Directory: ~p~n",[H]),
            false
    end.

%% @hidden
%% @doc Pull out advice information from ADF files and update global table.
%%   Then compile advice and source files so that we have BEAMs of everything.
%% @end 
build( AdviceDir, SourceDir, OutDir ) ->
    {ok, {Adv,Pcs}} = erladf:parse_all( AdviceDir ),
    aspecterl:update_global_table(Pcs, Adv),
    compile_files( AdviceDir, OutDir, false),
    compile_files( SourceDir, OutDir, true ).

%% @hidden
%% @doc Compile each file in Source Directory and place its BEAM in the output
%%   Directory. If Inject is true, then run the aspecterl injector on the file.
%% @end
compile_files( SourceDir, OutDir, Inject ) ->
   Erls = get_erl_files( SourceDir ),
   Opts = [{outdir, OutDir}] ++
            (if Inject -> [{parse_transform, aspecterl_injector}];
                true   -> []
             end),
   ok = compile_each( Erls, Opts ).

%% @hidden
%% @doc Compile each file in a list with the given Options.
compile_each( [], _ ) -> ok;
compile_each( [File|Rest], Options ) ->
    do_compile( File, Options ),
    compile_each( Rest, Options ).

%% @hidden
%% @doc Compile a particular Erlang file with the given Options.
do_compile( File, Options ) ->
    case compile:file( File, Options ) of
        {ok, _Module} -> ok;
        {ok, _Module, Warnings} -> io:fwrite("Warnings: ~p~n",[Warnings]);
        {ok, _Module, _Binary, Warns} -> io:fwrite("Warnings: ~p~n",[Warns]);
        error -> io:fwrite("Unknown Error.~n",[]);
        {error, Errors, Warnings} ->
            io:fwrite("Errors: ~p~nWarnings: ~p~n",[Errors, Warnings])
    end.

%% @hidden
%% @doc Grab all files with *.erl extension within a directory.
get_erl_files( Dir ) ->
    filelib:fold_files( Dir, "^.*\.erl$", false, fun(E,A)->[E|A] end, [] ).

