%% Advice Definition (ADF) File Parsing
%%
%%  This parser for Advice definition files will generate global pointcuts,
%%  and advice structures which can be applied application wide. The ADF file
%%  syntax is similar to the ADF file format from the ErlAOP project except
%%  with different options avaliable. 
%%
%%  @author Alexander Dean
-module(erladf).
-include("advice.hrl"). % For all record definitions.
-include("pointcut.hrl").

-export([is_advice_dir/1, 
         parse/1, parse_all/1]).

%% Dummy Record to carry advice and pointcut references in the global
%% context. We will unwrap before passing back to user of this library.
-record( aspect, { advice = [], pointcuts = [] }).

%%% Bindings to turn ADF files into our Pointcut/Advice record structures.
-define( DEFAULT_BINDINGS, [
    {'Advice', fun( Type, Module, Fun ) ->
                       #advice{ type=Type, module=Module ,name=Fun, args=[] }
               end},
    {'Aspect', fun( Adv, Pc ) -> #aspect{ advice=Adv, pointcuts=Pc } end},
    {'Pointcut', fun( M, F, A, S, B ) -> 
                       #pointcut{ module=M, func=F, arity=A, 
                                  scope=S, behaviour=B }
                 end}        
]).

%% @doc Checks if a particular directory has global advice files. Called when
%%   compiling a new application, checks if root project has advice files.
%% @end
-spec is_advice_dir( file:name_all() ) -> boolean().
is_advice_dir( DirPath ) ->
    case file:list_dir( DirPath ) of
        {ok, FileNames} -> (case get_adf_files( FileNames ) of
                                [] -> false;
                                _ -> true
                            end);
        {error, _} -> false
    end.

%% @doc Wraps call to file parser with the AspectErl ADF file bindings. We
%%   will return a list of global pointcuts/advice for injection.
%% @end
-spec parse( file:name_all() ) -> {ok, term()} | {error, Reason :: any()}.
parse( FilePath ) ->
    case file:script( FilePath, ?DEFAULT_BINDINGS ) of
        {ok, Aspects}   -> {ok, Aspects};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Wraps directory call and will get all adf files in the current 
%%  directory and returns the bindings. It will ignore advice in sub 
%%  directories
%% @end
-spec parse_all( file:name_all() ) -> {ok, term()} | {error, Reason :: any()}.
parse_all( DirPath ) ->
    case file:list_dir( DirPath ) of
        {error, Reason} -> {error, Reason};
        {ok, FileNames} -> 
            AdfFiles = get_adf_files( FileNames ),
            Gs = lists:foldl( fun( AdfFile , Globals ) -> 
                                AdfFilePath = DirPath ++ "/" ++ AdfFile,
                                case parse( AdfFilePath ) of
                                    {ok, G} -> G++Globals;
                                    _ -> Globals
                                end
                              end, [], AdfFiles ),
            {ok, unwrap(Gs)}
    end.

%%% =========================================================================
%%% Private functionality
%%% =========================================================================

%% @hidden
%% @doc Recursively gets all adf files from a list of files.
get_adf_files( Files ) -> get_adf_files( Files, [] ).
get_adf_files( [], A ) -> A;
get_adf_files( [H|R] , A ) ->
    case filename:extension(H) of
        ".adf" -> get_adf_files( R, [H|A] );
        _Other -> get_adf_files( R, A )
    end.

%% @hidden
%% @doc Unwraps our Aspect record structure around the global pointcuts/advice.
%%  The internal Advice record is more powerful and can link to the pointcut
%%  table.
%% @end
unwrap( Globals ) -> unwrap( Globals, {[], []} ).
unwrap( [], A ) -> A;
unwrap( [#aspect{ advice=A, pointcuts=P }|R], {Adv, Pcs} ) ->
    {ALinked, Ps} = rename_pcts( P, A, [] ),
    unwrap( R, {[ALinked|Adv], Ps++Pcs} ).

%% @hidden
%% @doc For each pointcut, name it and then link the advice to it.
rename_pcts( [], A, Ps ) -> { A, Ps };
rename_pcts( [P|R], A, Ps ) -> 
    Name = gen_global_name(P), 
    PNamed = P#pointcut{name=Name},
    ALinked = A#advice{pointcuts=[Name|A#advice.pointcuts]},
    rename_pcts( R, ALinked, [PNamed|Ps] ).

%% @hidden
%% @doc Returns a global name (hash) of the pointcut for advice lookup.
gen_global_name( Pct ) ->
    {global, erlang:phash2( Pct ) }.

