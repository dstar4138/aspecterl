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
-include("decorate.hrl").

% The Compile-time hook function. See include/aspect.hrl for more information.
-export([parse_transform/2]).

% Transformation State, this is built from the compile options as well as 
% environment settings for the aspect application.
-record( aspect_pt, { 
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
    State = check_options( Options ), 
    {NewState, NewAST} = pass_one( AST, State ),
    pass_two( NewState, NewAST ).


%%% =========================================================================
%%% Parse Passes
%%% =========================================================================

%% @private
%% @doc Take in the initial state after parsing options, and run through the
%%   AST to retrieve all PointCuts and Advice. This will delete the attributes
%%   it recognizes as it goes.
%% @end
pass_one( AST, #aspect_pt{ verbose=Verbose }=State ) -> 
    io:fwrite("AST = ~p\n",[AST]),
    {State,AST}.


pass_two( AST, _State ) -> AST.

%%% =========================================================================
%%% Internal Functionality
%%% =========================================================================

%% @hidden
%% @doc Default to rebar verbosity/debug level, otherwise look at applied
%%   compiler flags. This will build the default state for the parse
%%   transformer.
%% @end
check_options( Options ) ->
    #aspect_pt{ verbose = check_verbosity( Options ) }.
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
         


