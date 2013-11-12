%% Aspect.Erl -
%%
%%  This is the module which gets inserted into the pre-processor to strip out
%%  annotations for advice and pointcuts so that weeving can take place.
%%
%%  @author Alexander Dean
-module(aspect).
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
           verbose = false, % Should we print warnings, such as unused advice?
           debug = false % Should the compiler explain where advice is applied?
           %% .. TODO: what other compiler flags can we work with?
}).

%% @doc Loops through the Abstract Syntax Tree and extracts the portions of
%% interest then injects advice code where it's needed.
%% @end
parse_transform( AST, _Options ) -> 
    io:fwrite("AST = ~p\n",[AST]),
    AST.






%%% =========================================================================
%%% Internal Functionality
%%% =========================================================================

%% @hidden
%% @doc Default to rebar verbosity/debug level, otherwise look at applied
%%   compiler flags. This will build the default state for the parse
%%   transformer.
%% @end
check_options( Options ) ->
    #aspect_pt{ verbose = check_verbosity( Options ),
                debug   = check_debug( Options )
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
%% @doc Check compile flags for debugging. See check_options/1.
check_debug( Options ) -> get_opt( aspect_debug, Options ).

%% @hidden
%% @doc Check if a option is apart of the option list.
get_opt( Opt, Options ) -> lists:member( Opt, Options ).
         


