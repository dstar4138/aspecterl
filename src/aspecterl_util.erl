%% Aspecterl_util.erl - The AspectErl Utility module.
%%
%% This module has a couple utility functions to reduce the repeated 
%% functionality in both the extractor and injector parse transformers.
%%
%% @author Alexander Dean
-module(aspecterl_util).
-include("advice.hrl").
-include("defs.hrl").

-export( [ inform/3, check_options/2, check_verbosity/1 ] ).

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
    Attr = [Args || {attribute, _, ?AspectErlAttr, Args} <- AST],
    HasAdvice  = lists:member(advice_file, lists:flatten( Attr ) ),
    Behaviours = [B || {attribute, _, behaviour, B} <- AST],
    Exported   = [E || {attribute, _, export, EL} <- AST, E <- EL],
    Excluded   = lists:member(exclude, lists:flatten(Attr)),
    State = #aspect_pt{ 
        file = parse_trans:get_file( AST ),
        module = parse_trans:get_module( AST ),
        behaviours = Behaviours,
        inject_missing=lists:member(inject_missing, lists:flatten(Attr)),
        exported = Exported,
        verbose = check_verbosity( Options ),
        pct = [],
        adv = [] 
    },
    {HasAdvice, Excluded, State}.

%% @hidden 
%% @doc Check compile flags for verbosity. See check_options/1. Defaults to 
%%      rebar's value.
%% @end
check_verbosity( Options ) ->
    case application:get_env( rebar_global, verbose ) of
        undefined -> lists:member( verbose, Options );
        {ok, _ }  -> rebar
    end.

