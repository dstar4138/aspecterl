%% AspectErl Callbacks
%%
%% This module maintains several of the callback functions. These are used at
%% runtime and injected during compile time or at the discression of the 
%% programmer (either through AspectErl Macros or explicit calls).
%%
-module(aspecterl_cb).

%% AspectErl Callbacks.
-export([proceed/1]).



%% ===========================================================================
%% AspectErl Callback Functions
%% ===========================================================================

%% Proceed within the internals of the captured function
%% TODO: Is there any masking of self() or the applying function that needs to take place?
proceed(_FuncCall = {Module, Function, Args}) -> erlang:apply(Module,Function,Args).

