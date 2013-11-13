%% Aspecterl_injector.Erl - The Aspect Erl Advice Injector.
%%
%% This module walks through code and injects advice if there are matching 
%% pointcuts. This must be called after either erladf or aspecterl_extractor
%%
%% @author Alexander Dean
-module(aspecterl_injector).

-export([parse_transform/2]).

parse_transform( AST, _Options ) ->
    Nope = [ Args || {attribute, _, aspecterl, Args} <- AST],
    case lists:member(exclude, lists:flatten(Nope)) of
        true  -> io:fwrite("Ignoring file in injector");
        false -> io:fwrite("AST = ~p\n",[AST])
    end,
    AST.


