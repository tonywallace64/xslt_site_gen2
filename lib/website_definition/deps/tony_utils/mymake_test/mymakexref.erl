-module(mymakexref).
%% recompiling: transitive dependencies...
%% A transitive dependency exists where
%% a module relies upon an exported
%% resource (function or data structure),
%% and that resource has changed.
%%
%% It may be that the dependent module
%% is now invalid.
%%
%% Recompiling is unneeded when
%% none of the imports have changed.
%% 
-export([start/0]).

start() ->
    xref:start(?MODULE,modules).

add() ->
    xref:add_module(?MODULE,"ebin").
    
