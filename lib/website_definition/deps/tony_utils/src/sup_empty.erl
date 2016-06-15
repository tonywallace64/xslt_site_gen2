%% @doc
%% sup_empty
%%
%% This module generates an empty supervisor
%% for use where children are started dynamically
%% @end
-module (sup_empty).

-export([init/1,start_link/3,start_link/4]).

-spec start_link(one_for_one|one_for_all|rest_for_one,integer(),integer()) -> {ok,pid()}.
start_link(Strategy,MaxR,MaxTSecs) ->
  supervisor:start_link(?MODULE,[{ok,{{Strategy,MaxR,MaxTSecs},[]}}]).

-spec start_link({local, Name :: atom()} | {global, Name :: atom()},
   one_for_one|one_for_all|rest_for_one,integer(),integer()) -> {ok,pid()}.
start_link(RegName,Strategy,MaxR,MaxTSecs) ->
  supervisor:start_link(RegName,?MODULE,[{ok,{{Strategy,MaxR,MaxTSecs},[]}}]).

init([Z]) -> Z.

