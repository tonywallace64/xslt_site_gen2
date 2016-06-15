%% INPUT:
%% List of stuctures with next function following pattern of next in gb_trees
-module(merge_join).

-type iterator() :: list().
-type key() :: any().
-type value() :: any().
-type merge_item_def() :: {iterator(),fun ((iterator()) -> {key(),value(),iterator()})}.
-type merge_line() :: [{key(),value()}].
-type heap(A) :: [A].
-record (state, {merge_num::integer(),filter::fun((merge_line())->boolean()),next::heap({Position :: integer(),merge_item_def()}),current_line::list()}).

-export_type([merge_item_def/0]).
-export ([init/2,next/1,test/0,list_accessor_test/0]).

itr_get({Itr,Next}) ->
	Z = Next(Itr),
  case Z of
		none -> 
			none;
  	_ -> 
			{Key,Value,NewItr} = Z,
			{Key,Value,{NewItr,Next}}
	end.

-spec init(Sources::[merge_item_def()],Filter::fun((merge_line())->boolean())) -> {ok,#state{}}.
init(Sources,Filter) ->
	Heap = 
		lists:foldr(
			fun({Data,Accessor},{Oqn,Heap,UpdatedSources}) ->  
				Z=Oqn+1
				,{Val,NS}=Accessor(Data) 
				,NewHeap = heaps:add({Val,Z})
				,{Z,NewHeap,[NS|UpdatedSources]} 
			end
			,{0,heaps:new(),[]}
			,Sources)
	,Min = heaps:min(Heap)
	,H2 = heaps:delete_min(Heap)
  ,{state,length(Sources),Filter,H2,Min}.
		

-spec next(#state{}) -> {merge_line(),#state{}}.

next(State) ->
	%% retrieve top of heap
	%% retrieve from heap while top = retrieved value
  not_implemented.

-spec merge(#state{}) -> [{any(),[]}].

merge(S) ->
	case Z=next(S) of
		{R,S2} = Z ->  [R|merge(S2)];
		none -> []
  end.

%% test routines

%% list accessor functions 
list_next([]) -> none;
list_next([{Key,Value}|Rest]) -> {Key,Value,Rest}.

list_accessor_test() ->
	KV1 = [{2,set1two},{4,set1four},{6,set1six}],
	Z = fun list_next/1,
  {2,set1two,KV11} = itr_get({KV1,Z}),
	{4,set1four,KV12} = itr_get(KV11),
	{6,set1six,KV13} = itr_get(KV12),
  none = itr_get(KV13),
  passed.	

test() ->
	KV1 = [{2,set1two},{4,set1four},{6,set1six},{8,set1eight},{12,set1twelve}],
  KV2 = [{3,set2three},{6,set2six},{9,set2nine},{12,set2twelve}],
  Nextfun = fun([{K,V}|T]) -> {K,V,T} end,
	FullJoin = fun(X) -> X end,
  {ok,S} = init([{KV1,Nextfun},{KV2,Nextfun}],FullJoin),
  RawResult = 
		[{2,[set1two,nil]},{3,[nil,set2three]},{4,[set1four,nil]},
		{6,[set1six,set2six]},{8,[set1eight]},{12,[set1twelve,set2twelve]}],
  RawResult = merge(S),
	passed.
