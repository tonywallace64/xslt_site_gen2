-module (gb_trees_tools_new).
-export ([tree_next/2,tree_prev/2,tree_move/3,interval_iterator/3,test/0,ii_test/0,ii_random_test/2]).

%-define(de_bug(String),io:format("~s~n",[String])).
-define(de_bug(String),"").


%% PURPOSE
%%
%% To extend gb_trees:
%% tree_next, 
%% tree_prev
%% 

%% This version - estimate tree selectivity on interval_iterator
%%
tree_next(Key,Gb_Tree) ->
  tree_move(Key,Gb_Tree,next).

tree_prev(Key,Gb_Tree) ->
  tree_move(Key,Gb_Tree,prev).

tree_move(_Key,{0,nil},_) -> nil;
tree_move(Key,{Size,Tree},Direction) when is_integer(Size), Size >= 0 ->
  %% find next entry in tree where key value greater than key
  ?de_bug("new evaluation"),  
  tree_move1(Key,Tree,Direction).

tree_move1(Key,{Key,_,_,Gr},next) -> 
  ?de_bug("rule 1"),
  %% a key does not match itself...
  tree_move1(Key,Gr,next);

tree_move1(_,nil,_) -> 
  ?de_bug("rule 2"),
  nil;

tree_move1(Key1,{Key,Value,nil,_},next) when (Key1<Key)  -> 
  ?de_bug("rule 3"),
  %% if the key in the tree is greater than the key searched for
  %% and we are searching for a greater key
  %% and there is no smaller value then the key matches
  {Key,Value};

tree_move1(Key1,{Key,Value,Smaller1,_},next) when (Key1 < Key)  ->  
  ?de_bug("rule 4"),
  %% if the key in the tree is greater than the key searched for
  %% and so the the current value is a candidate for return,
  %% but there is smaller subtree that might have the correct
  %% next key.  If it has a qualifying key it will be less than
  %% the current value so that must be the correct value.
  case R=tree_move1(Key1,Smaller1,next) of
    nil -> {Key,Value};
    _ -> R
  end;

tree_move1(Key1,{Key,_,_,Larger1},next) when (Key1>Key) ->  
  ?de_bug("rule 5"),
  %% if the key in the tree is less than the key searched for
  %% then the key we seek may be in the larger subtree
  tree_move1(Key1,Larger1,next);

tree_move1(Key,{Key,_,Lt,_},prev) -> 
  ?de_bug("rule 6"),
  %% a key does not match itself...
  tree_move1(Key,Lt,prev);

tree_move1(Key1,{Key,Value,nil,_},prev) when (Key1>Key)  -> 
  ?de_bug("rule 7"),
  %% if the key in the tree is smaller that the key searched for
  %% and we are searching for a smaller key
  %% and there is no smaller key
  %% then the key matches
  {Key,Value};

tree_move1(Key1,{Key,_,Smaller1,_},prev) when (Key1<Key) ->  
  ?de_bug("rule 8"),
  %% if the key in the tree is greater than the key searched for
  %% then the key we seek may be in the smaller subtree
  tree_move1(Key1,Smaller1,prev);

tree_move1(Key1,{Key,Value,_,Larger1},prev) when (Key1 > Key)  ->  
  ?de_bug("rule 9"),
  %% if the key in the tree is less than the key searched for
  %% and so the the current value is a candidate for return,
  %% but there is larger subtree that might have the correct
  %% next key.  If it has a qualifying key it will be greater than
  %% the current value so that must be the correct value.
  case R=tree_move1(Key1,Larger1,prev) of
    nil -> {Key,Value};
    _ -> R
  end.

-type boundary() :: {any(),boolean()}.

-spec interval_iterator(
		MinBoundary::boundary(),MaxBoundary::boundary(),
		Tree::gb_tree()) -> {float(),gb_trees:iter()}.
interval_iterator(_,_,{0,nil}) -> {0,[]};
interval_iterator(MinB={MinValue,_},MaxB={MaxValue,_},{_,Iterator}) when MinValue =< MaxValue ->
  {Selectivity,PrunedTree} = ii1(1.0,MinB,MaxB,Iterator),
  Itr =  gb_trees:iterator({1,PrunedTree}),
	{Selectivity,Itr}.

ii1(S,_,_,nil) -> {S,nil};
ii1(S,MinB,MaxB,{Key,Value,Smaller,Bigger}) ->
  case value_test(MinB,MaxB,Key) of
		too_small -> ii1(S*0.5,MinB,MaxB,Bigger);
		too_large -> ii1(S*0.5,MinB,MaxB,Smaller);
		in_range ->
			{SSmaller,PrunedSmaller} = check_for_too_small(S*0.5,MinB,Smaller),
			{SLarger,PrunedLarger}   = check_for_too_large(S*0.5,MaxB,Bigger),
			{SSmaller+SLarger,{Key,Value,PrunedSmaller,PrunedLarger}}
	end.

check_for_too_small(S,_,nil) -> {S,nil};
check_for_too_small(S,MinB,{Key,Value,Smaller,Bigger}) ->
  case value_test_min(MinB,Key) of
		too_small -> check_for_too_small(S*0.5,MinB,Bigger);
		big_enough -> 
			{SSmaller,PrunedSmaller} = check_for_too_small(S/2,MinB,Smaller),
			{SSmaller+S/2,{Key,Value,PrunedSmaller,Bigger}}
  end.

check_for_too_large(S,_,nil) -> {S,nil};
check_for_too_large(S,MaxB,{Key,Value,Smaller,Bigger}) ->
	S2=S/2,
  case value_test_max(MaxB,Key) of
		too_large -> check_for_too_large(S2,MaxB,Smaller);
		small_enough -> 
			{SLarger,PrunedLarger}   = check_for_too_large(S2,MaxB,Bigger),
			{S2+SLarger,{Key,Value,Smaller,PrunedLarger}}
  end.

-spec value_test(MinBoundary::boundary(),MaxBoundary::boundary(),Value::any()) -> too_small|too_large|in_range.

value_test(MinB,MaxB,Value) ->
	IsTooSmall = value_test_min(MinB,Value),
  IsTooLarge = value_test_max(MaxB,Value),
  case {IsTooSmall,IsTooLarge} of
    {too_small,_} -> too_small;
		{_,too_large} -> too_large;
		{big_enough,small_enough} -> in_range
  end.

value_test_min({MinValue,_},Value) when Value < MinValue -> too_small;
value_test_min({MinValue,_},Value) when Value > MinValue -> big_enough;
value_test_min({MinValue,false},Value)   when Value =:= MinValue -> too_small;
value_test_min({MinValue,true },Value)   when Value =:= MinValue -> big_enough.

value_test_max({MaxValue,_},Value) when Value > MaxValue -> too_large;
value_test_max({MaxValue,_},Value) when Value < MaxValue -> small_enough;
value_test_max({MaxValue,false},Value) when Value =:= MaxValue -> too_large;
value_test_max({MaxValue,true },Value) when Value =:= MaxValue -> small_enough.

  

test() ->
  NoResult = nil,
  Empty=gb_trees:empty(),
  One_Element = gb_trees:enter(3,three,Empty),
  Left_Tree = gb_trees:enter(1,one,One_Element),

  NoResult = tree_next(3,gb_trees:empty()) ,
  NoResult = tree_next(5,One_Element),
  {3,three} = tree_next(1,One_Element),
  {3,three} = tree_next(1,Left_Tree),
  {1,one} = tree_next(0,Left_Tree),

  NoResult = tree_prev(3,Empty),
  NoResult = tree_prev(0,Left_Tree),
  {1,one} = tree_prev(3,Left_Tree),
  {1,one} = tree_prev(2.5,Left_Tree),
  {3,three} = tree_prev(4,Left_Tree),
  passed.

ii_comparator_test() ->
  %% test comparators
  too_small = value_test_min({1,false},0),
  too_small = value_test_min({1,false},1),
  big_enough = value_test_min({1,true},1),
	big_enough = value_test_min({1,true},2),
  {'EXIT',{_,_}} = (catch gb_trees_tools:value_test_min({1,not_a_boolean},1)),

  too_large = value_test_max({1,false},2),
  too_large = value_test_max({1,false},1),
	small_enough = value_test_max({1,true},1),
	small_enough = value_test_max({1,true},0),
  {'EXIT',{_,_}} = 
		(catch gb_trees_tools:value_test_max({1,not_a_boolean},1)),
  ok.

ii_test() ->
  ok = ii_comparator_test(),
  E = gb_trees:empty(),
  {0,[]} = interval_iterator({0,true},{10,true},E),
  Five = gb_trees:insert(5,five,E),
	%% the following iterator should fail as MinBoundary > MaxBoundary
	{'EXIT',{function_clause,_}} = 
		(catch interval_iterator({10,true},{0,true},Five)),
  {1.0,FiveItr} = 
		interval_iterator({0,true},{10,false},Five),
  {5,five,[]} = gb_trees:next(FiveItr),
  OddTree = 
		lists:foldl(
			fun ({Key,Value},Acc) -> 
				gb_trees:insert(Key,Value,Acc) end,
			Five,
			[{1,one},{3,three},{9,nine},{7,seven}]),
  {_,ThreeToSevenItr} = 
		interval_iterator({2,true},{8,false},OddTree),
  {3,three,FiveToSevenItr} = 
		gb_trees:next(ThreeToSevenItr),
	{5,five,SevenItr} = gb_trees:next(FiveToSevenItr),
	{7,seven,[]} = gb_trees:next(SevenItr),
  none = gb_trees:next([]),
  passed.

ii_random_test(MaxVal,NumTests) when NumTests > 0 ->
  ok = ii_comparator_test(),
  ii_random_test2(MaxVal,NumTests).

ii_random_test2(_,0) -> ok;
ii_random_test2(MaxVal,NumTests) ->
  MinIncl = random_bool(),
  MaxIncl = random_bool(),
  I1 = random:uniform(MaxVal),
  I2 = random:uniform(MaxVal),
  SetSize = random:uniform(MaxVal),
  io:format("I1=~p~nI2=~p~n",[I1,I2]),
	ii_random_test3
		({I1,MinIncl},{I2,MaxIncl},MaxVal,SetSize),
	ii_random_test2(MaxVal,(NumTests-1)).

ii_random_test3(P1={I1,_},P2={I2,_},MaxData,SetSize) when (I1 > I2) ->
	ii_random_test3(P2,P1,MaxData,SetSize);

ii_random_test3(MinB,MaxB,MaxData,SetSize) ->
  io:format("ii_random_test3: main code~n",[]),
	io:format(
		"MinB=~p~nMaxB=~p~nMaxData=~p~nSetSize=~p~n",
		[MinB,MaxB,MaxData,SetSize]),
  {MinVal,_}=MinB,
  {MaxVal,_}=MaxB,
  true = (MinVal =< MaxVal),

  DataSet = make_set(MaxData,SetSize),
	DataTree = 
		lists:foldl(
			fun (X,Acc) -> gb_trees:enter(X,X,Acc) end,
			gb_trees:empty(), 
			DataSet), 
  ExpectedResultsTree = 
		lists:foldl(
			fun (X,Acc) -> 
				case value_test(MinB,MaxB,X) of 
					in_range -> gb_trees:enter(X,X,Acc);
					_ -> Acc
				end
			end,
			gb_trees:empty(),
			DataSet),
	ExpectedSelectivity = (gb_trees:size(ExpectedResultsTree)/gb_trees:size(DataTree)),
	{Selectivity,TestIterator} =  
		interval_iterator(MinB,MaxB,DataTree),
	RefIterator = gb_trees:iterator(ExpectedResultsTree),
	io:format("ExpectedSelectivity=~p~n",[ExpectedSelectivity]),
  io:format("Selectivity=~p~n",[Selectivity]),
  %io:format("~nDataSet=~p~nDataTree=~p~nTestIterator=~p~nRefIterator=~p~n",
	%	[DataSet,DataTree,TestIterator,RefIterator]),
  itrCompare(TestIterator,RefIterator).

make_set(M,S) when S>0 -> make_set(M,S,[]).
make_set(_,0,A) -> A;
make_set(M,S,A) ->
	A2=[random:uniform(M)|A],
	make_set(M,S-1,A2).

itrCompare([],[]) -> ok;
itrCompare(A,B) ->
	{K1,V1,I1} = gb_trees:next(A),
	{K1,V1,I2} = gb_trees:next(B),
  %io:format("~nItr1=~p~nItr2=~p~n",[I1,I2]),
  itrCompare(I1,I2).


random_bool() -> random_bool(random:uniform(2)).
random_bool(1) -> false;
random_bool(2) -> true.

	
