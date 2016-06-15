-module (la).
-export ([list_accessor_test/0]).

%% list accessor functions 
list_next([]) -> none;
list_next([{Key,Value}|Rest]) -> {Key,Value,Rest}.

itr_get({Itr,Next}) ->
	Z = Next(Itr),
  case Z of
		none -> 
			none;
  	_ -> 
			{Key,Value,NewItr} = Z,
			{Key,Value,{NewItr,Next}}
	end.

list_accessor_test() ->
	KV1 = [{2,set1two},{4,set1four},{6,set1six}],
	Z = fun list_next/1,
  {2,set1two,KV11} = itr_get({KV1,Z}),
	{4,set1four,KV12} = itr_get(KV11),
	{6,set1six,KV13} = itr_get(KV12),
  none = itr_get(KV13),
  passed.	

