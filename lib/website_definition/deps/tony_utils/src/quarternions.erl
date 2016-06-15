-module (quarternions).

-export([identities/1,multiply/1,test/0,sum/1]).

% Real numbers denoted by r
% so all results have a dimension 
identities([i,j,k]) -> [-1,r];
identities([A,r])   -> [ 1,A];
identities([r,A])   -> [ 1,A];
identities([A,A])   -> [-1,r];
identities([i,j])   -> [ 1,k];
identities([j,k])   -> [ 1,i];
identities([k,i])   -> [ 1,j];
identities([A,B]) -> 
  [1,R] = identities([B,A]),
  [-1,R].

term([{V1,T1},{V2,T2}]) ->
	[Z,T] = identities([T1,T2]),
  {multiply([Z,V1,V2]),T}.

multiply(L) when is_list(L) ->
	lists:foldl(fun(X,A) -> do_multiply(A,X) end,nil,L).

sum(L) when is_list(L) ->
	lists:foldl(fun(X,A) -> do_sum(A,X) end,nil,L).


do_multiply({quarternion,R1,I1,J1,K1},{quarternion,R2,I2,J2,K2}) ->
  Raw_Product = 
    cartesian:product([
       [{R1,r},{I1,i},{J1,j},{K1,k}],
       [{R2,r},{I2,i},{J2,j},{K2,k}]]),
  Simplified = lists:map(fun(X) -> term(X) end,Raw_Product),
  %% Factorise with a list comprehension.  Each of [r,i,j,k] becomes
  %% a filter on Simplified
  R =[ {X,sum(lists:map(fun(X2)-> {Term,_}=X2, Term end,lists:filter(fun({_,T}) -> X==T end,Simplified)))} || X<-[r,i,j,k]],
  [{r,R3},{i,I3},{j,J3},{k,K3}] = R,
  {quarternion,R3,I3,J3,K3};
do_multiply(nil,A) -> A;
do_multiply(A,B) when is_number(A) and is_number(B) -> A*B;
do_multiply({product,Sign,L},-1) ->
    {product,Sign*-1,L};
do_multiply({product,Sign,L},B) -> 
  {product,Sign,lists:append([L,[B]])};
do_multiply(1,X) -> X;
do_multiply(X,{sum,L}) -> {sum,associative_rule_exp(X,L)};
do_multiply({sum,L},X) -> {sum,associative_rule_exp(X,L)};
do_multiply(-1,B) -> {product,-1,[B]};
do_multiply(B,-1) -> {product,-1,[B]};
do_multiply(A,B) -> {product,1,[A,B]}.

associative_rule_exp(Common,List) ->
    associative_rule_exp(Common,List,[]).
associative_rule_exp(_,[],Acc) -> Acc;
associative_rule_exp(Common,[H|T],Acc) ->
    associative_rule_exp(Common,T,[do_multiply(H,Common)|Acc]).

do_sum({quarternion,R1,I1,J1,K1},{quarternion,R2,I2,J2,K2}) ->
  {quarternion,sum([R1,R2]),sum([I1,I2]),sum([J1,J2]),sum([K1,K2])};
do_sum(nil,A) -> A;
do_sum(A,B) when is_number(A) and is_number(B) -> A+B;
do_sum({sum,L1},{sum,L2}) -> {sum,lists:append(L1,L2)};
do_sum({sum,L},B) -> {sum,lists:append([L,[B]])};
do_sum(A,B) -> {sum,[A,B]}.

test() -> 
  multiply([{quarternion,r1,i1,j1,k1},{quarternion,r2,i2,j2,k2}]).
  %do_multiply({quarternion,r1,i1,j1,k1},{quarternion,r2,i2,j2,k2}).
  %sum([one,two]).

