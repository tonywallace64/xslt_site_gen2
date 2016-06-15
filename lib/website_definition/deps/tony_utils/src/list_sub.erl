-module (list_sub).

-export([subst/2,test/0]).
-spec subst(string(),[{Target::string(),Replacement::string()}]) -> string().
subst(Source,ReplList) ->
	subst(Source,ReplList,[]).

subst([],_,Z) ->
	lists:reverse(Z);

subst(SourceRemaining,ReplList,Replaced) ->
	io:format("SR=~s~n",[SourceRemaining]),
	io:format("R =~s~n~n",[Replaced]),
	MatchList = lists:filter(fun(X) -> matches(SourceRemaining,X) end,ReplList),
	case MatchList =:= [] of
		true ->
			[NextChar|Remaining] = SourceRemaining,
			subst(Remaining,ReplList,[NextChar|Replaced]);
		false ->
			%% if multiple matches occur use the longest one
			%% accumulator holds longest match
			LongestMatch = 
				lists:foldl(
					fun (X={Tx,_},Acc={Ax,_}) -> 
						case length(Tx) > length(Ax) of 
							true -> X; 
							false -> Acc 
						end 
					end,
					{"",""},MatchList),
			Remaining = consume(SourceRemaining,LongestMatch),
			NewReplaced = prepend(Replaced,LongestMatch),
			subst(Remaining,ReplList,NewReplaced)
	end.

matches(L1,{Target,_Replacement}) ->
	m2(L1,Target).

m2(_,[]) -> true;
m2([],_) -> false;
m2([X|Y],[X|Z]) ->
	m2(Y,Z);
m2(_,_) -> false.

consume(Remaining,{_Target,_Replacement}) ->
	c2(Remaining,_Target).

c2(Z,[]) -> Z;
c2([H|T1],[H|T2]) -> c2(T1,T2).

prepend(Remaining,{_Target,_Repl}) ->
	p2(Remaining,_Repl).

p2(R,[]) -> R;
p2(Rem,[H|T]) -> p2([H|Rem],T).

test() ->
	Text = "This is a text for testing text replacement",
	" is a text for testing text replacement" = consume(Text,{"This","Replacement"}),
	"hello" = prepend("",{"Any","olleh"}),
	Target = subst(Text,[]),
	"This is 1 text for testing text repl1cement" = subst(Target,[{"a","1"}]),
	"This is 1 text for testing text repl2" = subst(Text,[{"a","1"},{"acement","2"}]).
