-module (list_sub).

-export([subst/2,dos2unix/2,test/0]).
%-spec subst(string(),[{Target::string(),Replacement::string()}]) -> string().

dos2unix(Src,Dst) ->
    case filelib:is_file(Src) of
	true ->
	    dos2unix1(Src,Dst);
	false ->
	    ok
    end.

dos2unix1(SrcFile,DstFile) ->
    CRLF = [13,10],
    LF = [10],
    {ok,SrcBin} = file:read_file(SrcFile), 
    Src = erlang:binary_to_list(SrcBin),
    Res = subst(Src,{CRLF,LF}),
    Dst2 =
	case filelib:is_dir(DstFile) of
	    true ->
		filename:join(DstFile,filename:basename(SrcFile));
	    false -> DstFile
	end,
    ok = file:write_file(Dst2,Res).


subst(Source,_ReplList={Target,Replacement}) ->
	subst1(Target,Replacement,Target,Source,[]).

-spec subst1(Target::string(),Replacement::string(),MatchRemain::string(),In::string(),Out::string()) -> string().

subst1(T,Rep,[],In,Out) ->
    %% target has been matched but has already been copied to out,
    %% pop off target and push on replacement
    Popped = consume2(Out,length(T)),
    Pushed = p2(Popped,Rep),
    %% continue substitution
    subst1(T,Rep,T,In,Pushed);
subst1(Target,Rep,[H|T],[H|T2],Out) ->
    %% next character matches
    %% copy to output and shorten matchremaining
    subst1(Target,Rep,T,T2,[H|Out]);
subst1(T,Rep,[_NextMatchChar|_],_In=[NoMatch|InRest],Out) ->
    %% no match, 
    %% copy to output, restore match for full target string
    subst1(T,Rep,T,InRest,[NoMatch|Out]);
subst1(_,_,_,[],Out) ->
    lists:reverse(Out).

consume2(R,0) -> R;
consume2([_|T],N) -> consume2(T,N-1).

p2(R,[]) -> R;
p2(Rem,[H|T]) -> p2([H|Rem],T).

test() -> 
    CRLF = [13,10],
    LF = [10],
    TestStr = lists:append([CRLF,"line1",CRLF,CRLF,"line2",CRLF]),
    ResultStr = lists:append([LF,"line1",LF,LF,"line2",LF]),
    ResultStr = subst(TestStr,{CRLF,LF}).
