%% ajw - could not get this one going
-module (multi_file_replace).
-export ([replace/4,test/0]).

replace(Dir,FilePattern,Recursive,ReplacementList) ->
	filelib:fold_files(Dir,FilePattern,Recursive,fun (Filename,_) -> repl(Filename,ReplacementList) end,ok).

repl(Filename,ReplacementList) ->
	io:format("replacing ~p in ~p~n",[ReplacementList,Filename]),
	{ok,BinaryContents} = file:read_file(Filename),
	Contents = binary_to_list(BinaryContents),
	NewContents = list_sub:subst(Contents,ReplacementList),
	ok = file:write_file(Filename,NewContents).

test() ->
	replace("test","*.html",true,[{"localhost","location.hostname"},{"1456","location.port"}]).
