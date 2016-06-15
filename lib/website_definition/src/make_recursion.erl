-module (make_recursion).
-export([main/1,test/0]).

-define(InputSeparator,32).
-define(WildCardChar,$%).

main([SrcListFile,TargetPattern]) ->
    SrcList = source_list(SrcListFile),
    TargetList = target_list(SrcList,TargetPattern),
    ReturnStatus = do_make(TargetList),
    halt (ReturnStatus).

do_make(TargetList) ->
    TL2 = [X++" " ||X <- TargetList],
    TL3 = lists:flatten(TL2),
    io:format("Making these targets ~n~p~n",[TL3]),
    Result = os:cmd(["make ",TL3,";echo $?"]),
    ResultList = re:split(Result,io_lib:nl()),
    io:format("Make returns as list: ~n~p~n",[ResultList]),
    output_results(ResultList).

output_results([StatusBin,<<>>]) ->
    I = binary_to_integer(StatusBin),
    I;
output_results([Msg|T]) ->
    io:format(standard_io,"~p~n",[Msg]),
    output_results(T).

source_list(SrcListFile) ->
    {ok,FileContentsBin} = file:read_file(SrcListFile),
    FileContentsStr = binary_to_list(FileContentsBin),
    string:tokens(FileContentsStr,[?InputSeparator]).
    
target_list(SrcList,TargetPattern) ->
    T=[_Before,_After] = string:tokens(TargetPattern,[?WildCardChar]),
    [target_name(SrcFile,T) || SrcFile <- SrcList].

target_name([10],_) ->
    "";
target_name(SrcFile,[Before,After]) ->
    Before ++ filename:basename(filename:rootname(SrcFile))++After.

test() ->
    file:write_file("make_recursion_test",<<"file1.xml",10,"file2.xml">>),
    ["file1.xml","file2.xml"] = source_list("make_recursion_test"),
    ["html/file1.xhtml","html/file2.xhtml"] = 
	target_list(["file1.xml","file2.xml"],"html/%.xhtml"),
    ok=file:delete("make_recursion_test"),
    passed.
    


    
