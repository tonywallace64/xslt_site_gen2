-module(make_resources).
-export([main/1,wipe_directory_test/0]).

main([XmlFile]) ->
    {Data,_} = xmerl_scan:file(XmlFile),
    make_res(Data).

make_res(XmlData) ->
    %io:format("processing resources~n"),
    Dirs = xmerl_xpath:string("/site/resources/dir",XmlData),
    %io:format("Data ~w~n",[XmlData]),
    Files = xmerl_xpath:string("/site/resources/file",XmlData),
    ResDir = xmerl_xpath:string("/site/resources/copydir",XmlData),
    %% copy files to temporary location
    wipe_directory("build"),
    file:make_dir("build"),
    DirList = [filename:join("build",nodestring(X))  ||X <- Dirs],
    %io:format("make_res DirList~n"),
    print_list_of_strings(DirList),
    [file:make_dir(X) || X <- DirList],
    FileList = [ nodestring(X) || X <- Files],
    %io:format("make_res FileList~n"),
    print_list_of_strings(FileList),
    [file:copy(X, filename:join("build",X)) ||
	X <- FileList],
    ResDirList = [nodestring(X) || X<-ResDir],
    [recursive_copy(X,filename:join("build",X)) || X<-ResDirList],
    ok.	  
    %[io:format("~s~n",[X]) || X <- DirList]

    %% delete old contents of resource directory

    %% copy in files from temporary location
 
wipe_directory(".") -> ok;
wipe_directory("..") -> ok;
wipe_directory(Name) ->
    case filelib:is_dir(Name) of
	true ->
	    wipe_dir1(Name);
	false ->
	    ok
    end.

wipe_dir1(Name) ->
    {ok,Filenames} = file:list_dir(Name),
    {Directories,Files} = 
	lists:splitwith(fun(X) -> filelib:is_dir(X) end,Filenames),
    %io:format("wipe_dir1 subdirectories~n"),
    %print_list_of_strings(Directories),
    %io:format("wipe_dir1 files~n"),
    %print_list_of_strings(Files),
    %io:format("recursive delete of directories~n"),
    [wipe_directory(filename:join(Name,X)) || X <- Directories],
    %io:format("delete local files"),
    [file:delete(filename:join(Name,X)) || X <- Files],
    file:del_dir(Name).

wipe_directory_test() ->
    %io:format("Test1: wipe_directory removes an empty directory ... testing"),
    file:make_dir("wdtest"),
    true = filelib:is_dir("wdtest"),
    wipe_directory("wdtest"),
    false = filelib:is_dir("wdtest"),
    %io:format("... passed~n"),
    %io:format("Test2: wipe directory containing regular files ... testing"),
    file:make_dir("wdtest"),
    os:cmd("touch wdtest/file1"),
    os:cmd("touch wdtest/file2"),
    wipe_directory("wdtest"),
    false = filelib:is_dir("wdtest")
    %io:format("... passed~n")
.


nodestring(Node) ->
    {xmlObj,string,Value} = xmerl_xpath:string("string(.)",Node),
    Value.

recursive_copy(Src,Dst) ->
    %% copy directory src into directory Dst
    %% test assertions
    %io:format("recursive_copy from ~s to ~s~n",[Src,Dst]),
    true = filelib:is_dir(Src),

    % make new target directory dst
    %io:format("make target directory~n"),
    ok = file:make_dir(Dst),
    %% copy all files inside src directory into dst
    %io:format("copy contents~n"),
    {ok,SrcTargets} = file:list_dir(Src),
    [copy_target(X,Src,Dst,filelib:is_dir(filename:join(Src,X))) 
     || X <- SrcTargets].

copy_target(Dir,Src,Dst,true) ->
    recursive_copy(filename:join(Src,Dir),filename:join(Dst,Dir));
copy_target(File,Src,Dst,false) ->
    %io:format("copy file ~s in ~s to ~s~n",[File,Src,Dst]),
    {ok,_} = file:copy(filename:join(Src,File),filename:join(Dst,File)).

print_list_of_strings(_LOS) ->
    %[io:format("~s,",[X])  || X <- LOS],
    %io:format("~n").
    ok.
    
