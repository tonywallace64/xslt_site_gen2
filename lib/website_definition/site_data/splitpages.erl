-module(splitpages).
%% V1.1
%% Changes
%%   File headers no longer inserted into output by this
%%   routine.  Done in XSL instead.  Because of this parameter
%%   PageData is not longer required and has been removed.
%%
-export([main/1]).


%% debugging is provided if the debug_flag is compiled in.
%% from command line use: erlc -Ddebug_flag xslt_extn
%% in the erlang shell use: c(xslt_extn, {d,debug_flag).
-ifdef(debug_flag).
-define (DEBUG(X), io:format(standard_error,"DEBUG ~p:~p ~s=~p~n",[?MODULE,?LINE,"X",X])).
-define (DEBUGSTR(X), io:format(standard_error, "DEBUG ~p:~p ~s~n",[?MODULE,?LINE,X])).
-else.
-define (DEBUG(X), void).
-define (DEBUGSTR(X), void).
-endif.


main([]) ->
    ?DEBUGSTR("MAIN~n"),
    main(["./"]);
main([SavePath|_]) ->
    Line = io:get_line(""),
    loop(SavePath,nofile,Line).

loop(_,SaveFile,eof) ->
    ?DEBUGSTR("CLOSING~n"),
    file:close(SaveFile);
loop(SavePath,SaveFile,Line) ->
    NewSaveFile = processLine(SavePath,SaveFile,Line),
    loop(SavePath,NewSaveFile,io:get_line("")).

processLine(Path,nofile,[$f,$i,$l,$e,$n,$a,$m,$e,$:|RestOfLine]) ->
    ?DEBUGSTR("OPEN FIRST FILE"),
    {Name,Rest} = getfilename(RestOfLine),
    ?DEBUG(Name),
    Filename = filename:join(Path,Name),
    {ok,NF}=file:open(Filename,[write]),
    processLine(Path,NF,Rest);
processLine(Path,SaveFile,[$f,$i,$l,$e,$n,$a,$m,$e,$:|RestOfLine]) ->
    ?DEBUGSTR("OPEN FILE"),
    file:close(SaveFile),
    {Name,Rest} = getfilename(RestOfLine),
    ?DEBUG(Name),
    {ok,NF}=file:open(filename:join(Path,Name),[write]),
    processLine(Path,NF,Rest);
processLine(_,nofile,_) -> nofile;
processLine(_,SaveFile,Data) ->
    ?DEBUG(Data),
    file:write(SaveFile,Data),
    SaveFile.

getfilename(R) ->
    IsFileChar = fun(X) -> not ((X=:=10) or (X=:=$<)) end,
    Z={_Name,_Rest2} = lists:splitwith(IsFileChar,R),
    %io:format("file is ~s~n",[Name]),
    Z.
    
