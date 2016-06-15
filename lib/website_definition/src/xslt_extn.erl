-module(xslt_extn).
-author("Tony Wallace").
-email("tony@tony.gen.nz").
-licence("Public Domain").
-purpose(
<<"
An erlang module to provide:
xinclude text file inclusion
Splitting an xml file into several target files
(like opposite of file inclusion)
xml_text - this tag allows xml to be stored as escaped text
and expanded again when \"final-output\" is performed

Limitations:
The xinclude functionality provided here does not conform to the
standard in that many options provided for in the standard are ignored.
">>).
%
% Licence: This program is donated to the public domain.
% Freely I have received, freely I give.
%
% I acknowledge the work of the Ericsson Laboratories,
% the creators of erlang, that made this work possible.
%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Modification:
%%  Copy inscope namespaces to first element after a "file" element.
%%  This is a consequence of the way XSLT 1.0 handles 
%%  namespaces, and the requirement
%%  to split this output into several files.
%% Author: Tony Wallace
%% Date: 10 June 2015
%% ****************************************************


-export([main/1,doevent/2,file/1,xml_parameter_formatting/1]).
-export([test/0,test1/0,test2/0,test3/0,test4/0,test5/0,test6/0,
	 test7/0,test8/0,test9/0,test10/0]).
-include ("erlsom_sax.hrl").
-record(startElement, {url,tag,prefix,attrlist}).
-record(endElement,{url,tag,prefix}).


%% debugging is provided if the debug_flag is compiled in.
%% from command line use: erlc -Ddebug_flag xslt_extn
%% in the erlang shell use: c(xslt_extn, {d,debug_flag}).
%% in rebar.config add the entry: {erl_opts,[{d,debug_flag}]}.

-ifdef(debug_flag).
-define (DEBUG(X), 
	 io:format(standard_error,
		   "<DEBUG module=\"~p\"~n line=\"~p\"~n timestamp=\"~p\">~n<![CDATA[~p]]></DEBUG>~n",
		   [?MODULE,?LINE,notime,X])).
-define (DEBUGSTR(X), 
	 io:format(standard_error,
		   "<DEBUG module=\"~p\"~n line=\"~p\"~n timestamp=\"~p\">~n<![CDATA[~s]]></DEBUG>~n",
		   [?MODULE,?LINE,notime,X])).

-define (DEBUG2(Msg,X), 
	 io:format(standard_error,
		   "~s = ~p~n",
		   [Msg,X])).

-else.
-define (DEBUG(X), void).
-define (DEBUGSTR(X), void).
-define (DEBUG2(Msg,X), void).
-endif.
-define (PRESERVE_WHITESPACE_DEFAULT,true).
-define (FINAL_OUTPUT_DEFAULT,false).
-define (DATA_DIR,".").
-define (MUST_MATCH,"~s ~s must match ~s ~s").


%% first_tag in the first tag after a file split,
%% a new namespace declaration needs to be put
%% into that first tag.
default_state() ->
       #{default_output => standard_io,
   	output => standard_io, 
	fileinclusions => ordsets:new(),
	tests => [],
	escape_to_xml => false,
	first_tag => false,
	final_output => ?FINAL_OUTPUT_DEFAULT,
	preserve_whitespace => ?PRESERVE_WHITESPACE_DEFAULT,
	element_stack => [],
	names_in_scope => []}.    

main(InputParameters) -> 
    ?DEBUG(["input parameters"|InputParameters]),
    State0 = default_state(),
    State1 = lists:foldl(fun do_param/2,State0,InputParameters),
    %% discard output until first output file statement
    {ok,TestList} = maps:find(tests,State1),
    case TestList == [] of
	true ->
	    file:set_cwd(?DATA_DIR),
	    ?DEBUGSTR("Reading standard input until eof"),
	    Str=lists:flatten(read_stdin:read_until_eof()),
	    ?DEBUGSTR("Outputting results"),
	    parseStr(Str,State1);
	false ->
	    ?DEBUG(TestList),
	    [do_test(T) || T <- TestList]
    end,
    ?DEBUGSTR("Finished"),
    init:stop().

file([Filename]) ->
    file([Filename,[]]);
file([Filename,InputParameters]) ->
    %% Purpose: to parse input from a file instead of stdin for testing purposes in erlang shell
    Dirname = filename:dirname(Filename),
    SavedDirName = filename:absname("."),
    State0 = default_state(),
    State1 = lists:foldl(fun do_param/2,State0,InputParameters),
    {ok,Str} = file:read_file(Filename),
    ok=file:set_cwd(Dirname), 
    parseStr(Str,State1),
    ok=file:set_cwd(SavedDirName).

do_param("+finaloutput",Si) ->
    Si#{final_output => true};
do_param("-finaloutput",Si) ->
    Si#{final_output => false};
do_param("+preserve_whitespace",Si) ->
    Si#{preserve_whitespace => true};
do_param("-preserve_whitespace",Si) ->
    Si#{preserve_whitespace => false};
do_param(T=[$t,$e,$s,$t|_],Si) ->
    %% run tests...
    ?DEBUGSTR("Adding test to test list"),
    ?DEBUG(T),
    {ok,OT} = maps:find(tests,Si),
    Si#{tests => [T|OT]};
do_param(Z,_) ->
    throw("Unknown option "++Z).


parseStr(Str,State) when is_list(Str),is_map(State) ->
    State2=State#{skip_end_tag => false, prefix_next_tag=> [] },
    erlsom:parse_sax(Str,State2,fun doevent/2).

%% Do event must comply with 
%% the PLI for erlsom:parse_sax
doevent(Event,State) when is_map(State) ->
    ?DEBUG(Event),
    %% manage element stack
    Ctx = {S1,_ParentNS,_ParentTag,_Ancestors} =
	manage_element_stack(Event,State),
    true = is_map(S1),
    %% ?DEBUG2("S1",S1),
    R = do_evt_1(Event,Ctx),
    true = is_map(R),
    ?DEBUG(R),
    io:format(standard_error,"~n",[]),
    R.

manage_element_stack(Event,State) when is_map(State) ->
    {ok,ES} = maps:find(element_stack,State),
    {_S1,_ParentNS,_ParentTag,_Ancestors} = es_1(ES,Event,State).

es_1([],Event={startElement,_,_,_,_},State) ->
    %% in case of empty element stack getting a startElement,
    %% put that element onto the stack
    {State#{element_stack => [Event]},"","",[]};
es_1([],{endElement,_,_,_,_},_) ->
    throw("Closing tag without an opening tag");
es_1([],_,State) ->
    io:format(standard_error,"No change to element stack which is empty~n",[]),
    {State,"","",[]};
es_1(ES=[{startElement,ParentNS1,ParentTag1,_,_}|Ancestors1],Event={startElement,_,_,_,_},State) ->
    io:format(standard_error,"Push start element onto stack~n",[]),
    {State#{element_stack => [Event | ES]},ParentNS1,ParentTag1,Ancestors1};
es_1([{startElement,NS,Tag,_,_}|Ancestors1],{endElement,NS,Tag,_,_},State) ->
    {State#{element_stack => Ancestors1},NS,Tag,Ancestors1};    
es_1([{startElement,ParentNS1,ParentTag1,_,_}|_Ancestors1],{endElement,NS,Tag,_,_},_) ->
    Msg = must_match({"Opening tag namespace",ParentNS1},{"closing tag namespace",NS}) ++
	must_match({"End tag",Tag},{"Opening tag",ParentTag1}),
    {error,Msg};
es_1([{startElement,ParentNS1,ParentTag1,_,_}|Ancestors1],_,State) ->
    io:format(standard_error,"No change to element stack option2~n",[]),
    {State,ParentNS1,ParentTag1,Ancestors1}.


do_evt_1({startElement,"abc","file",_,AttrList},{S1,_,_,_}) ->    
    Filename = attrvalue("filename",AttrList),
    {ok,OldFD} = maps:find(output,S1),
    S1#{output => set_output_file(Filename,OldFD), first_tag => true};
do_evt_1(OT2={startElement,"abc","xml_tags",_,[]},{S1,_,_,_}) ->    
    {ok,FinalOutput} = maps:find(final_output,S1),
    case FinalOutput of
	true ->
	    S1#{escape_to_xml => true};
        false ->
	    {ok,OldFD} = maps:find(output,S1),
	    WriteNamespaces = get_namespaces(S1),
	    write_open_tag(OT2,OldFD,WriteNamespaces),
	    S1#{first_tag => false}
    end;
do_evt_1({startElement,"http://www.w3.org/2001/XInclude","include",_,AttrList},{S1,_,_,_}) ->    
    ?DEBUGSTR("xinclude start tag found"),
    Filename = attrvalue("href",AttrList),
    Parse = attrvalue("parse",AttrList,"xml"),
    insert_file(Filename,Parse,S1),
    S1;
do_evt_1(OT=#startElement{},{S1,_,_,_}) ->    
    ?DEBUGSTR("other start tag found"),
    WriteNamespaces = get_namespaces(S1),
    {ok,OldFD} = maps:find(output,S1),
    write_open_tag(OT,OldFD,WriteNamespaces),
    S1#{prefix_next_tag => "",first_tag => false};
do_evt_1({endElement,"http://www.w3.org/2001/XInclude","include",_},{S1,_,_,_}) ->    
    S1;
do_evt_1( CT2={endElement,"abc","xml_tags",_},{S1,_,_,_}) ->    
    {ok,FinalOutput} = maps:find(final_output,S1),
    case FinalOutput of
	true -> 
	    ok; 
	false ->
	    {ok,OldFD} = maps:find(output,S1),
	    write_close_tag(CT2,OldFD)
    end,
    S1#{escape_to_xml => false};
do_evt_1({endElement,"abc","file",_},{S1,_,_,_}) ->    
    {ok,DefaultOutput} = maps:find(default_output,S1),
    {ok,OldFD} = maps:find(output,S1),
    S1#{output => set_output_file(DefaultOutput,OldFD)};
do_evt_1(CT = #endElement{} ,{S1,_,_,_}) ->    
    {ok,ET} = maps:find(skip_end_tag,S1),
    case ET of
	true -> 
	    S1#{skip_end_tag => false};
	false ->
	    {ok,OldFD} = maps:find(output,S1),
	    write_close_tag(CT,OldFD),
 	    S1
    end;
do_evt_1({characters,Characters} ,{S1,_,_,_}) ->    
    {ok,Esc} = maps:find(escape_to_xml,S1),
    {ok,FinalOutput} = maps:find(final_output,S1),
    {ok,OldFD} = maps:find(output,S1),
    OutputChars =
       case Esc and FinalOutput of
	   true ->
	       reverse_escaping(Characters);
	   false ->
	       xml_output_escaping(Characters)
       end,
    write_characters(OutputChars,OldFD),
    S1;
do_evt_1({processingInstruction,Type,Value} ,{S1,_,_,_}) ->    
    {ok,OldFD} = maps:find(output,S1),
    io:fwrite(OldFD,"~s",["<?"++Type++Value++"?>"]),
    S1;

do_evt_1({startPrefixMapping,Prefix,Url} ,{S1,_,_,_}) ->    
    {ok,PrefixNextTag} = maps:find(prefix_next_tag,S1),
    {ok,Nis} = maps:find(names_in_scope,S1),
    S1#{prefix_next_tag => add_property({Prefix,Url},PrefixNextTag),
	names_in_scope  => add_property({Prefix,Url},Nis)};

do_evt_1({stopPrefixMapping,Prefix}, {S1,_,_,_}) ->
    ONis = maps:find(names_in_scope,S1),
    S1#{names_in_scope => proplists:delete(Prefix,ONis)};
    
do_evt_1({ignorableWhitespace,String} ,{S1,ParentNS,ParentTag,_}) ->    
	    %% ignore whitespace after a xml_tags tag
	    {ok,P} = maps:find(preserve_whitespace,S1),
	    PreserveWhitespace = 
		    P and not
		       ((ParentTag =="file") and (ParentNS=="abc")),
	    case PreserveWhitespace of
		true ->
		    {ok,OldFD} = maps:find(output,S1),
		    write_characters(String,OldFD);
		false -> 
		    ok
	    end,
    S1;
do_evt_1(_,{S1,_,_,_}) ->    
    S1.

get_namespaces(S1) ->
    get_namespaces(S1,maps:find(first_tag,S1)).
get_namespaces(S1,{ok,true}) ->
    {ok,NSDecl}=maps:find(names_in_scope,S1),
    NSDecl;
get_namespaces(S1,{ok,false}) ->		       
    {ok,NSDecl} = maps:find(prefix_next_tag,S1),
    NSDecl.

add_property(Entry={Key,_Value},PropList) ->
    L1 = proplists:delete(Key,PropList),
    [Entry|L1].

insert_file(Filename,[],S1) ->
    insert_file_xml(Filename,S1);
insert_file(Filename,"xml",S1) ->
    insert_file_xml(Filename,S1);
insert_file(Filename,"text",S1) ->
    {ok,OldFD} = maps:find(output,S1),
    insert_file_text(Filename,OldFD).
    
must_match({_,X},{_,X}) ->
    "";
must_match({L1,V1},{L2,V2}) ->
    io_lib:format(?MUST_MATCH,[L1,V1,L2,V2]).


attrvalue(AttName,AttrList) ->
    attrvalue(AttName,AttrList,throw).

attrvalue(AttName,[],throw) ->
    throw("Attribute "++AttName++" not found");
attrvalue(_,[],Default) ->
    Default;
attrvalue(AttName,[#attribute{localName=AttName,value=Value}|_],_) ->
    Value;
attrvalue(AttName,[_|T],Default) ->
    attrvalue(AttName,T,Default).

-spec set_output_file(string()) -> integer().
set_output_file(FileName) ->
    ?DEBUGSTR("setting output directory to"),
    ?DEBUG(FileName),
    ?DEBUGSTR("current directory is"),
    ?DEBUG(filename:absname(".")),

    {ok,F}=file:open(FileName,[write]),
    F.

set_output_file(FileName,standard_io) ->
    set_output_file(FileName);

set_output_file(FileName,OldFd) ->
    ok=file:close(OldFd),
    {ok,F} = file:open(FileName,[write]),
    F.

insert_file_xml(Filename,State) ->
    {ok,I} = maps:find(fileinclusions,State),
    State2 = case ordsets:is_element(Filename,I) of
	true ->
	    throw("recursive xinclude on file "++Filename);
	false ->
	    State#{fileinclusions => ordsets:add_element(Filename,I)}
    end,
    Str =
	case file:read_file(Filename) of
	    {ok,Str2} -> Str2;
	    {X,Y} -> 
		io:format(standard_error,"Could not read file ~s~n",[Filename]),
		throw({X,Y})
	end,
    erlsom:parse_sax(Str,State2,fun doevent/2).
    
-spec insert_file_text(string(), integer()) -> ok.
insert_file_text(Filename,FD) -> 
    Str1 =
	case file:read_file(Filename) of
	    {ok,Str} -> Str;
	    {error,enoent} ->
		throw (lists:flatten(io_lib:format("missing file ~s",[Filename])))
	end,
    Str2 = xml_output_escaping(binary_to_list(Str1)),
    io:fwrite(FD,"~s",["<pre>"]),
    io:fwrite(FD,"~s",[Str2]),
    io:fwrite(FD,"~s",["</pre>"]).

xml_parameter_formatting(X) ->
    xml_output_escaping(lists:flatten(io_lib:write(X))).

xml_output_escaping(S) ->
    [case X of 
	$< -> "&lt;"; 
	$" -> "&quot;"; 
	$> -> "&gt;"; 
	$' -> "&apos;"; 
	$& -> "&amp;"; 
	_ -> X 
    end 
    || X <- S ].

reverse_escaping(S) ->
    reverse_escaping(S,[]).

reverse_escaping([$&|T],Q) ->
    reverse_escape(T,[$&],Q);
reverse_escaping([H|T],Q) ->
    reverse_escaping(T,[H|Q]);
reverse_escaping([],Q) ->
    lists:reverse(Q).

reverse_escape(S,"&lt;",Q) ->
    reverse_escaping(S,[$<|Q]);
reverse_escape(S,"&quot;",Q) ->
    reverse_escaping(S,[$"|Q]);
reverse_escape(S,"&gt;",Q) ->
    reverse_escaping(S,[$>|Q]);
reverse_escape(S,"&apos;",Q) ->
    reverse_escaping(S,[$'|Q]);
reverse_escape(S,"&amp;",Q) ->
    reverse_escaping(S,[$&|Q]);
reverse_escape([X|S],A,Q) when length(A) < 6 ->
    reverse_escape(S,A++[X],Q);
reverse_escape(_,Seq,_) ->
    throw("Invalid escape sequence "++Seq).

write_open_tag({startElement,_,Tag,"",AttrList},FD,NSDecl) ->
    ?DEBUGSTR("write_open_tag rule1"),
    ?DEBUG(NSDecl),
    io:fwrite(FD,"~s",["<"++Tag++" "]), 
    ok=write_name_decl(NSDecl,FD),
    write_attributes(AttrList,FD),
    io:fwrite(FD,"~s",[">"]);

write_open_tag({startElement,_,Tag,Prefix,AttrList},FD,NSDecl) ->
    ?DEBUGSTR("write_open_tag rule2"),
    io:format(standard_error,"write_open_tag ~n  Prefix=~s~n  Tag=~s~n  Attributes=~p~n  Namespaces=~p~n",[Prefix,Tag,AttrList,NSDecl]),
    io:fwrite(FD,"~s",["<"++Prefix++":"++Tag++" "]), 
    ok=write_name_decl(NSDecl,FD),
    write_attributes(AttrList,FD),
    io:fwrite(FD,"~s",[">"]).

write_close_tag({endElement,_,LocalName,""}, FD)->
   io:fwrite(FD,"~s",["</"++LocalName++">"]);

write_close_tag({endElement,_,LocalName,Prefix},FD) ->
    io:fwrite(FD,"~s",["</"++Prefix++":"++LocalName++">"]).

write_name_decl([],_) ->
    ok;
write_name_decl([{"",Url}|T],FD) -> 
    NS = " xmlns=\""++Url++"\"",
    io:format(standard_error,"write_name_decl (no prefix) url=~s~n",[Url]),
    io:fwrite(FD,"~s",[NS]),
    write_name_decl(T,FD);
    
write_name_decl([{Prefix,Url}|T],FD) ->
    NS = " xmlns:"++Prefix++"=\""++Url++"\"",
    ?DEBUG(NS),
    io:format(standard_error,"write_name_decl prefix=~s url=~s~n",[Prefix,Url]),
    io:fwrite(FD,"~s",[NS]),
    write_name_decl(T,FD).

write_attributes([],_) ->  ok;
write_attributes([{attribute,LocalName,_Uri,Prefix,Value}|T],FD) ->
    io:fwrite(FD,"~s",[" "++Prefix++LocalName++"=\""++Value++"\""]),
    write_attributes(T,FD).

write_characters(String,FD) ->
    io:fwrite(FD,"~s",[String]).


do_test("test") ->
    test();
do_test("test1") ->
    test1();
do_test("test2") ->
    test2();
do_test("test3") ->
    test3();
do_test("test4") ->
    test4();
do_test("test5") ->
    test5();
do_test("test6") ->
    test6();
do_test("test7") ->
    test7();
do_test("test8") ->
    test8();
do_test("test9") ->
    test9();
do_test("test10") ->
    test10();
do_test(X) ->
    throw("unknown test "++X).

test_cmd() ->
    %"escript ebin/xslt_extn.beam ".
    "./escript ".


test() ->
    io:format("testing started, current directory is ~n~s~n",[filename:absname(".")]),
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6(),
   % test7(),
    test8(),
    test9(),
    test10(),
    ok.

test1() ->
    Testid = "test1",
    InputFile = "test/input/test1.xml",
    OutputFile = filename:join(["test/output",Testid])++".xml",
    ErrorFile = filename:join(["test/output",Testid])++".err.txt",
    Options = "",
    io:format(standard_error,"~s~n",[Testid]),
    Cmd = test_cmd()++ Options ++" < " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    ?DEBUG(Cmd),
    os:cmd(Cmd),
    ok.

test2() ->
    Testid = "test2",
    InputFile = "test/input/testinclusion.xml",
    OutputFile = filename:join(["test/output",Testid])++".xml",
    ErrorFile = filename:join(["test/output",Testid])++".err.txt",
    Options = "",
    io:format(standard_error,"~s~n",[Testid]),
    Cmd = test_cmd()++ Options ++" < " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    ?DEBUG(Cmd),
    os:cmd(Cmd),
    ok.
    
test3() ->
    Testid = "test3",
    InputFile = "test/input/testinclusion2.xml",
    OutputFile = filename:join(["test/output",Testid])++".xml",
    ErrorFile = filename:join(["test/output",Testid])++".err.txt",
    Options = "",
    io:format(standard_error,"~s~n",[Testid]),
    Cmd = test_cmd()++ Options ++" < " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    ?DEBUG(Cmd),
    os:cmd(Cmd),
    ok.
    
test4() ->
    Testid = "test4",
    InputFile = "test/input/testinclusion3.xml",
    OutputFile = filename:join(["test/output",Testid])++".xml",
    ErrorFile = filename:join(["test/output",Testid])++".err.txt",
    Options = "",
    io:format(standard_error,"~s~n",[Testid]),
    Cmd = test_cmd()++ Options ++" < " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    ?DEBUG(Cmd),
    os:cmd(Cmd),
    ok.
    
test5() ->
    io:format("test5~n"),
    InputFile = "test/input/test_file_output.xml",
    OutputFile = "test/output/test5.xml",
    ErrorFile = "test/output/test5.err.txt",
    Options = "+finaloutput",
    Cmd = test_cmd()++ Options ++" < " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    ?DEBUG(Cmd),
    os:cmd(Cmd),
    ok.

test6() ->
    Testid = "test6",
    OutputFile = filename:join(["test/output",Testid])++".xml",
    io:format(standard_error,"~s~n",[Testid]),
    Fd = set_output_file(OutputFile),
    {ok,Str} = file:read_file("test/input/test_namespaces.xml"),
    S0 = default_state(),
    parseStr(Str,S0#{output => Fd}),
    ok.

test7() ->    
    Testid = "test7",
    io:format(standard_error,"~s~n",[Testid]),
    %% Test recursive xml include
    InputFile = "test/input/recursive_include_xml.xml",
    OutputFile = "test/output/test7.xml",
    ErrorFile = "test/output/test7.err.txt",
    Options = "",
    Cmd = test_cmd()++ Options ++" < " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    os:cmd(Cmd).

test8() ->
    Testid = "test8",
    io:format(standard_error,"~s~n",[Testid]),
    %% test reverse escaping
    "<tag>" = reverse_escaping("&lt;tag&gt;"),
    "a<tag>b" = reverse_escaping("a&lt;tag&gt;b"),
    "\"hi\'" = reverse_escaping("&quot;hi&apos;"),
    "&" = reverse_escaping("&amp;"),
    ok.

test9() ->
    Testid = "test9",
    io:format(standard_error,"~s~n",[Testid]),
    %% test xml_tags  +finaloutput (default)
    %% expect validly formed xml output
    %% expect tags in escaped format
    %% test reverse escaping in final output
    InputFile = "test/input/xml_tags_tag.xml",
    OutputFile = "test/output/test9.xml",
    ErrorFile = "test/output/error9.txt",
    Options = "+finaloutput",
    %% test relative directories okay
    true = filelib:is_dir("test/input"),
    true = filelib:is_dir("test/output"),
    %% generate test data
    Data = ["<root xmlns:q=\"abc\">",
	     "<q:xml_tags>&lt;tag/&gt;</q:xml_tags>",
	     "</root>"],
    file:write_file(InputFile,
		    Data),
    %% run test
    ?DEBUGSTR("Test9 running test"),
    Cmd = test_cmd()++Options++" < " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    ?DEBUG(Cmd),
    os:cmd(Cmd),
    %% check result

    case filelib:is_regular(OutputFile) of
	true ->
	    {ok,R} = file:read_file(OutputFile),
	    io:format("~s~n",[R]);
	false ->
	    throw("test 9 did not generate an output file")
    end.

test10() ->
    Testid = "test10",
    io:format(standard_error,"~s~n",[Testid]),
    %% test xml_tags not final output (default)
    %% expect validly formed xml output
    %% expect tags in escaped format
    InputFile = "test/input/xml_tags_tag.xml",
    OutputFile = "test/output/test10.xml",
    ErrorFile = "test/output/error10.txt",
    Options = "",
    %% test relative directories okay
    true = filelib:is_dir("test/input"),
    true = filelib:is_dir("test/output"),
    %% generate test data
    Data = ["<root xmlns:q=\"abc\">",
	     "<q:xml_tags>&lt;tag/&gt;</q:xml_tags>",
	     "</root>"],
    file:write_file(InputFile,
		    Data),
    %% run test
    ?DEBUGSTR("Test10 running test"),
    Cmd = test_cmd()++Options++"< " 
	++ InputFile ++ " > " ++ OutputFile
	++ " 2> " ++ ErrorFile,
    ?DEBUG(Cmd),
    os:cmd(Cmd),
    %% check result

    case filelib:is_regular(OutputFile) of
	true ->
	    {ok,R} = file:read_file(OutputFile),
	    io:format("~s~n",[R]);
	false ->
	    throw("test 10 did not generate an output file")
    end.
    
