-module (mymake).
%% *********************************************************
%% Use erlang xref data to recompile files as required
%% Advantages over conventional make files:
%%   - minimal configuration.  
%% When is a compile required?
%%   (1) when the product does not exist. 
%%   TODO: (2)
%%   (2) when the product is out of date
%%     (2.1) the product depends on some source objects
%%     (2.2) one or more of those source objects has changed
%% ********************************************************* 
%TODO: process cross references
-export([all/0,main/1,test/0]).
-include_lib("kernel/include/file.hrl").

-record (rule,{src_suffix,src_dir="src",dst_suffix,dst_dir="ebin",compiler_opts}).
get_static_deps() ->
    [#rule{src_suffix=".yrl",src_dir="src",dst_suffix=".erl",dst_dir="src",compiler_opts=[debug_info]},
     #rule{src_suffix=".erl",src_dir="src",dst_suffix=".beam",dst_dir="ebin",compiler_opts=[{include_dir,"include"},debug_info]}].

report(CmpRes) ->
    io:format("~w~n",[CmpRes]).
make_dep_file(Src,Rule) ->
    io:format("compiling ~s~n",[Src]),
    #rule{dst_dir=DestDir,compiler_opts=Opts} = Rule,
    CmpRes = compile:file(Src,[{outdir,DestDir},Opts]),
    report(CmpRes).

check_dep_file(Src,Dst,Rule) ->
    io:format("check_dep_file called with the following parameters~n~s~n~s~n",[Src,Dst]),
    %% check for Prod
    case filelib:is_file(Dst) of
	true ->
	    {ok,#file_info{mtime=SrcFileDate}} = file:read_file_info(Src,[posix]),
	    {ok,#file_info{mtime=DstFileDate}} = file:read_file_info(Dst,[posix]),
	    case SrcFileDate > DstFileDate of
		true ->
		    make_dep_file(Src,Rule);
		false ->
		    ok
	    end;
	false ->
	    make_dep_file(Src,Rule)
    end.
    
merge(Z1=[H1|L1],Z2=[H2|L2],Acc) ->
    H1base = filename:basename(H1),
    H2base = filename:basename(H2),
    if
	(H1base<H2base) ->
	    merge(L1,Z2,[{H1,none}|Acc]);
	(H2base<H1base) ->
	    merge(Z1,L2,[{none,H2}|Acc]);
	(H1base=:=H2base) ->
	    merge(L1,L2,[{H1,H2}|Acc])
    end;
merge([H1|L1],[],Acc) ->
    merge(L1,[],[{H1,none}|Acc]);
merge([],[H2|L2],Acc) ->
    merge([],L2,[{none,H2}|Acc]);
merge([],[],Acc) ->
    Acc.
    
do_static(Rule) ->
    %% work on the rule stuff...
    SrcPattern = "*"++Rule#rule.src_suffix,
    #rule{src_dir=SrcDir,dst_dir=DstDir} = Rule,
    io:format("SrcPattern ~s~n",[SrcPattern]),
    SrcList1 =
	lists:sort(
	  filelib:wildcard(
	    SrcPattern,
	    SrcDir)),
    SrcList = [filename:join(SrcDir,X)  || X <- SrcList1],
    %io:format("SrcList~n~p~n",[SrcList]),
    DstList1 =
	lists:sort(
	  filelib:wildcard(
	    "*"++Rule#rule.dst_suffix,
	    Rule#rule.dst_dir)),
    DstList = [filename:join(DstDir,X) || X <- DstList1],
    %io:format("DstList~n~p~n",[DstList]),
    MrgList =
	merge(SrcList,DstList,[]),
    %io:format("MrgList~n~p~n",[MrgList]),
    lists:map(
      fun({Src,none}) ->
	      %% Src but no dest, compile
	      make_dep_file(Src,Rule);
	 ({none,Dst}) ->
	      %% Dst but no source, issue warning
	      {warning,lists:append([Dst," has no corresponding source file."])};
	 ({Src,Dst}) ->
	      %% both source and dest, need to check if re-compile is needed
	      check_dep_file(Src,Dst,Rule)
      end,
      MrgList).

do_static_deps() ->
    StaticDeps = get_static_deps(),
    [do_static(X) || X <- StaticDeps].

all() ->
    {ok,Cwd}=file:get_cwd(),
    io:format("calculating static dependencies~ndir = ~s~n",[Cwd]),
    do_static_deps().

main(["test"]) ->
    io:format("main/1 test parameter~n"),
    test();
main(_) ->
    io:format("main/1 ignoring parameters~n"),
    all().

test1() ->
    %% ***********************************
    %% New file should be compiled
    %% ***********************************
    file:write_file("src/empty.erl",lists:append(["-module(empty).",io_lib:nl()])),
    all(),
    case filelib:is_file("ebin/empty.beam") of
	true ->
	    {ok,"made empty.beam okay~n"};
	false ->
	    {error,"failed to make empty.beam~n"}
    end.
    
test2b(false) ->
    {error,"source file not recompiled after change"};
test2b({vsn,Vsn}) ->
    io:format("Read version number is ~w~n",[Vsn]),
    {ok,"source file recompiled after change"}.

test2() ->
    %% ***********************************
    %% Modified file should be compiled
    %% ***********************************
    file:write_file("src/empty.erl",lists:append(["-vsn(2.0).",io_lib:nl()]),[append]),
    all(),
    Attrs=empty:module_info(attributes),
    test2b(lists:keyfind(vsn,1,Attrs)).
    
test3b(true) ->
    {ok,"unchanged file not compiled"};
test3b(false) ->
    {error,"unchanged file is compiled"}.

test3() ->
    %% ***************************************
    %% Unmodified file, no reason to recompile
    %% ***************************************
    {ok,#file_info{mtime=BeforeSeconds}} = file:read_file_info("ebin/empty.beam",[posix]),
    all(),
    {ok,#file_info{mtime=AfterSeconds}} = file:read_file_info("ebin/empty.beam",[posix]),
    test3b(BeforeSeconds =:= AfterSeconds).

sleep(Time) ->
    receive
    after (Time*1000) -> ok
    end.

test() ->
    os:cmd("rm -r mymake_test"),
    os:cmd("mkdir mymake_test"),
    os:cmd("mkdir mymake_test/src"),
    os:cmd("mkdir mymake_test/ebin"),
    file:set_cwd("mymake_test"),
    {ok,_} = test1(),
    sleep(1),
    {ok,_} = test2(),
    {ok,_} = test3().
