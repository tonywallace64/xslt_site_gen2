-module (worm).

%% IMPORTANT
%% This module makes is subject to race conditions in multi process situations
%% where more than one process is updating a file.  In such situations it
%% MUST be wrapped in a single process or some form of locking employed to protect data.

%% NOTE
%% The data visible to the client is the state of the file at the time it was opened
%% Writes (including those made by the client themselves) are not visible until
%% the file is reopened or flushed.

% this is a wrapper over a standard unix file that has the follow characteristics:
% 1) The file is appended to: overwriting is not permitted.
%    If an attempt is made to write to a file while not in the position for appending,
%    an error is returned.
% 2) Data is written in reverse order from front to back
% 3) Data is read from the back of the file towards the front
% 4) File position references are from the front towards the back,
%   the file position of new data is always increasing.


-export ([seek/2,read/2,write/2,open/1,flush/1,close/1,test/0]).
-include_lib("kernel/include/file.hrl").

-type os_filename() :: string().
-type file_ctl_rec() :: {FileDesc:: integer(), Filename:: string(), ReadPosition :: integer(), MaxOffset :: integer()}.
-type file_position() :: integer().

-spec open(os_filename())  -> file_ctl_rec().

open(Filename) ->
    {ok,Fd} = file:open(Filename,[read,append,binary]),
    ReadPosition = filelib:file_size(Filename),
    {Fd,Filename,ReadPosition,ReadPosition}.

-spec close(file_ctl_rec()) -> ok.
close({Fd,_,_,_}) ->
    file:close(Fd).

-spec flush(file_ctl_rec()) -> file_ctl_rec().
flush({Fd,FileName,_,_}) ->
    {ok,FI} = file:read_file_info(FileName),
    Size = FI#file_info.size,
    {Fd,FileName,Size,Size}.
    
-spec write(file_ctl_rec(),iolist()) -> {ok,file_position()}.
write(X,Data) when is_list(Data) ->
    write(X,list_to_binary(Data));
write({Fd,FileName,_,_},Data) ->
    WriteTransaction = 
	fun() ->
		ok=file:write(Fd,b_rev(Data)),
		{ok,FI} = file:read_file_info(FileName),
		Size = FI#file_info.size,
		{ok,Size}
	end,
    transaction(WriteTransaction).

-spec read(file_ctl_rec(),integer()) -> {ok,file_ctl_rec(),binary()}.
read(X={Fd,FileName,RP,MRP},DataLength) ->
    case DataLength =< RP of
	true ->
	    From = RP-DataLength,
	    {ok,Data} = file:pread(Fd,From,DataLength),
	    {ok,{Fd,FileName,RP-DataLength,MRP},b_rev(Data)};
	false -> 
	    read(X,RP)
    end.

-spec seek(file_ctl_rec(),NewOffset :: integer()) -> file_ctl_rec().
seek({Fd,FileName,RP,MRP},NewOffset) when RP =< MRP-> 
    {Fd,FileName,NewOffset,MRP}.

b_rev(X) when is_list(X) ->
    b_rev(list_to_binary(X));
b_rev(<<>>) -> <<>>;
b_rev(<<H:8,T/binary>>) ->
    R = b_rev(T),
    <<R/binary,H:8>>.

transaction(Fun) ->
    Fun().

test() ->
    <<1,2,3,4>> = b_rev(<<4,3,2,1>>),
    transaction(fun() ->
			io:format("transaction executes okay\n") end),
    Fn = "test.data",
    ok = file:write_file(Fn,<<>>),
    Fd = open(Fn),
    {ok,Fd2,<<>>} = read(Fd,all),
    {ok,5}        = write(Fd,"abcde"),
    {ok,Fd3,<<>>} = read(Fd2,5),
    Fd4           = flush(Fd3),
    {ok,Fd5,<<"abcde">>} = read(Fd4,5),
    % now if new data is added, it should go to the front
    {ok,11} = write(Fd5,"line 2"),
    Fd6    = flush(Fd5),
    {ok,_,<<"line 2abc">>} = read(Fd6,9),
    ok = close(Fd6),
    Fd6b= open(Fn),
    {ok,Fd7,<<"line 2abc">>} = read(Fd6b,9),
    {ok,_,<<"de">>} = read(Fd7,5),
    'test passed okay'.



    
