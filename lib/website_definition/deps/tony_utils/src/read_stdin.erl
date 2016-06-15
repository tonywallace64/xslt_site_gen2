-module (read_stdin).
%% Instead of appending blocks they are reversed
%% and prepended, and reversed again at the end.
%% This should be more efficient for small block sizes
-export([read_until_eof/0,read_until_eof/1]).

read_until_eof() ->
    read_until_eof(1024).

read_until_eof(BlockSize) ->
    S1=readinput([],[],BlockSize),
    lists:flatten(lists:reverse(S1)).

readinput(Acc,eof,_) ->
    Acc;
readinput(Acc,Next,BlockSize) ->
    readinput([Next|Acc],io:get_chars('',BlockSize),BlockSize).


    
