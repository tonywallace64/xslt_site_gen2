-module (documentation).
-export([generate/0]).

generate() ->
  {ok,RawFileList} = file:list_dir("src"),
  FilteredList = lists:filter(fun (X) -> filename:extension(X) == ".erl" end, RawFileList),
  Q = lists:map(fun (X) -> lists:append(["src/",X]) end, FilteredList),
  edoc:files(Q,[{dir,"doc"}]).
