-module (worm_fs).

%% A file system built on Write Once Read Many technology
%% Overwriting files is BAD in that it destroys history
%% Large drives are now cheap so lets not rewrite.
%%
%% Another objective is that each session is essentially an isolated
%% transaction.  Writes by other processes are invisible until
%% the view of the file system is refreshed.
%%
%% As all writes are to the end of the filesystem, this means
%% that all writes after the begin_transaction are not visible.
%%
%% Every single process therefore needs its own view of the
%% file system.  Making an actual copy is large state and
%% not the sort of thing to be carried around or deep copied.
%% But as file writes are made to the end of the filesystem,
%% the answer is to keep this state in a central server,
%% and filter out any data with an address greater than that
%% indicated in start_transaction.
%%

-behaviour(gen_server).
%% Gen_server used to store/manage large shared state
%% Don't want shared state available to clients 
%% as it needs to be filtered before they get it.

-export([
	 begin_transaction/0,
	 end_transaction/0,
	 maybe_end_transaction/0,
	 refresh/0,
	 write_file/3,
	 read_file/2,
	 dir/1,
	 dir_tree_re/2,
	 dir_tree_re/3,
	 fold/3
	]).

%% Session_state records the state of a processes_session
-type in_transaction() :: boolean().
-type filename() :: string().
-type regexp() :: string().
-type file_list() :: [filename()].

-record(session_state,
	{
	  transaction :: in_transaction(),
	  fs_max :: non_neg_integer(),
	  current_dir :: inode()
	}).

-include("inode.h").

-record(wormfs_state,
	{
	  user_sessions :: dict(),
	  sblock :: superblock()
	}).


refresh() ->
    maybe_end_transaction(),
    begin_transaction().

-spec write_file(FS :: reference(), Filename :: string(), Data :: binary()).
write_file(FileSystem,Filename,Data) ->
    %% this routine executed in client space...
    gen_server:call(FileSystem,{write_file,self(),Filename,Data}).

-spec read_file(FS :: reference(), Pathname :: string()).
read_file(FileSystem,Pathname) ->
    gen_server:call(FileSystem,{read_file,self(),Filename}).

-spec dir_tree_re(FS :: reference()) -> filelist().
dir(FileSystem) ->
    gen_server:call(FileSystem,{dir,self()}).

-spec dir_tree_re(FS :: reference(), Dir :: string()) -> filelist().
dir_tree_re(FileSystem,Dirname) ->
    gen_server:call(FileSystem,{dirtreere,self(),Dirname,".*"}).

-spec dir_tree_re(FS :: reference(), Dir :: string(), FileFilter :: regexpr())
		 -> filelist().
dir_tree_re(FileSystem,Dirname,FileFilter) ->
    gen_server:call(FileSystem,{dirtreere,self(),Dirname,FileFilter}).


