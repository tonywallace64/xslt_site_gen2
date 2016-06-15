-module(filesystem).
% Objectives
% 1) WORM - write once read many
% 2) Lossless (consequence of WORM)
% 3) Write to front
%  Clients read file information from front of file and access accordingly
%  When  new data prepended old clients still have a consistent old version to work from
% 4) Handle large directories efficently
% 5) Versioned when a new version of a file written to front of system old versions of file remain for history
%  and auditing purposes

% Discussion
% When a new version of a file is written to the front of the file, a directory entry referencing that entry
% must be prepended to the front of the file as well so that the new data can be referenced.  Let a directory be
% a tree of pages the page containing that new reference is by definition now out of date, and so on up the tree to the 
% root of the directory.  Changes with a directory result in a new directory root entry for that directory, which
% then requires changes that propogate up to the root of the directory tree.
%
% This however is not burdensome as the number of levels of updated index is a log of the number of entries.  The
% base of the log being the number of entries in each index page.  The cost in the number of index entries to be
% updated can be reduced by delaying the update of the directory so that some of the new updated nodes (particularly
% those near the root are shared.  Indeed, this process can be deferred indefinitely by keeping the index in memory
% but for reasons of data security it should be periodically written to disc.
%
% index_page =
%  {
%		this_disc_offset,
%		name,
%               type,
%		data_disc_offset,
%		data_length,
%		left_tree,
%		right_tree
%  }
%  
% this_disc_offset - integer
%       The location on disc of this index page
%	Useful when index_page is stored in memory
% left_tree, right_tree - disc_offset | index_page_cache_entry | null
% name - string
% type - file | directory
% data_disc_offset - offset to data on WORM device
% data_length - integer
%
% index_page_cache_entry =
%  {
%    cache_offset
%  }
%
% Maintaining the dirty list of directory nodes requires a linked list when is not compatable with this recursive
% tree structure.  One way to resolve this is to store the nodes in a dict structure instead and index that dict
% by node numbers.  The changes to the index_page structure above are as follows:
% 
% -type(ipage,non_neg_integer()).
%    ipage is the offest to the referenced page, or zero if no such page exists.
%
% -record index_page{
%    name :: string(), 
%    this_disc_offset :: integer(), 
%    type :: 'file'|'directory',
%    data_length :: integer(),
%    next_dirty :: ipage(),
%    left_tree :: ipage(),
%    right_tree :: ipage()
%  }
% 


