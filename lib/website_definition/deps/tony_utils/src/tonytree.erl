-module (tonytree).

-export([new/1,make_tree/2,get/3]).
%% Attempt to build a tree structure that supports
%% xpath operations (at least the child,parent,prev,next
%% axes that can be extended to decendent, ancestor etc.
%%
%% These can be deduced from a given node identifier so
%% that the client can move by executing a
%% string(xpath_expression,current_node,root()) command.
%%
%% let the xpath expression be compiled into
%%   compiled_xpath_expr = [xpath_component]
%%   xpath_component = {axis,node_specifier,[qualifier]}
%%
%% To support these operations, the data nodes are of the form
%% data_node = {parent()|nil,prev()|nil,next()|nil,first_child()|nil,last_child()|nil,Contents}
%% parent() = noderef(),
%% prev() = noderef(),
%% next() = noderef(),
%% first_child() = noderef()
%% root() = noderef()=1,
%% self() = noderef().
%%
%% These nodes are stored in a dictionary with {key,value} pairs of the form
%% {self(),data_node()}
%%
%% noderef() = positive_integer()
%%

-record(data_node,{self,parent=nil,prev=nil,next=nil,first_child=nil,last_child=nil,content}).
-type xdm() :: {integer(),#data_node{}}.

%% gb_trees used as a keyvalue store
%% macros used to enable choice of different storage
%% library
-define(New(Size),array:new([{size,Size},{fixed,false}])).
-define(Insert(Key,Value,Tree),array:set(Key,Value,Tree)).
-define(Update(Key,Value,Tree),array:set(Key,Value,Tree)).
-define(Lookup(Key,Tree),{value,array:get(Key,Tree)}).


-spec new(any()) -> xdm().
new(Content) ->
    {1,?Insert(1,#data_node{self=1,content=Content},?New(5000))}.

-spec insert_last_child(any(),reference(),xdm()) -> xdm().
insert_last_child(Content,CurrentRef,{MaxNode,Tree}) ->
    {value,ParentNode}
	= ?Lookup(CurrentRef,Tree),
    {value,OldLast}
	= ?Lookup(CurrentRef#data_node.last_child,Tree),
    R=MaxNode+1,
    #data_node{last_child=Prev} = ParentNode,
    T1=?Insert(R,#data_node{self=R,parent=CurrentRef,prev=Prev,content=Content},Tree),
    T2=?Update(CurrentRef,ParentNode#data_node{last_child=R},T1),
    T3=?Update(OldLast#data_node.self,OldLast#data_node{next=R},T2),
    {R,T3}.

%-spec make_tree(fun()->node(),fun(node())->boolean()) -> xdm().
make_tree(GetNode,IsLeafFun) ->
    Node = GetNode(),
    XDM = {1,_Tree} = new(Node), 
    NewNode = GetNode(),
    make_tree(GetNode,IsLeafFun,Node,NewNode,IsLeafFun(Node),XDM).

% make_tree(GetNode:: fun(),IsLeafFun::fun(),ParentNode,NodeToInsert,IsNewNodeALeaf,XDM)
make_tree(GetNode,IsLeafFun,ParentNode,NewNode,open_tag,XDM) ->
    %% inserting a tree node
    insert_last_child(NewNode,ParentNode,XDM),
    ReadNode = GetNode(),
    %% recursive call into the new node as new parent
    make_tree(GetNode,IsLeafFun,NewNode,ReadNode,IsLeafFun(ReadNode),XDM);

make_tree(GetNode,IsLeafFun,ParentNode,NewNode,next,XDM) ->
    %% inserting a leaf node
    NewXDM = {_NewRef,_Tree} = insert_last_child(NewNode,ParentNode,XDM),
    ReadNode = GetNode(),
    make_tree(GetNode,IsLeafFun,ParentNode,ReadNode,IsLeafFun(ReadNode),NewXDM);

make_tree(GetNode,IsLeafFun,ParentNode,NewNode,close_tag,XDM) ->
    %% closing tag
    ReadNode = GetNode(),
    NewParent = move(parent,ParentNode,XDM),
    make_tree(GetNode,IsLeafFun,NewParent,ReadNode,IsLeafFun(ReadNode),XDM).

move(self,Ref,_) ->
    Ref;
move(_,nil,_) -> nil;
move(parent,Ref,{_Root,Tree}) ->
    {value,Node} = ?Lookup(Ref,Tree),
    Node#data_node.parent;
move('last-child',Ref,{_Root,Tree}) ->
    {value,Node} = ?Lookup(Ref,Tree),
    Node#data_node.last_child;
move('first-child',Ref,{_Root,Tree}) ->
    {value,Node} = ?Lookup(Ref,Tree),
    Node#data_node.first_child;
move(prev,Ref,{_Root,Tree}) ->
    {value,Node} = ?Lookup(Ref,Tree),
    Node#data_node.prev;
move(next,Ref,{_Root,Tree}) ->
    {value,Node} = ?Lookup(Ref,Tree),
    Node#data_node.next.


get(Axis,Current,XDM) ->
    lists:flatten(get2(Axis,Current,XDM)).
get2(_,nil,_) ->
    %% following links will eventually lead to a nil reference
    [];
get2('following-sibling',Ref,XDM) ->
    assemble(next,Ref,XDM,[]);
get2('proceding-sibling',Ref,XDM) ->
    assemble(prev,Ref,XDM,[]);
get2(child,Ref,Z) ->
    FirstChild = move('first-child',Ref,Z),
    get2('following-sibling',FirstChild,Z);
get2(parent,Ref,XDM) ->    
    [move(parent,Ref,XDM)];
get2(ancestor,Ref,XDM) ->
    assemble(parent,Ref,XDM,[]);
get2('ancestor-or-self',Ref,XDM) ->
    [self()|get2(ancestor,Ref,XDM)];
get2(descendent,Ref,XDM) ->
    Children = get2(child,Ref,XDM),
    %% note that all nodes need to be in "document order"
    %% (XML Path language specification).  This means
    %% left recursion, that is all children of prev
    %% sibling need to come before the children of the
    %% next sibling.
    [get('descendent-or-self',X,XDM) || X <- Children];
get2('descendent-or-self',Ref,XDM) ->
    [Ref|get2(descendent,Ref,XDM)];
get2(preceding,Ref,XDM) ->    
    %% scan through nodes in reverse document order, when an
    %% ancestor of the ref node is encountered skip that node
    %% and process ancestors preceeding nodes.
    %% Finally reverse the list and return.
    ParentNode = move(parent,Ref,XDM),
    P=preceding_scan(Ref-1,ParentNode,XDM,[]),
    lists:reverse(P);
get2(following,Ref,XDM) ->
    case move(next,Ref,XDM) of
	nil ->
	    get2(following,move(parent,Ref,XDM),XDM);
	Next ->
	    following_scan(Next,XDM,[])
    end.

preceding_scan(0,_,_,Acc) -> Acc;
preceding_scan(Parent,Parent,XDM,Acc) ->
    %% encounted a parent node,
    preceding_scan(Parent-1,move(parent,Parent,XDM),XDM,Acc);
preceding_scan(Ref,Parent,XDM,Acc) ->
    preceding_scan(Ref-1,Parent,XDM,[Ref|Acc]).
    
following_scan(Max,{Max,_},Acc) -> 
    lists:reverse([Max|Acc]);
following_scan(Ref,XDM,Acc) -> 
    following_scan(Ref+1,XDM,[Ref|Acc]).

    



%% Assemble is to return a list of
%% qualifing nodes starting with the 
%% closest to the current node.
%%
%% By default we want to assemble 
%% along axis not including the start node
%% If an axis requires a self node this
%% can be done by initialising NodeSet to [self]
%% For this reason assemble is broken into assemble
%% and assemble2 with assemble omitting to store
%% the initial node reference.
assemble(_,nil,_,Nodeset) ->
    Nodeset;
assemble(PathDirection,SelfNode,XDM,Nodeset) ->
    NextNode = move(PathDirection,SelfNode,XDM),
    assemble2(PathDirection,NextNode,XDM,Nodeset).
assemble2(_,nil,_,Nodeset) ->    
    Nodeset;
assemble2(PathDirection,CurrentNode,XDM,Nodeset) ->
    NextNode = move(PathDirection,CurrentNode,XDM),
    Rest = 
	assemble2(PathDirection,NextNode,XDM,Nodeset),
    [CurrentNode|Rest].
    
test_assembly() ->
    tests_not_written.
    

    
    
