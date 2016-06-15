%% @doc
%% gen_server that supervise and restarts event handlers (that implement the 
%% gen_event behavior) added with gen_event:add_sup_handler/3, suitable to be 
%% used in a standard otp supervision tree
%%
%% Changes AJW:
%% 1/ Make a manager for the events to handle
%% 2/ Add handlers to this manager
%% 3/ replaced lager:error with localerror as I did not have the library lager
%% @end

-module(event_handler_guard).
%-author('marcelog@gmail.com').
-author('tony@tony.gen.nz'). 
-behavior(gen_server).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {manager_pid::pid()}).
-type state():: #state{}.
-type name()::{local,atom()}|{global,atom()}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([start_link/2]).
 
%%% gen_server callbacks.
-export([
  init/1, terminate/2, code_change/3,
  handle_call/3, handle_cast/2, handle_info/2
]).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the gen_server. Links an event manager to the gen_server, then
%% Accepts a list of module name , list of parameter
%% pairs, which are the event handlers needed to start and monitor.
-spec start_link(name(),[{module(),[term()]}|module()]) -> {ok, pid()} | ignore | {error, term()}.
start_link({'local',NameValue},EventHandlers) 
     when is_list(EventHandlers) and is_atom(NameValue) ->
  start_link_1({'local',NameValue},EventHandlers);
start_link({'global',NameValue},EventHandlers) 
     when is_list(EventHandlers) ->
  start_link_1({'global',NameValue},EventHandlers);
start_link({'via',NameValue},EventHandlers) 
     when is_list(EventHandlers) ->
  start_link_1({'via',NameValue},EventHandlers).

start_link_1(Name,EventHandlers) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {Name,EventHandlers}, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init({name(),[{module(),[any()]}|module()]}) -> {ok, state()}.
init({Name={'local',NameValue},EventHandlers}) 
    when is_list(EventHandlers) and is_atom(NameValue) ->
  init_1(Name,EventHandlers);
init({Name={'global',_},EventHandlers}) 
    when is_list(EventHandlers) ->
  init_1(Name,EventHandlers);
init({Name={'via',_},EventHandlers}) 
    when is_list(EventHandlers) ->
  init_1(Name,EventHandlers);
init(_) ->
  throw("invalid name or handler list format: use name format {local,atom()} | {global,term() | {via,term()}"). 
init_1(Name,EventHandlers) 
     when is_list(EventHandlers) ->
  {ok,Manager} = gen_event:start_link(Name), 
  [start({Manager,X})  || X <- EventHandlers],
  {ok, #state{manager_pid=Manager}, hibernate}. 
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
  local_error("Invalid cast: ~p", [Msg]),
  {noreply, State, hibernate}.
 
%% @doc Expects a gen_event_EXIT message and re-starts the event handler.
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
  local_error("Event Handler ~p died with: ~p", [Handler,Reason]),
  start(Handler),
  {noreply, State, hibernate};
 
handle_info(Msg, State) ->
  local_error("Invalid msg: ~p", [Msg]),
  {noreply, State, hibernate}.
 
-spec handle_call(
  term(), {pid(), reference()}, state()
  ) -> {reply, term() | {invalid_request, term()}, state()}.
handle_call(Req, _From, State) ->
  local_error("Invalid request: ~p", [Req]),
  {reply, invalid_request, State, hibernate}.
 
-spec terminate(atom(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.
 
-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->  
  {ok, State}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc start assigns each handler to the locally created event manager
%% 
-spec start(module()) -> ok | {'EXIT', term()} | term().
start({Manager,{Handler,Args}}) ->
  gen_event:add_sup_handler(Manager,Handler,Args);
start({Manager,Module}) ->
  gen_event:add_sup_handler(Manager,Module,[]).

local_error(F,S) ->
  say(F,S),
  throw (error).

say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

