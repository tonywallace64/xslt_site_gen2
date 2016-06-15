%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(static_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).
-export([main/1]).

%% API.
main(Args) ->
    %% wait for supervisor to exit before exiting script
    {ok,SupPid} = start(normal,Args),
    Ref = monitor(process,SupPid),
    receive
	{'DOWN',Ref,process,SupPid,_Reason} ->
	    ok
    end.

start(_Type, _Args) ->
    io:format(standard_error,"Starting applications~n",[]),
    {ok,Started} = application:ensure_all_started(cowboy),
    io:format(standard_error,"Started applications ~p~n",[Started]),
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {file,  "priv/static/index.html"}},
			{"/[...]", cowboy_static, {dir,  "priv/static",
				[{mimetypes, cow_mimetypes, all}]}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
    io:format(standard_error,"Started static_world webserver ~n",[]),
    X= {ok,_SupervisorPid} = static_world_sup:start_link(),
    io:format(standard_error,"Started supervisor~n",[]),
    X.

stop(_State) ->
	ok.
