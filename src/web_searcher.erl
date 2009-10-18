%%%-------------------------------------------------------------------
%%% File    : web_searcher.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(web_searcher).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, start_worker/0, start_worker/1]).


start_link() ->
    supervisor:start_link(?MODULE, []).

    

init(_Args) ->
    io:format("supervisor init ~p ~n", [self()]),
   {ok, {
            {one_for_one, 1, 60},
            [
             {wsw1, {ws_worker, start_link, ['Worker1']},
               permanent, 5000, worker, []},
             {wsw2, {ws_worker, start_link, ['Worker2']},
               permanent, 5000, worker, []}
            ]
        }
    }.
    
start_worker() -> start_worker(self()).
start_worker(Pid) ->
    MyPid = self(),
    io:format("supervisor start worker ~p ~n", [MyPid]),
    supervisor:start_child(Pid,
        {wsw0, {ws_worker, start_link, ['Worker0']},
            permanent, 5000, worker, []}).


