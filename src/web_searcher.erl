%%%-------------------------------------------------------------------
%%% File    : web_searcher.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : logger server
%%%-------------------------------------------------------------------
-module(web_searcher).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link(?MODULE, []).

    

init(_Args) ->
   {ok, {
            {one_for_one, 1, 60},
            [
             {wsw1, {ws_worker, start_link, []},
               permanent, 5000, worker, []},
             {wsw2, {ws_worker, start_link, []},
               permanent, 5000, worker, []}
            ]
        }
    }.



