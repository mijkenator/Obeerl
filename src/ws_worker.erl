-module(ws_worker).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-behaviour(gen_server).

-record(worker_state, {
        workername,
        wokrnumber,
        configitem}).

start_link(WorkerName) ->
    io:format("wsworker started ~p ~n", [WorkerName]),
    gen_server:start_link({local, WorkerName}, ?MODULE, #worker_state{workername = WorkerName}, []).
    
init(_Args) ->
  %gen_server:cast(self(), {accepted, self()}),
  {ok, 'worker inited'}.
  
handle_cast({accepted, _Pid}, State=#worker_state{}) ->
    io:format("cast !!!! ~n"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}. 
  
% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
