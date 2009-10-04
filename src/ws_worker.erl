-module(ws_worker).
-export([start_link/0]).

start_link() ->
    io:format("wsworker started ~n"),
    mijkutils:sleep(5),
    io:format("wsworker exited ~n"),
    ok.