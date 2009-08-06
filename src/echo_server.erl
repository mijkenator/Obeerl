-module(echo_server).
-author('Jesse E.I. Farmer <jesse@20bits.com>').

-export([start/0, loop/2]).

% echo_server specific code
start() ->
    socket_server:start(?MODULE, 7000, {?MODULE, loop}).
    
loop(Socket, Opts) ->
    case mijktcp:recv(Socket, 0, Opts) of
        {ok, Data} ->
            mijktcp:send(Socket, Data, Opts),
            loop(Socket, Opts);
        {error, closed} ->
            ok
    end.