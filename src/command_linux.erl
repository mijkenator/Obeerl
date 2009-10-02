-module(command_linux).
-author('mijkenator@gmail.com').

-export([ls/0, ls/1]).

ls(Data) -> ls().
ls() ->
    Port = erlang:open_port({spawn,ls}, []),
    receive
        {Port, {data, Data}} -> {ok, Data}
    end.