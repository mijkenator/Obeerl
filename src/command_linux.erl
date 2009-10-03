-module(command_linux).
-author('mijkenator@gmail.com').

-export([ls/1, ll/1, pwd/1]).

ls(_Data)  -> exec_external_program(ls).
ll(_Data)  -> exec_external_program('ls -la').
pwd(_Data) -> exec_external_program(pwd).
    
    
    
exec_external_program(Program) ->
    Port = erlang:open_port({spawn, Program}, []),
    receive
        {Port, {data, Data}} -> {ok, [ erlang:list_to_atom(Str) ||
                                             Str <- string:tokens(Data, "\n") ]}
    end.