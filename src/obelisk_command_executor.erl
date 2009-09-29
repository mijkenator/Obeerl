-module(obelisk_command_executor).
-author('mijkenator@gmail.com').

-export([is_command/1]).
-export([exec_command/2]).

%
% {type:command, name:command_name, data:{ command specific fields }}
%

%
%{"type":"command", "name":"login", "data":{"login":"xxxx", "password":"xxxxx"}}
%{type:\"command\", name:\"login\", data:{login:\"xxxx\", password:\"xxxxx\"}}
%{type:'command',name:'login',data:{login:'xxxx',password:'xxxxx'}}
%

is_command(JSONString) ->
    io:format("Input string ~p ~n", [JSONString]),
    ErlObj = rfc4627:unicode_decode(JSONString),
    io:format("Erlang data(rfc4627) ~p ~n", [ErlObj]),
    ErlObj1 = mochijson2:decode(JSONString),
    io:format("Erlang data(mochjson2) ~p ~n", [ErlObj1])
    .
    
    
exec_command(Command, LoginFlag) when LoginFlag =:= login ->
    case obelisk_proto:get_value_by_name("name",Command) of
        {ok, "logout"} ->
            {ok, nologin, [{ok, 'logged off'}]};
        _ ->
            {ok, login, obelisk_proto:exec_command_wc(Command)}
    end;
exec_command(Command, LoginFlag) when LoginFlag =:= nologin ->
    case obelisk_proto:get_value_by_name("name",Command) of
        {ok, "login"} ->
            case obelisk_proto:exec_command_wc(Command) of
                [{ok,_}|_]    -> {ok, login, [{ok, 'Access granted'}]};
                Fail          -> {ok, nologin, [{error, Fail}]}
            end;
        _ ->
            {ok, nologin, [{error, 'login first please'}]}
    end.