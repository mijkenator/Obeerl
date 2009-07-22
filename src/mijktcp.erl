-module(mijktcp).
-author('mijkenator@gmail.com').


-export([listen/2, is_ssl/1, clear_opts/1, controlling_process/3, close/2]).

%
% Options = [listen_option()] ++ {use_ssl, true | false}
%
listen(Port, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:listen(Port, mijktcp:clear_opts(Options));
        false ->
            gen_tcp:listen(Port, mijktcp:clear_opts(Options))
    end.

controlling_process(Socket, NewOwner, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:controlling_process(Socket, NewOwner);
        false ->
            gen_tcp:controlling_process(Socket, NewOwner)
    end.

close(Socket, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:close(Socket);
        false ->
            gen_tcp:close(Socket)
    end.

%
% check Options array for {use_ssl, true | false}
% return true | false
%

is_ssl([]) -> false;
is_ssl([H|T]) ->
    case H of
        {use_ssl, true}   -> true;
        {use_ssl, _Param} -> false;
        _ -> is_ssl(T)
    end.    

%
% auxiliary function for clear_opts
%

is_ssl_tuple(T) ->
    case T of
        {use_ssl, _}   -> true;
        _              -> false
    end.  
%
% remove {use_ssl, true | false} tuple from Options array
% return Options array
%

clear_opts(H) -> [X || X <- H , is_ssl_tuple(X) =:= false].
    

