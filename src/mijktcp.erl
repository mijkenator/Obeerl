-module(mijktcp).
-author('mijkenator@gmail.com').


-export([listen/2, is_ssl/1, clear_opts/1, controlling_process/3,
    close/2, connect/3, connect/4, send/3, accept/2, recv/3, recv/4,
    setopts/3, peername/2 ]).

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

connect(Address, Port, Options) ->  connect(Address, Port, Options, 10000).
connect(Address, Port, Options, Timeout) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:connect(Address, Port, mijktcp:clear_opts(Options), Timeout);
        false ->
            gen_tcp:connect(Address, Port, mijktcp:clear_opts(Options), Timeout)
    end.

send(Socket, Data, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:send(Socket, Data);
        false ->
            gen_tcp:send(Socket, Data)
    end.
    
accept(Socket, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:transport_accept(Socket);
        false ->
            gen_tcp:accept(Socket)
    end.

recv(Socket, Length, Options) ->
    recv(Socket, Length, infinity, Options).
recv(Socket, Length, Timeout, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:recv(Socket, Length, Timeout);
        false ->
            gen_tcp:recv(Socket, Length, Timeout)
    end.
    
setopts(Socket, Opts, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:setopts(Socket, Opts);
        false ->
            inet:setopts(Socket, Opts)
    end.

peername(Socket, Options) ->
    case mijktcp:is_ssl(Options) of
        true  ->
            ssl:peername(Socket);
        false ->
            inet:peername(Socket)
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
    

