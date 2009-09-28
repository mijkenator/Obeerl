-module(mijkutils).

-export([ sleep/1, fold_file_lines/3, countlines/1, readlines/1, get_last_line/1,
          str_like/2 ]).

-export([ get_obelisk_socket_options/2 ]).

sleep(T) when is_integer(T), T > 0 ->
    receive
    after T * 1000 -> true
    end.
    
int_fold_lines(Device, Kons, Result) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Result;
        Line -> NewResult = Kons(Line, Result),
                int_fold_lines(Device, Kons, NewResult)
    end.

fold_file_lines(FileName, Kons, Knil) ->
    {ok, Device} = file:open(FileName,[read]),
    int_fold_lines(Device, Kons, Knil).
    
countlines(FileName) ->
  fold_file_lines(FileName, fun(_L,R) -> R + 1 end, 0).

readlines(FileName) ->
    lists:reverse(fold_file_lines(FileName, fun(L,R) -> [L|R] end, [])).

get_last_line(FileName) ->
    fold_file_lines(FileName, fun(L,_R) -> L end, 0).
    
str_like(String, RegExp) ->
    case regexp:match(String, RegExp) of
        {match,_,_} -> true;
        _ -> false
    end.
    
get_obelisk_socket_options(AppName, PortName) ->
    case application:get_env(AppName, PortName) of
        {ok, true} ->
            [binary, {packet, 0}, {reuseaddr, true}, {keepalive, true},
            {backlog, 30}, {active, false}, {use_ssl, true},
            {depth, 2},
            {certfile,   "../server-cert.pem"}, 
            {keyfile,    "../server-key.pem"},
            {cacertfile, "../cacert.pem"}];
        _          ->
            [binary, {packet, 0}, {active, false}, {use_ssl, false}]
    end.