-module(etap_t_003).
-export([start/0]).


start() ->
    ssl:start(),
    etap:plan(12),
    etap:diag("Common application tests."),
    etap_can:loaded_ok(logger, "module 'logger' loaded"),
    etap_can:loaded_ok(mijkcfg, "module 'mijkcfg' loaded"),
    etap_can:loaded_ok(obelisk_listener, "module 'obelisk_listener' loaded"),
    etap_can:loaded_ok(obelisk_commander, "module 'obelisk_commander' loaded"),
    etap_can:loaded_ok(obelisk_app, "module 'obelisk_app' loaded"),
    
    AppRet = application:start(obelisk),
    etap:is(AppRet, ok, "Application obelisk started"),
    case AppRet of
        {error, Reason}     -> io:format("Obelisk failed reason ~p ~n", [Reason]);
        ok                  -> void;
        Unknown             -> io:format("Obelisk return unknown value ~p ~n", [Unknown])
    end,
    
    %
    % test work port
    %
    {ok, ListenPort} = application:get_env(obelisk, listen_port),
    io:format("trying to connect to ~p port ~n", [ListenPort]),
    
    {ConnectStatus, ConnectRet}  = gen_tcp:connect({127,0,0,1},
            ListenPort, [{packet,2}]),
    io:format("listen port ~p ~n", [ListenPort]),
    etap:is(ConnectStatus, ok, string:concat("Connect to port ",
                                                    integer_to_list(ListenPort))),
    %mijkutils:sleep(2),
    case ConnectStatus of
        ok         -> 
                        gen_tcp:send(ConnectRet, <<"hello">>),
                        {_, _, EchoResponse} = receive M -> M end,
                        etap:is(EchoResponse, "hello", "echo response")
                        ;
        error      ->
                        io:format("Connect to port ~p failed: ~p ~n", [ListenPort, ConnectRet])
    end,
    
    %
    % test control port
    %
    {ok, ListenControlPort} = application:get_env(obelisk, control_port),
    io:format("trying to connect to control ~p port ~n", [ListenControlPort]),
    Opts = case application:get_env(obelisk, control_port_tls) of
        {ok, true} -> io:format("Use ssl ~n"),
                [{packet, 2}, {use_ssl, true}];
        _          -> io:format("Without ssl ~n"),
                [{packet, 2}, {use_ssl, false}]
    end,
    
    {ConnectControlStatus, ConnectControlRet} = mijktcp:connect({127,0,0,1}, ListenControlPort, Opts),
    io:format("listen control port ~p ~n", [ListenControlPort]),
    io:format("connect controlport ~p ~p ~n", [ConnectControlStatus, ConnectControlRet]),
    etap:is(ConnectControlStatus, ok, string:concat("Connect to port ",
                                            integer_to_list(ListenControlPort))),
    case ConnectControlStatus of
        ok         -> 
                        mijktcp:send(ConnectControlRet, <<"hello">>, Opts),
                        {_, _, EchoControlResponse} = receive Mc -> Mc end,
                        etap:is(EchoControlResponse, "hello", "echo response")
                        ;
        error      ->
                        io:format("Connect to port ~p failed: ~p ~n", [ListenControlPort, ConnectControlRet])
    end,
    
    {ConnectControlStatus2, ConnectControlRet2} = mijktcp:connect({127,0,0,1}, ListenControlPort, Opts),
    io:format("2 connect controlport ~p ~p ~n", [ConnectControlStatus, ConnectControlRet]),
    etap:is(ConnectControlStatus2, ok, string:concat("2 connect to port ",
                                            integer_to_list(ListenControlPort))),
    case ConnectControlStatus2 of
        ok         -> 
                        mijktcp:send(ConnectControlRet2, <<"hello2">>, Opts),
                        {_, _, EchoControlResponse2} = receive Mc2 -> Mc2 end,
                        etap:is(EchoControlResponse2, "hello2", "2 echo response")
                        ;
        error      ->
                        io:format("2 connect to port ~p failed: ~p ~n", [ListenControlPort, ConnectControlRet2])
    end,
    

    etap:end_tests().