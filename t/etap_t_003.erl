-module(etap_t_003).
-export([start/0]).


start() ->
    etap:plan(10),
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
    
    {ConnectControlStatus, ConnectControlRet}  = gen_tcp:connect({127,0,0,1},
            ListenControlPort, [{packet,2}]),
    io:format("listen control port ~p ~n", [ListenControlPort]),
    etap:is(ConnectControlStatus, ok, string:concat("Connect to port ",
                                            integer_to_list(ListenControlPort))),
    case ConnectStatus of
        ok         -> 
                        gen_tcp:send(ConnectControlRet, <<"hello">>),
                        {_, _, EchoControlResponse} = receive Mc -> Mc end,
                        etap:is(EchoControlResponse, "hello", "echo response")
                        ;
        error      ->
                        io:format("Connect to port ~p failed: ~p ~n", [ListenControlPort, ConnectControlRet])
    end,
    

    etap:end_tests().