-module(etap_t_006).
-export([start/0]).

start() ->
    etap:plan(1),
    etap:diag("Obelisk login/logout tests."),
    
    ValidCommand = "{\"type\":\"command\",\"name\":\"login\",\"data\":{\"login\":\"xxxx\",
        \"password\":\"xxxxx\"},\"multi\":\"val1\",\"multi\":\"val2\"}",
    ValidCommand2 = "{\"type\":\"command\",\"name\":\"logincheg\",\"data\":{\"login\":\"xxxx\",
        \"password\":\"xxxxx\"},\"multi\":\"val1\",\"multi\":\"val2\"}",
    InValidCommand = "{\"type\":\"command\",\"name\":,\"data\":{\"login\":\"xxxx\",\"password\":\"xxxxx\"}}",
    
    ssl:start(),
    AppRet = application:start(obelisk),
    etap:is(AppRet, ok, "Application obelisk started"),
    case AppRet of
        {error, Reason}     -> io:format("Obelisk failed reason ~p ~n", [Reason]);
        ok                  -> void;
        Unknown             -> io:format("Obelisk return unknown value ~p ~n", [Unknown])
    end,
    {ok, ListenControlPort} = application:get_env(obelisk, control_port),
    io:format("trying to connect to control ~p port ~n", [ListenControlPort]),
    Opts = case application:get_env(obelisk, control_port_tls) of
        {ok, true} -> io:format("Use ssl ~n"),
                [{packet, 2}, {use_ssl, true}];
        _          -> io:format("Without ssl ~n"),
                [{packet, 2}, {use_ssl, false}]
    end,
    {ConnectControlStatus, ConnectControlRet} = mijktcp:connect({127,0,0,1}, ListenControlPort, Opts),
    case ConnectControlStatus of
        ok         -> 
            mijktcp:send(ConnectControlRet, ValidCommand2, Opts),
            {_, _, EchoControlResponse} = receive Mc -> Mc end,
            io:format("Obelisk resp1: ~p ~n", [EchoControlResponse]);
            %etap:is(EchoControlResponse, "hello", "echo response");
        error      ->
            io:format("Connect to port ~p failed: ~p ~n", [ListenControlPort, ConnectControlRet])
    end,
    
    
    etap:end_tests().    