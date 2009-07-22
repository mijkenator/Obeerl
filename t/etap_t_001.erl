-module(etap_t_001).
-export([start/0, rpcs_test/2]).

-record(tester, {xx, yy, zz}).

start() ->
    etap:plan(13),

    etap:diag("Test cfg module."),
    etap_can:loaded_ok(mijkcfg, "module 'mijkcfg' loaded"),
    
    %{ok,CfgPid} = mijkcfg:start_link("test.cfg"),
    CfgPid = case mijkcfg:start_link("test.cfg") of
        {ok,CPid}       -> io:format("~-15w Cfg process start return 'ok' ~n", ['INFO']),
                           CPid;
        {error, Error}  -> io:format("~-15w Cfg process start return 'error': ~p ~n", ['ERROR', Error]),
                           void;
        {ignore}        -> io:format("~-15w Cfg process start return 'ignore' ~n", ['INFO']),
                           void
    end,
    etap_process:is_pid(CfgPid, "Cfg process started"),
    etap_process:is_alive(CfgPid, "Cfg process is alive"),
    %etap:is(mijkcfg:start_link("test.cfg"),
    %    {error,{already_started,Pid}},
    %    "Duplicate start error"),
    
    %Answ = mijkcfg:lookup(loglevel),
    %io:format("~-15w ~p ~n", ['INFO', Answ]),
    etap:is(mijkcfg:lookup(loglevel), [{loglevel, "DEBUG"}], "Get loglevel from Config"),
    etap:is(mijkcfg:lookup(logopentype), [{logopentype, write}], "Get log open type from Config"),
    etap:is(mijkcfg:lookup(unknownoption), [], "Search unknown option in Config"),
    
    % request via server Pid
    etap:is(rpcs_test(CfgPid, loglevel), [{loglevel, "DEBUG"}], "Get loglevel from Config via Pid"),
    etap:is(rpcs_test(CfgPid, logopentype), [{logopentype, write}], "Get log open type from Config via Pid"),
    etap:is(rpcs_test(CfgPid, unknownoption), [], "Search unknown option in Config via Pid"),
    
    
    % request via gen_server call with Name
    etap:is(rpcs_test(mijkcfg, loglevel), [{loglevel, "DEBUG"}], "Get loglevel from Config via gen_server call with Name"),
    etap:is(rpcs_test(mijkcfg, logopentype), [{logopentype, write}], "Get log open type from Config via gen_server call with Name"),
    etap:is(rpcs_test(mijkcfg, unknownoption), [], "Search unknown option in Config via gen_server call with Name"),
    
    etap:is(gen_server:call(CfgPid, {test,unknown}), ok, "Send unknown request"),
    
    
    
    etap:end_tests().


rpcs_test(Pid, Key) ->
    gen_server:call(Pid, {search,Key}).
  

