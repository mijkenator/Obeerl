-module(etap_t_002).
-export([start/0]).


start() ->
    etap:plan(19),
    etap:diag("Test logger module."),
    etap_can:loaded_ok(logger, "module 'logger' loaded"),
    
    CfgPid = case mijkcfg:start_link("test.cfg") of
        {ok,CPid}       -> io:format("~-15w Cfg process start return 'ok' ~n", ['INFO']),
                           CPid;
        {error, CError}  -> io:format("~-15w Cfg process start return 'error': ~p ~n", ['ERROR', CError]),
                           void;
        {ignore}        -> io:format("~-15w Cfg process start return 'ignore' ~n", ['INFO']),
                           void
    end,
    
    LogPid = case logger:start_link(CfgPid) of
        {ok,LPid}       -> io:format("~-15w Log process start return 'ok' ~n", ['INFO']),
                           LPid;
        {error, LError}  -> io:format("~-15w Log process start return 'error': ~p ~n", ['ERROR', LError]),
                           void;
        {ignore}        -> io:format("~-15w Log process start return 'ignore' ~n", ['INFO']),
                           void
    end,
    
    
    gen_server:call(LogPid, {debug, "test debug"}),
    % last string is 'DEBUG' ....... "test debug", string count is 1
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 1, "First debug line"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'DEBUG'.+\"test debug\"\s*$"), true, "Correct last line"),
            
    gen_server:call(LogPid, {warn, "test warn"}),
    % last string is 'WARNING' ....... "test warn", string count is 2
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 2, "Two warn line"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'WARNING'.+\"test warn\"\s*$"), true, "Correct last line"),
            
    gen_server:call(LogPid, {error, "test error"}),
    % last string is 'ERROR' ....... "test error", string count is 3
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 3, "Third error line"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'ERROR'.+\"test error\"\s*$"), true, "Correct last line"),
            
    gen_server:call(LogPid, {warsn, "test wrong type"}),
    % last string is 'ERROR' ....... "test error", string count is 3
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 3, "Third error line (bad loglevel, no output)"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'ERROR'.+\"test error\"\s*$"), true, "Correct last line"),
    
    
    %change log level to WARNING
    gen_server:call(LogPid, {change_log_level, "WARNING"}),
    gen_server:call(LogPid, {debug, "test debug"}),
    % last string is 'ERROR' ....... "test error", string count is 3
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 3, "No debug output while WARN loglevel is set"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'ERROR'.+\"test error\"\s*$"), true, "Correct last line"),
    
    gen_server:call(LogPid, {warn, "test warn"}),
    % last string is 'WARNING' ....... "test warn", string count is 4
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 4, "Correct warn output"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'WARNING'.+\"test warn\"\s*$"), true, "Correct last line"),
            
    gen_server:call(LogPid, {error, "test error"}),
    % last string is 'ERROR' ....... "test error", string count is 5
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 5, "Correct error output"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'ERROR'.+\"test error\"\s*$"), true, "Correct last line"),
    
    
    %change log level to ERROR
    gen_server:call(LogPid, {change_log_level, "ERROR"}),
    gen_server:call(LogPid, {debug, "test debug"}),
    % last string is 'ERROR' ....... "test error", string count is 5
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 5, "No debug output while ERROR loglevel is set"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'ERROR'.+\"test error\"\s*$"), true, "Correct last line"),
    
    gen_server:call(LogPid, {warn, "test warn"}),
    % last string is 'ERROR' ....... "test error", string count is 5
    % mijkutils:sleep(1),
    etap:is(mijkutils:countlines("test.log"), 5, "No warn output while ERROR loglevel is set"),
    etap:is(mijkutils:str_like(mijkutils:get_last_line("test.log"),
            "^'ERROR'.+\"test error\"\s*$"), true, "Correct last line"),
    
    
    etap:end_tests().




