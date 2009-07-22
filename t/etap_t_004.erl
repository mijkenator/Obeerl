-module(etap_t_004).
-export([start/0]).

start() ->
    etap:plan(7),
    etap:diag("mijktcp tests."),
    
    Opts_without_flag = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    Opts_true = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false},
            {use_ssl, true},
            {depth, 2},
            {certfile,   "../cert.pem"}, 
            {keyfile,    "../key.pem"},
            {cacertfile, "../cacerts.pem"}
            ],
    Opts_false = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}, {use_ssl, false}],
    Opts_bad_arg = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}, {use_ssl, fase}],
    Opts_single = [ binary ],
    
    etap:is(mijktcp:is_ssl(Opts_without_flag), false, "is_ssl without flag"),
    etap:is(mijktcp:is_ssl(Opts_true), true, "is_ssl true"),
    etap:is(mijktcp:is_ssl(Opts_false), false, "is_ssl false"),
    etap:is(mijktcp:is_ssl(Opts_bad_arg), false, "is_ssl with badarg"),
    etap:is(mijktcp:is_ssl(Opts_single), false, "is_ssl single element array"),
    
    {RetCode, RetValue} = mijktcp:listen(4440, Opts_false),
    etap:is(RetCode, ok, "listen port without ssl"),
    
    ssl:start(),
    {RetCodeSSL, RetValueSSL} = mijktcp:listen(4441, Opts_true),
    etap:is(RetCodeSSL, ok, "listen port with ssl"),
    
    etap:end_tests().