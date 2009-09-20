-module(etap_t_005).
-export([start/0]).

start() ->
    etap:plan(7),
    etap:diag("JSON parser tests."),
    
    %Str1 = "{\"type\":\"command\",\"name\":\"login\",\"data\":{\"login\":\"xxxx\",\"password\":\"xxxxx\"}}",
    %Str2 = "{\"type\":\"command\",\"name\":\"login\",\"data\":{\"login\":\"лялял\",\"password\":\"жужу\"}}",
    %io:format("Str1 ~p ~n", [Str1]),
    %io:format("Str2 ~p ~n", [Str2]),
    %io:format("rfc4627 unicode_decode ~p ~n", [rfc4627:unicode_decode(Str1)]),
    %io:format("rfc4627 decode ~p ~n", [rfc4627:decode(Str1)]),
    %io:format("mochijson2 decode ~p ~n", [mochijson2:decode(Str1)]),
    %io:format("rfc4627 decode str2 ~p ~n", [rfc4627:decode(Str2)]),
    %io:format("mochijson2 decode str2 ~p ~n", [mochijson2:decode(Str2)]),
    
    ValidCommand = "{\"type\":\"command\",\"name\":\"login\",\"data\":{\"login\":\"xxxx\",
        \"password\":\"xxxxx\"},\"multi\":\"val1\",\"multi\":\"val2\"}",
    ValidCommand2 = "{\"type\":\"command\",\"name\":\"logincheg\",\"data\":{\"login\":\"xxxx\",
        \"password\":\"xxxxx\"},\"multi\":\"val1\",\"multi\":\"val2\"}",
    InValidCommand = "{\"type\":\"command\",\"name\":,\"data\":{\"login\":\"xxxx\",\"password\":\"xxxxx\"}}",
    
    
    {ValidState, ValidRet} = obelisk_proto:is_command(ValidCommand),
    {InValidState, InValidRet} = obelisk_proto:is_command(InValidCommand),
    io:format("ValidRet ~p ~n", [ValidRet]),
    etap:is(ValidState, ok, "Check valid command"),
    etap:is(InValidState, error, "Check invalid command"),
    
    etap:is(obelisk_proto:get_value_by_name("type", ValidRet), {ok,"command"}, "find command type"),
    etap:is(obelisk_proto:get_value_by_name("name", ValidRet), {ok,"login"},   "find command name"),
    
    etap:is(obelisk_proto:get_all_values_by_name("type", ValidRet), ["command"], "find 2 command type"),
    etap:is(obelisk_proto:get_all_values_by_name("multi",ValidRet), ["val1","val2"], "find all 'multi' values"),
    etap:is(obelisk_proto:get_all_values_by_name("nonex",ValidRet), [], "find values of not existing attr"),
    
    ExecRet2 = obelisk_proto:exec_command(ValidCommand2),
    io:format("ExecRet2 ~p ~n", [ExecRet2]),
    
    ExecRet = obelisk_proto:exec_command(ValidCommand),
    io:format("ExecRet ~p ~n", [ExecRet]),
    
    ExecRetIC = obelisk_proto:exec_command(InValidCommand),
    io:format("ExecRetIC ~p ~n", [ExecRetIC]),
    
    io:format("E1~n"),
    T_one = rfc4627:encode({obj, [{one, two}]}),
    io:format("E2~n"),
    T_two = rfc4627:encode({obj, ExecRet}),
    io:format("Encode exam ~p ~n", [ T_two ]),
    
    TestList = [111,98,101,95,112,114,111,116,111,95,97,117,116,104,58,108,111,103,105,110],
    io:format("List 1 ~p ~n", [ TestList ]),
    
    etap:end_tests().
    

    