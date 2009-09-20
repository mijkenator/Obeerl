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
    
    ExecRet = obelisk_proto:exec_command(ValidCommand),
    io:format("ExecRet ~p ~n", [ExecRet]),
    
    etap:end_tests().
    

    