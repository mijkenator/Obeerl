-module(test_decode).
-export([start/0]).


start() ->
    S1 = [{error,'login first please'}],
    %R1 = {"error":[108,111,103,105,110,32,102,105,114,115,116,32,112,108,101,97,115,101]},
    S2 = [{ok,"Access granted"}],
    %R2 = {"ok":[65,99,99,101,115,115,32,103,114,97,110,116,101,100]},
    
    io:format("Test decode ~p ~n", [S1]),
    R1 = rfc4627:encode({obj, S1}),
    io:format("Test decode resp ~p ~n", [R1]),
    
    R2 = rfc4627:encode_noauto({obj, S1}),
    io:format("Test decode resp 2 ~p ~n", [R2]),
    R3 = xmerl_ucs:to_utf8(R2),
    io:format("Test decode resp 3 ~p ~n", [R3])
    .