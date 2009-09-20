-module(obe_proto_auth).
-author('mijkenator@gmail.com').

-export([login/1, login_step2/1, login_step3/1]).

login(Data) ->
    {ok, "obe_proto_auth:login"}.
    
login_step2(Data) ->
    {ok, "obe_proto_auth:login_step2"}.
    
login_step3(Data) ->
    {ok, "obe_proto_auth:login_step3"}.