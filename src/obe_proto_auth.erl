-module(obe_proto_auth).
-author('mijkenator@gmail.com').

-export([login/1, login_step2/1, login_step3/1]).

login(_Data) ->
    {ok, "obe_proto_auth:login"}.
    
login_step2(_Data) ->
    {ok, "obe_proto_auth:login_step2"}.
    
login_step3(_Data) ->
    {ok, "obe_proto_auth:login_step3"}.