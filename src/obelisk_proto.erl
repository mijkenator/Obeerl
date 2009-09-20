-module(obelisk_proto).
-author('mijkenator@gmail.com').

-export([is_command/1, get_value_by_name/2,
        get_all_values_by_name/2,
        exec_command/1, exec_command_wc/1
        known_commands/0]).


known_commands() ->
%
% [{command_name, module, function}]
%
    [
        {"login", "obe_proto_auth", "login"},
        {"login1", "auth", "login"},
        {"login2", "auth", "login"},
        {"login", "obe_proto_auth", "login_step2"},
        {"login", "obe_proto_auth", "login_step3"}
    ].


exec_command(String) ->
    case is_command(String) of
        {ok, Command}   ->
            exec_command_wc(Command);
        {error, Reason} ->
            {error, Reason}
    end.

exec_command_wc(Command) ->
    {ok, CommandName} = get_value_by_name("name",Command),
    [ apply(list_to_atom(Module), list_to_atom(Function), [Command]) ||
                {Name, Module, Function} <- known_commands(), Name =:= CommandName ].

is_command(String) ->
    case rfc4627:decode(String) of
        {ok, {obj, JSStruct}, _Hren} ->
            {ok, JSStruct};
        {error, Reason}              ->
            {error, Reason}
    end.

get_value_by_name(Name,[]) ->
    {error, "not found"};
get_value_by_name(Name,[H|T]) ->
    case H of
        {Name1, Value} when Name1 =:= Name ->
            {ok, binary_to_list(Value)};
        _ ->
            get_value_by_name(Name, T)
    end.
    
get_all_values_by_name(Name,JSStruct) ->
    [ binary_to_list(X) || {Name1, X} <- JSStruct, Name1 =:= Name ].