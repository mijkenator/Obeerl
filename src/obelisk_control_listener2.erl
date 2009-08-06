-module(obelisk_control_listener2).
-author('mijkenator@gmail.com').

-behaviour(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/3]).
-export([start_link/2]).
-export([loop_func/2, call_loop/3]).

-record(server_state, {
        port,
        loop,
        ip=any,
        socket_o,        % socket options array
        module_o,
        lsocket=null}).

start(Name, Port, _Loop) ->
    State = #server_state{port = Port, loop = loop_func},
    gen_server:start_link({local, Name}, ?MODULE, State, []).
    
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    State = #server_state{port = Port, loop = loop_func, module_o = Module},
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []). 
    

init(State = #server_state{port=Port}) ->
    Opts = case application:get_env(obelisk, control_port_tls) of
        {ok, true} ->
            [binary, {packet, 2}, {reuseaddr, true}, {keepalive, true},
            {backlog, 30}, {active, false}, {use_ssl, true},
            {depth, 2},
            {certfile,   "../server-cert.pem"}, 
            {keyfile,    "../server-key.pem"},
            {cacertfile, "../cacert.pem"}];
        _          ->
            [binary, {packet, 2}, {reuseaddr, true}, {keepalive, true},
            {backlog, 30}, {active, false}]
    end,
    case mijktcp:listen(Port, Opts) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket, socket_o = Opts},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)}.

accept_loop({Server, LSocket, Loop, Opts}) ->
    {ok, Socket} = mijktcp:accept(LSocket, Opts),
    % Let the server spawn a new process and replace this loop
    % with the echo loop, to avoid blocking
    case mijktcp:is_ssl(Opts) of
        true  ->
            ssl:ssl_accept(Socket)
    end,
    gen_server:cast(Server, {accepted, self()}),
    call_loop(Loop, Socket, Opts).
    
% To be more robust we should be using spawn_link and trapping exits
accept(State = #server_state{lsocket=LSocket, loop = Loop, socket_o = Opts}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop, Opts}]),
    State.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


loop_func(Socket, Opts) ->
    logger:log({debug, "loop func"}),
    case mijktcp:recv(Socket, 0, Opts) of
        {ok, Data} ->
            logger:log({debug, string:concat("recv data: ", binary_to_list(Data))}),
            mijktcp:send(Socket, Data, Opts),
            loop_func(Socket, Opts);
        {error, closed} ->
            ok
    end.

call_loop({M, F}, Socket, Opts) ->
    M:F(Socket, Opts);
call_loop(Loop, Socket, Opts) ->
    ?MODULE:Loop(Socket, Opts).
    