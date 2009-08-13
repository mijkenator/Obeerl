-module(obelisk_app).
-author('mijkenator@gmail.com').

-behaviour(application).

%% Internal API
-export([start_client/1]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).
-define(DEF_C_PORT,  2225).
-define(DEF_CONF,    "/etc/obelisk.cfg").


%%-define(DEF_CONF,    "/home/ashim/research/obeerl_otp/t/test.cfg").

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(Opts) ->
    supervisor:start_child(obelisk_com_sup, [Opts]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    ListenPort = get_app_env(listen_port, get_app_env(listen_port, ?DEF_PORT)),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, obelisk_commander]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   tcp_server_sup,                          % Id       = internal id
                  {obelisk_listener,
                        start_link,[Port,Module,
                        listener1, listen_port_tls,
                        {obelisk_listener, loop_func}]},   % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [obelisk_listener]                       % Modules  = [Module] | dynamic
              },
              {   tcp_control_server_sup,                  % Id       = internal id
                  {obelisk_listener,
                        start_link,
                        [get_app_env(control_port,
                            ?DEF_C_PORT),Module,
                        listener2, control_port_tls,
                        {obelisk_listener, loop_func}]},   % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [obelisk_listener]                       % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   obelisk_com_sup,
                  {supervisor,start_link,[{local, obelisk_com_sup}, ?MODULE, [Module]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              },
              % Config storage
              {
                  obelisk_config,
                  {mijkcfg,start_link,[get_app_env(config_file, ?DEF_CONF)]},
                  permanent,
                  2000,
                  worker,
                  [mijkcfg]
              },
              % Obelisk logger
              {
                  obelisk_logger,
                  {logger,start_link,[mijkcfg]},
                  permanent,
                  2000,
                  worker,
                  [logger]
              }
            ]
        }
    };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    %case application:get_env(application:get_application(self()), Opt) of
    case application:get_env(obelisk, Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.