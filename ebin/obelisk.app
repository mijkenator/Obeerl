{application, obelisk,
 [
  {description, "Obelisk command server"},
  {vsn, "1.0"},
  {id, "obelisk"},
  {modules,      [obelisk_listener, obelisk_listener2, obelisk_commander, mijkcfg,
                  logger, obelisk_control_listener2]},
  {registered,   [obelisk_listener2, tcp_server_sup, obelisk_listener2, obelisk_config,
                  obelisk_logger, obelisk_control_listener2]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {obelisk_app, []}},
  {env, [
        {config_file,  "/home/ashim/research/Git/Obeerl/t/test.cfg"},
        {ums_root,     "/var/lib/ums" },
        {listen_port,  2223},
        {listen_port_tls, false},
        {control_port, 2225},
        {control_port_tls, true}
        ]}
 ]
}.