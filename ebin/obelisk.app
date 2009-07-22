{application, obelisk,
 [
  {description, "Obelisk command server"},
  {vsn, "1.0"},
  {id, "obelisk"},
  {modules,      [obelisk_listener, obelisk_commander, mijkcfg, logger]},
  {registered,   [tcp_server_sup, obelisk_listener, obelisk_config, obelisk_logger]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {obelisk_app, []}},
  {env, [
        {config_file,  "/home/ashim/research/obeerl_otp/t/test.cfg"},
        {ums_root,     "/var/lib/ums" },
        {listen_port,  2223},
        {listen_port_tls, true},
        {control_port, 2225},
        {control_port_tls, true}
        ]}
 ]
}.