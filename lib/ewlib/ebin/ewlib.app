{application, ewlib,
 [{description, "Erlware support library"},
  {vsn, "0.7.4.0"},
  {modules, [ewl_talk, ewl_elwrap_h, ewl_get_opt, ewl_string_manip, ewl_file, ewl_config_diff]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, eunit]}]}.
