{application, ewlib,
 [{description, "Erlware support library"},
  {vsn, "0.7.5.0"},
  {modules, [ewl_talk, ewl_elwrap_h, ewl_get_opt, ewl_string_manip, ewl_file, ewl_config_diff, ewl_sinan_paths]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, eunit]}]}.
