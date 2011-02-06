{application, ewlib,
 [{description, "Erlware support library"},
  {vsn, "0.9.7.3"},
  {modules, [
	     ewl_talk,
	     ewl_elwrap_h,
	     ewl_get_opt,
	     ewl_string_manip,
	     ewl_file,
	     ewl_config_diff,
	     ewl_installed_paths,
	     ewl_package_paths,
	     ewl_sinan_paths,
             ewl_plists
	    ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, eunit]}]}.
