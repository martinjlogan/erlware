%%% -*- mode:erlang -*-
{application, fslib,
 [
  % A breif descrition of the application.
  {description, "A massive collection of various untilities written since 1999.  Some are junk, some are very useful. Cleaning would be good. Docs are prety good."},

  % The version number of the application.
  {vsn, "5.5.2"},

  % The modules that the application contains.
  {modules,
   [
	fs_boot_smithe,
	fs_db_init,
	fs_edge_trigger,
	fs_elwrap_h,
	fs_email_alert,
	fs_email,
	fs_event_tracker_h,
	fs_exit_handler,
	fs_exit_handler_h,
	fs_file,
	fs_filemon,
	fs_fixture_server,
	fs_fprof_util,
	fs_gen_tcp_recv,
	fs_lib,
	fs_lists,
	fs_message_bus,
	fs_message_bus_logger,
	fs_net,
	fs_process_data,
	fs_string,
	fs_syslog_h,
	fs_syslog_lib,
	fs_tcp_gateway,
	fs_test_lib,
	fs_time,
	fs_weighted_distribution
   ]},

  % All of the names that an application registers - avoids name clashes.
  {registered, []},

  % Applications that this one depends on.
  {applications,[kernel, stdlib, sasl, eunit]}
 ]
}.

