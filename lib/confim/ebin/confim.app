%%% -*- mode:erlang -*-
{application, confim,
 [
  % A quick description of the application.
  {description, "OTP Configuration Improved"},

  % The version of the applicaton
  {vsn, "8.0.2"},

  % All modules used by the application.
  {modules,
   [
    confim,
    conf_app,
    conf_sup,
    conf_init,
    conf_tree,
    conf_override_config
   ]},

  % All of the registered names the application uses.
  {registered, []},

  % Applications that are to be started prior to this one.
  {applications,
   [
    kernel, 
    stdlib,
    sasl
   ]},

  % OTP application loader will load, but not start, included apps
  {included_applications, []},

  % configuration parameters
  {env, []},

  {mod, {conf_app, []}}
 ]
}.

