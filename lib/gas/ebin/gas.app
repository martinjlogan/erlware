%%% -*- mode:erlang -*-
{application, gas,
 [
  % A quick description of the application.
  {description, "Basic services for a release"},

  % The version of the applicaton
  {vsn, "8.0.2"},

  % All modules used by the application.
  {modules,
   [
    gas,
    gas_sup,
    gas_transform,
    gas_override_config
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

  % The M F A to start this application.
  {mod, {gas, []}}
 ]
}.

