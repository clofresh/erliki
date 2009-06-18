{application, erliki,
 [{description, "erliki"},
  {vsn, "0.01"},
  {modules, [
    erliki,
    erliki_app,
    erliki_sup,
    erliki_web,
    erliki_deps
  ]},
  {registered, []},
  {mod, {erliki_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
