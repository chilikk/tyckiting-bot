{application, space_application,
 [{description,  "Space tyckiting client"},
  {vsn,          "0.1"},
  {id,           "space"},
  {modules,      [space,
                  space_application,
                  space_ai]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {space_application, []}},
  {env, [{current_ai,   space_ai},
         {host_address, "ws://127.0.0.1:3000"},
         {team_name,    <<"RamDenys">>}]
  }]}.
