let () =
  [%probe "name" (Printf.printf "default\n")];
  [%probe "name" ~enabled_at_init:true (Printf.printf "enabled at init\n")];
  [%probe "name" ~enabled_at_init:false (Printf.printf "not enabled at init\n")];
  ()
