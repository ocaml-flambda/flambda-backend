let () =
  [%probe "a" (Printf.printf "default\n")];
  [%probe "b" ~enabled_at_init:true (Printf.printf "enabled at init\n")];
  [%probe "c" ~enabled_at_init:false (Printf.printf "not enabled at init\n")];
  ()
