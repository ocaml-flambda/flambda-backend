let () =
  let open Printf in
  printf "With .mli: %s\n" (String_monoid.append "Hello " "world!");
  printf "Without .mli: %s\n" (String_monoid_no_mli.append "Hello " "world!");
  ()
