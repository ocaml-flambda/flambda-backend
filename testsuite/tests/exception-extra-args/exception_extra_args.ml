(* TEST *)

(* Check that exception handler extra args are working properly *)

let [@inline never] print_endline x = print_endline x

let g () = failwith "foo"

let f () =
  let r = ref "foo" in
  let s = ref "bar" in
  try
    print_endline "foo";
    r := "foo2";
    s := "bar2";
    (g [@inlined never]) ()
  with exn -> (
    print_endline ((Printexc.to_string [@inlined never]) exn);
    print_endline !r;
    print_endline !s
  )

let () = f ()

