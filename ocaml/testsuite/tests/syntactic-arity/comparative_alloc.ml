(* TEST
 * setup-ocamlopt.byte-build-env
 ** ocamlopt.byte
 *** run
 **** check-program-output
*)

(* Check that the runtime arity of a function (i.e., its 'fast path' for
   runtime application) matches its syntactic arity (i.e., the number
   of arguments appearing directly following [fun]).
*)

let is_zero_alloc f =
  let before = Gc.minor_words () in
  f ();
  let after = Gc.minor_words () in
  int_of_float (after -. before) = 0

let run ~name f =
  let x, y, z = 1, 2, 3 in
  let arity =
    if is_zero_alloc (fun () ->
      let f = Sys.opaque_identity (f x) in
      let f = Sys.opaque_identity (f y) in
      f z)
    then "1-ary fun returning 1-ary fun returning 1-ary fun"
    else if is_zero_alloc (fun () ->
      let f = Sys.opaque_identity (f x y) in
      f z)
    then "2-ary fun returning 1-ary fun"
    else if is_zero_alloc (fun () ->
        let f = Sys.opaque_identity (f x) in
        f y z)
    then "1-ary fun returning 2-ary fun"
    else if is_zero_alloc (fun () -> f x y z)
    then "3-ary fun"
    else "unknown arity"
  in
  Printf.printf "%s: %s\n" name arity

let () =
  print_endline "Key:";
  print_endline "  <function description>: <function arity>";
  print_newline ();
  run (fun _ _ _ -> ()) ~name:"3 params";
  run (fun _ _ -> fun _ -> ()) ~name:"2 params then 1 param";
  run (fun _ -> fun _ _ -> ()) ~name:"1 param then 2 params";
  run (fun _ -> fun _ -> fun _ -> ())
    ~name:"1 param, then 1 param, then 1 param";
  run (fun _ -> let g _ _ = () in g)
    ~name:"1 param then let-bound 2 params";
  run (fun _ _ -> let g _ = () in g)
    ~name:"2 params then let-bound 1 param";
  run (fun _ -> let g _ = let h _ = () in h in g)
    ~name:"1 param, then let-bound 1 param, then let-bound 1 param";
;;
