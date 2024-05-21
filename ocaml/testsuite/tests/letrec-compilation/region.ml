(* TEST *)

(* Recursive values are not allowed to be stack-allocated, but their
   defining expressions are allowed to make allocations on the stack.
   This can introduce a region around the whole definition. *)

let rec f =
  let p = local_ (fun msg -> print_string msg) in
  p "hello, ";
  p "world!";
  print_newline ();
  fun x -> f x

(* Original bug report: unused function *)
let rec foo =
  let _f x = x, foo in
  function
  | None -> foo None
  | Some x -> x
