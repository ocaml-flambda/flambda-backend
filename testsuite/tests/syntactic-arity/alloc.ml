<<<<<<< HEAD
(* TEST
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 run;
 check-program-output;
||||||| 121bedcfd2
(* TEST

flags = "-w +A-70"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** check-ocamlopt.byte-output
**** run
***** check-program-output
=======
(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







>>>>>>> 5.2.0
*)

type a = { mutable a : int }

let mutable_pat1 {a} b = a + b

let mutable_pat2 b {a} = b + a

let measure name f =
  let a = {a = 1} in
  let b = 2 in
  let before = Gc.minor_words () in
  let (_ : int) = f ~a ~b in
  let after = Gc.minor_words () in
  let alloc = int_of_float (after -. before) in
  match alloc with
  | 0 -> Printf.printf "%S doesn't allocate\n" name
  | _ -> Printf.printf "%S allocates\n" name

let () =
<<<<<<< HEAD
  measure "mutable_pat1" (fun ~a ~b -> mutable_pat1 a b);
  measure "mutable_pat2" (fun ~a ~b -> mutable_pat2 b a)
||||||| 121bedcfd2
  measure "noalloc" (fun ~a ~b -> noalloc b a);
  measure "alloc" (fun ~a ~b -> alloc a b)


let dont_warn_with_partial_match None x = x
=======
  measure "mutable_pat1" (fun ~a ~b -> mutable_pat1 a b);
  measure "mutable_pat2" (fun ~a ~b -> mutable_pat2 b a)

(* TEST
 flags = "-w +A-70";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 run;
 check-program-output;
*)
>>>>>>> 5.2.0
