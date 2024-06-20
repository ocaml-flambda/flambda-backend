(* TEST
   flags="-extension layouts_beta";
*)

type r1 = { x1 : string; y1 : float#; }
type r2 = { x2 : string; y2 : float#; z2 : int }

type v =
  | A of float#
  | B of string * float#
  | C of string * float# * int
  | D of string

let r1           = { x1 = "x1"; y1 = #1.0 }
let create_r1 () = { x1 = "x1"; y1 = #1.0 }

let r2           = { x2 = "x2"; y2 = #2.0; z2 = 5 }
let create_r2 () = { x2 = "x2"; y2 = #2.0; z2 = 5 }

let a           = A #5.0
let create_a () = A #5.0

let vs           = [A #4.0; B ("B", #5.0); C ("C", #6.0, 6); D "hey"]
let create_vs () = [A #4.0; B ("B", #5.0); C ("C", #6.0, 6); D "hey"]

let () = print_endline "Testing pattern matching..."

let () =
  let { x1 = "x1"; y1 = #1.0 } = r1 in
  let { x1 = "x1"; y1 = #1.0 } = create_r1 () in
  let { x1 = "x1"; y1 = #1.0 } = Sys.opaque_identity r1 in
  let { x1 = "x1"; y1 = #1.0 } = Sys.opaque_identity (create_r1 ()) in
  let { x2 = "x2"; y2 = #2.0; z2 = 5 } = r2 in
  let { x2 = "x2"; y2 = #2.0; z2 = 5 } = create_r2 () in
  let { x2 = "x2"; y2 = #2.0; z2 = 5 } = Sys.opaque_identity r2 in
  let { x2 = "x2"; y2 = #2.0; z2 = 5 } = Sys.opaque_identity (create_r2 ()) in
  let A #5.0 = a in
  let A #5.0 = create_a () in
  let A #5.0 = Sys.opaque_identity a in
  let A #5.0 = Sys.opaque_identity (create_a ()) in
  let [A #4.0; B ("B", #5.0); C ("C", #6.0, 6); D "hey"] = vs in
  let [A #4.0; B ("B", #5.0); C ("C", #6.0, 6); D "hey"] = create_vs () in
  let [A #4.0; B ("B", #5.0); C ("C", #6.0, 6); D "hey"] =
    Sys.opaque_identity vs
  in
  let [A #4.0; B ("B", #5.0); C ("C", #6.0, 6); D "hey"] =
    Sys.opaque_identity (create_vs ())
  in
  ()
[@@warning "-partial-match"]
;;

let () = print_endline "Success!"

let () = print_endline "Testing optimization (this is a no-op in bytecode)..."

let () =
  match Sys.backend_type with
  | Bytecode -> ()
  | Native | Other _ ->
    (* Need to guard against:
       - Removal of duplicate immutable block allocations after inlining
       - Discovery of semantics of functions via result types
       Wrapping the closures themselves in [opaque_identity] does this.
    *)
    assert ((Sys.opaque_identity create_r1) ()
      == (Sys.opaque_identity create_r1 ()));
    assert ((Sys.opaque_identity create_r2) ()
      == (Sys.opaque_identity create_r2) ());
    assert ((Sys.opaque_identity create_a) ()
      == (Sys.opaque_identity create_a) ());
    assert ((Sys.opaque_identity create_vs) ()
      == (Sys.opaque_identity create_vs) ())
;;

let () = print_endline "Success!"
