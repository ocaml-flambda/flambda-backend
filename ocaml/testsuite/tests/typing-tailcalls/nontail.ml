(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids";
 ocamlopt.opt;
 {
   stack-allocation;
   compiler_reference2 = "${test_source_directory}/nontail.stack.reference";
   check-ocamlopt.opt-output;
 }{
   no-stack-allocation;
   compiler_reference2 = "${test_source_directory}/nontail.heap.reference";
   check-ocamlopt.opt-output;
 }
*)

(* All the `apply_mode`'s in the generated output are expected to be
   `apply_mode Nontail`, except for constructors and sequence
   "applications", which have `apply_mode Default`. *)

let [@inline never] f str = print_endline str

module M = struct
  let f = f
end

(* These calls should not be inferred as tail-calls because their callees
   are not defined in some ancestor let rec. *)
let foo () =
  f "hello";
  f "goodbye"

let bar () =
  f "hello";
  M.f "goodbye"

(* These should be nontail because they are not directly bound via a
   function argument. *)
type ('a, 'b) holds_fn = { g : ('a -> 'b) }

let pipe_struct_let a strct =
  let { g = g } = strct in g a

let pipe_tuple_let a tup =
  let (g, _) = tup in g a

let pipe_struct_match a strct =
  match strct with
  | { g = g } -> g a

let pipe_tuple_match a tup =
  match tup with
  | (g, _) -> g a

let pipe_option a opt =
  match opt with
  | Some g -> Some (g a)
  | None -> None

let pipe_option_ifnone a ifnone opt =
  match ifnone, opt with
  | ifnone, Some g -> Some (g a)
  | ifnone, None -> ifnone a


(* These should be nontail because their definitons were shadowed. *)
let map_shadowed opt ~g =
  match opt with
  | None -> None
  | Some x -> let g = f in Some (g x)

let map_shadowed = (fun opt ~g ->
  match opt with
  | None -> None
  | Some x -> let g = f in Some (g x))

let pipe_struct_shadowed a { g = g } = let g = f in g a

let pipe_tuple_shadowed a (g, _) = let g = f in g a

let pipe_struct_match_shadowed a = function
  | { g = g } -> let g = f in g a

let pipe_tuple_match_shadowed a = function
  | (g, _) -> let g = f in g a

let pipe_option_shadowed a = function
  | Some g -> let g = f in Some (g a)
  | None -> None

let pipe_option_ifnone_shadowed a = function
  | ifnone, Some g -> let g = f in Some (g a)
  | ifnone, None -> let ifnone = (fun a -> f "none"; None) in ifnone a
