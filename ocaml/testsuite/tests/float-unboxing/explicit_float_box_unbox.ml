(* TEST 
 native;
*)

[@@@ocaml.flambda_o3]

type t = float
type u = float#

external globalize : (t[@local_opt]) -> t = "%obj_dup"

let magic_globalize =
  let zero = Sys.opaque_identity ~-.0. in
  fun (local_ t) -> globalize t +. zero
;;

let[@inline] boxed_f t =
  if t > 0.0
  then (
    let t' = ceil t in
    if t' <= 1.0
    then Int.of_float t'
    else invalid_arg (Printf.sprintf "argument (%f) is too large" (magic_globalize t)))
  else if t >= -1.0
  then Int.of_float t
  else invalid_arg (Printf.sprintf "argument (%f) is too small" (magic_globalize t))
;;

external ceil : u -> u = "caml_ceil_float" "ceil" [@@unboxed] [@@noalloc]
external box : (u[@unboxed]) -> (t[@local_opt]) = "%box_float"
external unbox : (t[@local_opt]) -> (u[@unboxed]) = "%unbox_float"

let[@inline] unboxed_f u =
  if (box u) > 0.0
  then (
    let u' = ceil u in
    if (box u') <= 1.0
    then Int.of_float (box u')
    else invalid_arg (Printf.sprintf "argument (%f) is too large" (box u)))
  else if (box u) >= -1.0
  then Int.of_float (box u)
  else invalid_arg (Printf.sprintf "argument (%f) is too small" (box u))
;;

let xs = Sys.opaque_identity [| 0. |]

let old_g () = boxed_f xs.(0)
let new_g () = unboxed_f (unbox xs.(0))

let check_alloc _name f ~expect =
  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  let _x = (f[@inlined never]) () in
  let a2 = Gc.allocated_bytes () in
  let alloc = (a2 -. 2. *. a1 +. a0) in

  match Sys.backend_type with
  | Sys.Bytecode -> ()
  | Sys.Native -> assert (alloc = expect)
  | _ -> assert false

let () = 
  check_alloc "fail w/ magic globalize" old_g ~expect:0.;
  check_alloc "fail w/ explicit box/unbox" new_g ~expect:16.
