(* TEST
   flags = "-extension layouts_beta";
   native;
*)

(* Unboxing of variants with mixed constructors *)

type t =
  | Const_t
  | Boxed of Float.t

type u =
  | Const_u
  | Unboxed of float#

external unbox : float -> float# = "%unbox_float"

let t_to_u (t : t) : u =
  match t with
  | Const_t -> Const_u
  | Boxed field -> Unboxed (unbox field)
;;

let f t =
  (* The result of [t_to_u] is speculatively unboxed *)
  let _ = t_to_u t in
  ()
;;
