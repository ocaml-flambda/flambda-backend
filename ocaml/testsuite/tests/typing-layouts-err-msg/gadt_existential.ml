(* TEST
   flags = "-extension layouts_alpha"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

type ('a: void) t = 'a
let f x: 'a t = x
type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
let f = function Dyn (type a) (w, x : a ty * a) -> ignore (f x)
