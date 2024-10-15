(* TEST
 flags = "-extension layouts_beta";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* CR layouts v7.1: merge with [layout_poly_native.ml] once products are out of
   beta *)

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
external[@layout_poly] opaque : ('a : any). 'a -> 'a = "%opaque"
external[@layout_poly] magic : ('a : any) ('b : any). 'a -> 'b = "%obj_magic"
external[@layout_poly] apply : ('a : any) ('b : any). ('a -> 'b) -> 'a -> 'b = "%apply"
external[@layout_poly] revapply : ('a : any) ('b : any). 'a -> ('a -> 'b) -> 'b = "%revapply"

module I64 = Stdlib_upstream_compatible.Int64_u
module F = Stdlib_upstream_compatible.Float_u

let print_product (#(i,f,s,b,i64) : #(int * F.t * string * bool * I64.t)) =
  Printf.printf "#(%d, %s, %s, %b, %s)\n"
    i (F.to_string f) s b (I64.to_string i64)

let () =
  print_product (id #(1, #2., "3", true, #5L))

let () =
  print_product (opaque #(1, #2., "3", true, #5L))

let () =
  print_product (magic #(1, #2., "3", true, #5L))

let () =
  print_product (apply (fun x -> x) #(1, #2., "3", true, #5L))

let () =
  print_product (revapply #(1, #2., "3", true, #5L) (fun x -> x))

let () =
  print_product (Sys.opaque_identity #(1, #2., "3", true, #5L))
