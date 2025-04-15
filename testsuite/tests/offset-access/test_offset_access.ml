(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }
*)

external box_float : float# -> (float[@local_opt]) = "%box_float"

external[@layout_poly] read_offset :
  'a ('b : any). 'a -> int64# -> 'b = "%unsafe_read_idx"

external[@layout_poly] write_offset :
  'a ('b : any). 'a -> int64# -> 'b -> unit = "%unsafe_write_idx"

type t = { s : string ; j : float# ; k : float# }

let () =
  let r = { s = "hi" ; j = #10.; k = #20. } in
  write_offset r #8L #(#200., #300.);
  let #( j, k ) : #( float# * float# ) = read_offset r #8L in
  Printf.printf "%f %f\n" (box_float j) (box_float k)
  (* prints 200.00 300.00 *)
