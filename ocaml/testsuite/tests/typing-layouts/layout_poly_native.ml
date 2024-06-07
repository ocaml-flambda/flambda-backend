(* TEST
 flags = "-extension layouts";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)


external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
external[@layout_poly] opaque : ('a : any). 'a -> 'a = "%opaque"
external[@layout_poly] magic : ('a : any) ('b : any). 'a -> 'b = "%obj_magic"
external[@layout_poly] apply : ('a : any) ('b : any). ('a -> 'b) -> 'a -> 'b = "%apply"
external[@layout_poly] revapply : ('a : any) ('b : any). 'a -> ('a -> 'b) -> 'b = "%revapply"

type t_any : any
module N = Stdlib_upstream_compatible.Nativeint_u
module I32 = Stdlib_upstream_compatible.Int32_u
module I64 = Stdlib_upstream_compatible.Int64_u
module F = Stdlib_upstream_compatible.Float_u
let () =
   (* id (assert false : t_any); *)
  Printf.printf "%s\n" (N.to_string (id #1n));
  Printf.printf "%s\n" (I32.to_string (id #2l));
  Printf.printf "%s\n" (I64.to_string (id #3L));
  Printf.printf "%s\n" (F.to_string (id #4.));
  Printf.printf "%s\n" (id "abcde");
  ()

let () =
  Printf.printf "%s\n" (N.to_string (opaque #1n));
  Printf.printf "%s\n" (I32.to_string (opaque #2l));
  Printf.printf "%s\n" (I64.to_string (opaque #3L));
  Printf.printf "%s\n" (F.to_string (opaque #4.));
  Printf.printf "%s\n" (opaque "abcde");
  ()

let () =
  Printf.printf "%s\n" (N.to_string (magic #1n));
  Printf.printf "%s\n" (I32.to_string (magic #2l));
  Printf.printf "%s\n" (I64.to_string (magic #3L));
  Printf.printf "%s\n" (F.to_string (magic #4.));
  Printf.printf "%s\n" (magic "abcde");
  ()

let () =
  Printf.printf "%s\n" (N.to_string (apply (fun x -> x) #1n));
  Printf.printf "%s\n" (I32.to_string (apply (fun x -> x) #2l));
  Printf.printf "%s\n" (I64.to_string (apply (fun x -> x) #3L));
  Printf.printf "%s\n" (F.to_string (apply (fun x -> x) #4.));
  Printf.printf "%s\n" (apply (fun x -> x) "abcde");
  ()

let () =
  Printf.printf "%s\n" (N.to_string (revapply #1n (fun x -> x)));
  Printf.printf "%s\n" (I32.to_string (revapply #2l (fun x -> x)));
  Printf.printf "%s\n" (I64.to_string (revapply #3L (fun x -> x)));
  Printf.printf "%s\n" (F.to_string (revapply #4. (fun x -> x)));
  Printf.printf "%s\n" (revapply "abcde" (fun x -> x));
  ()

let () =
  Printf.printf "%s\n" (N.to_string (Sys.opaque_identity #1n));
  Printf.printf "%s\n" (I32.to_string (Sys.opaque_identity #2l));
  Printf.printf "%s\n" (I64.to_string (Sys.opaque_identity #3L));
  Printf.printf "%s\n" (F.to_string (Sys.opaque_identity #4.));
  Printf.printf "%s\n" (Sys.opaque_identity "abcde");
  ()
