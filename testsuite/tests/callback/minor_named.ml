(* TEST
<<<<<<< HEAD
 include unix;
 modules = "minor_named_.c";
 libunix;
 {
   bytecode;
 }{
   native;
 }
||||||| 121bedcfd2
   include unix
   modules = "minor_named_.c"
   * libunix
   ** bytecode
   ** native
=======
 modules = "minor_named_.c";
>>>>>>> 5.2.0
*)

(* Tests Callback.register and caml_named_value on a young object *)

external incr_ref : unit -> unit = "incr_ref"

let () =
  let r = ref 40 in
  Callback.register "incr_ref" r;
  incr_ref ();
  Gc.minor ();
  incr_ref ();
  Printf.printf "%d\n" !r
