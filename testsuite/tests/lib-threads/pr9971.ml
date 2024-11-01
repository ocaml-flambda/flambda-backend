(* TEST
 runtime4;
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* This test was deleted in OCaml 5 upstream
   (rev 55da58ca6c9144331c7fa56a5d0083cb97b50925) *)

let t =
  let t = Thread.create (fun _ -> ())() in
  Thread.join t

let () =
  Thread.exit ()
