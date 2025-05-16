(* TEST
   include systhreads;
   hassysthreads;
   runtime5;
   { bytecode; }
   { native; }
*)

let () =
  Thread.join (Thread.create (fun () -> Thread.use_domains ()) ())
