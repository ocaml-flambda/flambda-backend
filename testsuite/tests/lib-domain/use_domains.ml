(* TEST
   include systhreads;
   hassysthreads;
   multidomain;
   runtime5;
   { bytecode; }
   { native; }
*)

let () = Thread.use_domains ()

let () =
  Thread.join (Thread.create (fun () ->
      Domain.join ((Domain.Safe.spawn [@alert "-unsafe_parallelism"]) (fun () -> ()))) ())
