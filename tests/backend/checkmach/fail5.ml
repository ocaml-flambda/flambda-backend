exception Exn of (int * int)
let[@noalloc] test x f =
  (* alloc_exn check fails because of the indirect call to [f], that is not post-dominated
     by a raise. *)
  if x > 0 then f x else raise (Exn (f (x*x),f (x+x)))
