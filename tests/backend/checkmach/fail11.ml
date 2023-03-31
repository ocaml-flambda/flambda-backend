exception E

type t = { x : int; mutable y : int }

(* There is a closure allocation in [f].

   Triggers
   Warning 68 [match-on-mutable-state-prevent-uncurry]: This pattern depends on
   mutable state.  It prevents the remaining arguments from being uncurried, which will
   cause additional closure allocations.

   See also
   https://github.com/ocaml/RFCs/pull/32 *)
let[@zero_alloc] f {x; y} () = Printf.eprintf "%d\n%!" y; raise E
let foo t = f t ()
