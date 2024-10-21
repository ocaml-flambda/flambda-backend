exception E

type t = { x : int; mutable y : int }

(* [f] is a normal 2-ary function; full applications of [f] do not allocate
   a closure.

   On old versions of the compiler, this would allocate a closure and would trigger
   the warning:

   Warning 68 [match-on-mutable-state-prevent-uncurry]: This pattern depends on
   mutable state.  It prevents the remaining arguments from being uncurried, which will
   cause additional closure allocations.

   See also
   https://github.com/ocaml/RFCs/pull/32 *)
let[@zero_alloc] f {x; y} () = Printf.eprintf "%d\n%!" y; raise E
let foo t = f t ()
