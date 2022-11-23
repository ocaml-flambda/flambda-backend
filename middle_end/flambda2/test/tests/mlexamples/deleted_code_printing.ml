(* This example shows a case where the generated program starts with a sequence
   of deleted code bindings, then a variable, then the rest of the program. This
   caused a bug when printing the result if deleted bindings are not printed, as
   the printer expects all sequences of symbol/code bindings to include at least
   one printable element. *)
type t = { mutable foo : int }

let env = { foo = 42 }

external id : 'a -> 'a = "%opaque"

let foobar () =
  let rec aux () = if id false then env.foo else aux () in
  aux ()
