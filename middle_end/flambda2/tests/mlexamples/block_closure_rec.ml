(* A statically-allocated block in a recursive loop with a closure *)

let rec foo x = `Foo foo
