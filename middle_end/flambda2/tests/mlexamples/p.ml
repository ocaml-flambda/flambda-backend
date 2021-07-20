
let [@inline never] foo k o _ = 0

let bar oc z = foo (fun x -> x) oc z
let f z = bar 0 z
