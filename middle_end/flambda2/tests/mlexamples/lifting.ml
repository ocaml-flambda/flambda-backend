external ( + ) : int -> int -> int = "%addint"
external rand : unit -> int = "rand"

let r0 = rand ()

let r1 = rand ()

let r2 = rand ()

let [@inline always] f x = x + r0 + r1 + r2
