let f g = g _

let go () = let g = f (fun x -> x) in g 1