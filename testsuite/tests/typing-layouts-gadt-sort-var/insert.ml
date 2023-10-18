type t_void [@@void]

let rec g (_ : t_void) = ()

and packed = Gadt.Mk ((fun _ -> assert false), g)
