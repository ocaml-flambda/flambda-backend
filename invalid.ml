type t_void : void

external unbox_unit : unit -> t_void = "%unbox_unit"

type void_variant =
    A of t_void
  | B of t_void

let id1 = function
  | B _ -> ()
  | A _ -> assert false

let magic_B = id1 (B (unbox_unit ()))
