
type fun_t = arg1:Function_a.t -> arg2:Function_a.t -> unit -> Function_a.t

type take_t = Function_a.t -> unit
type return_t = unit -> Function_a.t

let f_opt : ?opt:int -> Function_a.t -> unit = fun ?opt _ -> ()
let f_opt_2 : ?opt:int -> unit -> Function_a.t = fun ?opt _ -> assert false

