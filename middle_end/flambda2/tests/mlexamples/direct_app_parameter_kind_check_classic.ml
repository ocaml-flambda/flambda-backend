[@@@ocaml.flambda_oclassic]

type _ arity = Unb : ( #('a * 'b) -> unit) arity
let x : (#(int * int) -> unit) arity = Unb

type _ tuple = Tup2 : {  arg1 : 'a ;  arg2 : 'b } -> ( 'a -> 'b -> unit) tuple

type 'c t = 'c tuple option ref

let send (type c) (t : c t) (c : c) : unit =
  match !t with
  | None -> ()
  | Some (Tup2 {arg1; arg2}) -> c arg1 arg2

let create (type c) (_arity : c arity) : c t =
 ref None

let () = send (create x) (fun _ -> ());
