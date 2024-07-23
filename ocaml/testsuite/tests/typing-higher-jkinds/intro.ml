(* TEST
 flags = "-g -extension layouts_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* module type S = sig
  type t : any
end

module C : S = struct
  type t = float
end

let x = (module C : S with type t = 'a) *)

type ('m : value => value) monad = {
  return : 'a. 'a -> 'a 'm;
  bind : 'a 'b. 'a 'm -> ('a -> 'b 'm) -> 'b 'm
}

let singleton : 'a. 'a -> 'a list = fun x -> [x]
let concat_map : 'a 'b. 'a list -> ('a -> 'b list) -> 'b list = fun xs f -> List.concat_map f xs

let list : list monad = {
  return = singleton;
  bind = concat_map
}

let prod m x y = m.bind x (fun a -> m.bind y (fun b -> m.return (a, b)))

let xs : int list = [-1; 1]
let ys : int list = [0; 1; 2]
let result = prod list xs ys
let () = assert (result = [(-1, 0); (-1, 1); (-1, 2); (1, 0); (1, 1); (1, 2)])
