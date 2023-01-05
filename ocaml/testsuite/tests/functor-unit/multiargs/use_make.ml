(* CR-soon lmaurer: In a previous incarnation of functor units, we let `Param2`
   refer to `Param`, creating an interdependency between parameter types. Punting
   on this for now, as we'll soon have a better story for it with open
   compilation units. Once we can write on the command line that `Param` occurs
   free in `Param2`, we can make [A.t] a plain variant type again and let
   [B.print] refer to it (adjusting [Param] and [Param2] to match, of course). *)
module A = struct type t = [ `A | `B ] end
module B = struct
  let print = function
      `A -> print_endline "A.A"
    | `B -> print_endline "A.B"
end

module M = Make(A)(B);;

let () = M.f ()
