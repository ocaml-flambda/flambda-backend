module A = struct type t = A | B end
module B = struct
  let print = function
      A.A -> print_endline "A.A"
    | A.B -> print_endline "A.B"
end

module M = Make(A)(B);;

let () = M.f ()
