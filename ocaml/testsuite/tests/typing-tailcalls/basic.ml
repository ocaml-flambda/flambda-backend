(* TEST
 flags = "-dtypedtree -dsel";
 native;
*)

let [@inline never] f str = print_endline str

module M = struct
  let f = f
end

let should_be_tail () =
  f "hello";
  f "goodbye"

let shouldn't_be_tail () =
  f "hello";
  M.f "goodbye"

