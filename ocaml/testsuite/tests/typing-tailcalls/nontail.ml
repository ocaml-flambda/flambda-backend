(* TEST
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids";
 native;
*)

let [@inline never] f str = print_endline str

module M = struct
  let f = f
end

(* These calls should not be inferred as tail-calls because their callees
   are not defined in some ancestor let rec. *)
let foo () =
  f "hello";
  f "goodbye"

let bar () =
  f "hello";
  M.f "goodbye"
