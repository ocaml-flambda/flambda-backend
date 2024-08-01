(* TEST
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids -dcmm -c";
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


let weird5 n = (((n * 3 + 1) * 4) mod 5) = 0
let weird6 n = (((n * 3 + 1) * 4) mod 6) = 0

let if_statement n =
  weird6 (if weird5 n then n + 1 else n + 2)


let amem s t = Sys.opaque_identity true

let () =
  let maybe_negate under_not m v =
    if under_not
    then (
      match v with
      | None -> Some m
      | Some _ -> None)
    else v
  in
  let mem s v under_not m =
    maybe_negate m (if amem s v then Some m else None)
  in
  let ofday_mem s time under_not m =
    maybe_negate m (if amem s time then Some m else None)
  in
  Sys.opaque_identity (ignore maybe_negate; ignore mem; ignore ofday_mem)
