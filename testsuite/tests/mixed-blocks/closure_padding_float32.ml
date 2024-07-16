(* TEST
   runtime5;
   include stdlib_stable;
   ocamlopt_flags="-extension layouts_alpha -extension small_numbers";
   native;
*)

(* From https://github.com/ocaml-flambda/flambda-backend/pull/2698
   although submitted as part of
   https://github.com/ocaml-flambda/flambda-backend/pull/2763
*)

module Float32_u = Stdlib_stable.Float32_u

type t =
  | Mutable_str of { mutable x : string }
  | Float32 of float32#

let[@inline always] go x y =
  let f =
    match x with
    | Float32 f32 ->
      (fun[@inline never] () ->
         match y with
         | Mutable_str _ -> f32
         | Float32 _ -> assert false)
    | Mutable_str _ -> assert false
  in
  f ()

let () =
  let x = Float32 #4.s in
  let y = Mutable_str { x = "s" } in
  let _ = go x y in
  ()
