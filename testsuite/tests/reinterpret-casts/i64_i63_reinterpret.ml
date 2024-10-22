(* TEST *)

(* Tests for reinterpret-cast primitives tagged_int63 <-> unboxed_int64 *)

[@@@ocaml.flambda_o3]

external i63_to_i64 : int -> int64# =
  "%reinterpret_tagged_int63_as_unboxed_int64"
external i64_to_i63 : int64# -> int =
  "%reinterpret_unboxed_int64_as_tagged_int63"

external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"

(* List functions that can be unrolled *)

let rec map f = function
  | [] -> []
  | x::xs -> (f x) :: map f xs

let[@loop never] rec iter2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> ()
  | (a1::l1, a2::l2) -> f a1 a2; iter2 f l1 l2
  | (_, _) -> invalid_arg "iter2"

let examples = [min_int; -1; 0; 1; max_int]

let examples_int64 =
  (map [@unrolled 6])
    (fun i -> Int64.logor (Int64.mul (Int64.of_int i) 2L) 1L) examples

(* This simplifies to a function that just returns unit (and does nothing
   else)! *)
let[@inline never] test () =
  (iter2 [@unrolled 6]) (fun i i64 ->
      let i64_unboxed = i63_to_i64 i in
      let i64' = box_int64 i64_unboxed in
      if not (Int64.equal i64 i64') then
        failwith (Printf.sprintf
          "i63_to_i64 failure on 0x%x: got 0x%Lx, expected 0x%Lx" i i64' i64);
      let i' = i64_to_i63 i64_unboxed in
      if not (Int.equal i i') then
        failwith (Printf.sprintf
          "i64_to_i63 failure on 0x%Lx -> 0x%x: expected 0x%x" i64 i' i))
    examples examples_int64

(* This version checks the Cmm compilation of the primitives. *)
let[@inline never] test_opaque () =
  List.iter2 (fun i i64 ->
      let i64_unboxed =
        Sys.opaque_identity (i63_to_i64 (Sys.opaque_identity i))
      in
      let i64' = box_int64 i64_unboxed in
      if not (Int64.equal i64 i64') then
        failwith (Printf.sprintf
          "i63_to_i64 failure on 0x%x: got 0x%Lx, expected 0x%Lx" i i64' i64);
      let i' = i64_to_i63 i64_unboxed in
      if not (Int.equal i i') then
        failwith (Printf.sprintf
          "i64_to_i63 failure on 0x%Lx -> 0x%x: expected 0x%x" i64 i' i))
    examples examples_int64

let () =
  test ();
  test_opaque ();
  print_endline "ok"
