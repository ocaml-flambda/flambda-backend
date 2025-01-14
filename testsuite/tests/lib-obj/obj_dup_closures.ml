(* TEST *)

external int_as_pointer : _ -> int = "%int_as_pointer"

module Int64_u = struct
  external to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
    [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"
    [@@warning "-187"]

  let equal x y = Int64.equal (to_int64 x) (to_int64 y)
end

let[@opaque] rand_near_minor_heap () =
  let r = ref () in
  let i : int = Obj.magic (int_as_pointer r) in
  let minor_heap_size_in_bytes =
    assert (Sys.word_size = 64);
    (Gc.get ()).minor_heap_size * Sys.word_size / 8
  in
  let b = minor_heap_size_in_bytes / 2 in
  let n = (Random.int b - (b / 2)) * 2 in
  Int64_u.of_int64 (Int64.of_int ((i * 2) + n))

let[@opaque] rand_string () =
  match Random.int 3 with
  | 0 -> "goat"
  | 1 -> "sheep"
  | 2 -> "cow"
  | _ -> assert false

(* Example flambda2 output:

(c1_1arg/870UV) =
  (set_of_closures Heap
  ({((c1_1arg/4 ∷ 𝕍*|Null) camlObj_dup_closures__c1_1arg_4_22_code)})
  (env {((i64_1/0 ∷ ℕ𝟞𝟜) i64_1/864UV)}))
(c2_1arg/879UV) =
  (set_of_closures Heap
  ({((c2_1arg/5 ∷ 𝕍*|Null) camlObj_dup_closures__c2_1arg_5_23_code)})
  (env {((i64_2/1 ∷ ℕ𝟞𝟜) i64_2/865UV) ((x/2 ∷ 𝕍) x/868UV)}))
(c3_1arg/892UV) =
  (set_of_closures Heap
  ({((c3_1arg/6 ∷ 𝕍*|Null) camlObj_dup_closures__c3_1arg_6_24_code)})
  (env {((x/3 ∷ 𝕍) x/868UV)}))
(c1_2arg/904UV) =
  (set_of_closures Heap
  ({((c1_2arg/7 ∷ 𝕍*|Null) camlObj_dup_closures__c1_2arg_7_25_code)})
  (env {((i64_3/4 ∷ ℕ𝟞𝟜) i64_3/866UV)}))
(c2_2arg/914UV) =
  (set_of_closures Heap
  ({((c2_2arg/8 ∷ 𝕍*|Null) camlObj_dup_closures__c2_2arg_8_26_code)})
  (env {((i64_4/5 ∷ ℕ𝟞𝟜) i64_4/867UV) ((x/6 ∷ 𝕍) x/868UV)}))
(c3_2arg/928UV) =
  (set_of_closures Heap
  ({((c3_2arg/9 ∷ 𝕍*|Null) camlObj_dup_closures__c3_2arg_9_27_code)})
  (env {((x/7 ∷ 𝕍) x/868UV)}))
(rec_c1_1arg/941UV rec_c2_1arg/942UV rec_c3_1arg/943UV rec_c1_2arg/944UV
  rec_c2_2arg/945UV rec_c3_2arg/946UV) =
  (set_of_closures Heap
  ({((rec_c1_1arg/10 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c1_1arg_10_28_code)
    ((rec_c2_1arg/11 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c2_1arg_11_29_code)
    ((rec_c3_1arg/12 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c3_1arg_12_30_code)
    ((rec_c1_2arg/13 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c1_2arg_13_31_code)
    ((rec_c2_2arg/14 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c2_2arg_14_32_code)
    ((rec_c3_2arg/15 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c3_2arg_15_33_code)})
  (env
    {((i64_1/8 ∷ ℕ𝟞𝟜) i64_1/864UV) ((i64_2/9 ∷ ℕ𝟞𝟜) i64_2/865UV)
    ((i64_3/10 ∷ ℕ𝟞𝟜) i64_3/866UV) ((i64_4/11 ∷ ℕ𝟞𝟜) i64_4/867UV)
    ((x/12 ∷ 𝕍) x/868UV)}))
*)

let[@opaque] make_closures (i64_1 : int64#) (i64_2 : int64#)
    (i64_3 : int64#) (i64_4 : int64#) (x : string) =
  (* Two-word function slot (i.e. one argument) cases *)
  let[@opaque] c1_1arg () =
    (* Only an unboxed environment *)
    i64_1
  in
  let[@opaque] c2_1arg () =
    (* An unboxed environment plus a scannable environment *)
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_2
  in
  let[@opaque] c3_1arg () =
    (* Only a scannable environment *)
    let (_i : int) = Sys.opaque_identity (String.length x) in
    100
  in
  (* Three-word function slot (i.e. more than one argument) cases *)
  let[@opaque] c1_2arg () () = i64_3 in
  let[@opaque] c2_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_4
  in
  let[@opaque] c3_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    200
  in
  (* Cases to exercise [Infix_tag] logic *)
  let[@opaque] rec rec_c1_1arg () = i64_1
  and[@opaque] rec_c2_1arg () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_2
  and[@opaque] rec_c3_1arg () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    300
  and[@opaque] rec_c1_2arg () () =
    let (_ : int64#) = Sys.opaque_identity i64_3 in
    rec_c1_1arg ()
  and[@opaque] rec_c2_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    let (_ : int64#) = Sys.opaque_identity i64_4 in
    rec_c2_1arg ()
  and[@opaque] rec_c3_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    rec_c3_1arg ()
  in
  ( c1_1arg,
    c2_1arg,
    c3_1arg,
    c1_2arg,
    c2_2arg,
    c3_2arg,
    rec_c1_1arg,
    rec_c2_1arg,
    rec_c3_1arg,
    rec_c1_2arg,
    rec_c2_2arg,
    rec_c3_2arg )

let check (i64_1 : int64#) (i64_2 : int64#) (i64_3 : int64#)
    (i64_4 : int64#) (x : string)
    ( c1_1arg,
      c2_1arg,
      c3_1arg,
      c1_2arg,
      c2_2arg,
      c3_2arg,
      rec_c1_1arg,
      rec_c2_1arg,
      rec_c3_1arg,
      rec_c1_2arg,
      rec_c2_2arg,
      rec_c3_2arg ) =
  assert (Int64_u.equal (c1_1arg ()) i64_1);
  assert (Int64_u.equal (c2_1arg ()) i64_2);
  assert (Int.equal (c3_1arg ()) 100);
  assert (Int64_u.equal (c1_2arg () ()) i64_3);
  assert (Int64_u.equal (c2_2arg () ()) i64_4);
  assert (Int.equal (c3_2arg () ()) 200);
  assert (Int64_u.equal (rec_c1_1arg ()) i64_1);
  assert (Int64_u.equal (rec_c2_1arg ()) i64_2);
  assert (Int.equal (rec_c3_1arg ()) 300);
  assert (Int64_u.equal (rec_c1_2arg () ()) i64_1);
  assert (Int64_u.equal (rec_c2_2arg () ()) i64_2);
  assert (Int.equal (rec_c3_2arg () ()) 300)

let check_one () =
  let i64_1 = rand_near_minor_heap () in
  let i64_2 = rand_near_minor_heap () in
  let i64_3 = rand_near_minor_heap () in
  let i64_4 = rand_near_minor_heap () in
  let x = rand_string () in
  let ( c1_1arg_original,
        c2_1arg_original,
        c3_1arg_original,
        c1_2arg_original,
        c2_2arg_original,
        c3_2arg_original,
        rec_c1_1arg_original,
        rec_c2_1arg_original,
        rec_c3_1arg_original,
        rec_c1_2arg_original,
        rec_c2_2arg_original,
        rec_c3_2arg_original ) =
    make_closures i64_1 i64_2 i64_3 i64_4 x
  in
  let dup (type a) (x : a) : a = Obj.(obj (dup (repr x))) in
  let c1_1arg = dup c1_1arg_original in
  let c2_1arg = dup c2_1arg_original in
  let c3_1arg = dup c3_1arg_original in
  let c1_2arg = dup c1_2arg_original in
  let c2_2arg = dup c2_2arg_original in
  let c3_2arg = dup c3_2arg_original in
  let rec_c1_1arg = dup rec_c1_1arg_original in
  let rec_c2_1arg = dup rec_c2_1arg_original in
  let rec_c3_1arg = dup rec_c3_1arg_original in
  let rec_c1_2arg = dup rec_c1_2arg_original in
  let rec_c2_2arg = dup rec_c2_2arg_original in
  let rec_c3_2arg = dup rec_c3_2arg_original in
  Gc.compact ();
  check i64_1 i64_2 i64_3 i64_4 x
    ( c1_1arg,
      c2_1arg,
      c3_1arg,
      c1_2arg,
      c2_2arg,
      c3_2arg,
      rec_c1_1arg,
      rec_c2_1arg,
      rec_c3_1arg,
      rec_c1_2arg,
      rec_c2_2arg,
      rec_c3_2arg )

let () =
  Random.init 123;
  for x = 1 to 10_000 do
    check_one ()
  done
