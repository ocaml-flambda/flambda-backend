(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files} stubs.c";
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_beta -extension simd_beta";
   native;
 }
*)
(* Test compilation correctness for array of unboxed int64x2s. General
   tests around type-checking should go to [basics.ml]. *)

module Int64x2_I = struct

  type t = int64x2

  external box : int64x2# -> int64x2 = "%box_vec128"
  external unbox : int64x2 -> int64x2# = "%unbox_vec128"

  external interleave_low_64 : t -> t -> t = "" "caml_sse2_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_64 : t -> t -> t = "" "caml_sse2_vec128_interleave_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_of : int64 -> t = "" "caml_int64x2_low_of_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_to : t -> int64 = "" "caml_int64x2_low_to_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : int64 -> t = "" "caml_int64x2_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add : t -> t -> t = "" "caml_sse2_int64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "" "caml_sse2_int64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  let neg x = sub (const1 0L) x

  let high_to x =
    let x = interleave_high_64 x x in
    low_to x

  let of_i64s x y =
    let x = low_of x in
    let y = low_of y in
    interleave_low_64 x y

  let mul x y =
    let xl, yl = low_to x, low_to y in
    let xh, yh = high_to x, high_to y in
    of_i64s Int64.(mul xh yh) Int64.(mul xl yl)

  let of_int i = of_i64s (Int64.of_int i) (Int64.of_int i)
  let max_val = of_i64s Int64.max_int Int64.max_int
  let min_val = of_i64s Int64.min_int Int64.min_int
  let rand x =
    let h, l = high_to x, low_to x in
    of_i64s (Random.int64 h) (Random.int64 l)

  let print v = Format.printf "%Ld:%Ld" (high_to v) (low_to v)

  let compare v1 v2 =
    let v1h, v2h = high_to v1, high_to v2 in
    let v1l, v2l = low_to v1, low_to v2 in
    let h = Int64.compare v1h v2h in
    if h = 0 then Int64.compare v1l v2l else h
end

module Int64x2_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = int64x2
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> Int64x2_I.compare x y = 0)
  let mem x a =
    let n = length a in
    let rec loop i =
      if i = n then false
      else if Int64x2_I.compare (unsafe_get a i) x = 0 then true
      else loop (succ i) in
    loop 0

  module I = Int64x2_I
end
module _ = Test_gen_u_array.Test (Int64x2_array)

module Int64x2_u_array0 : Gen_u_array.S0
                        with type element_t = int64x2#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = int64x2#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_unboxed_vec128_array_length
  external length : ('a : vec128). 'a array -> int = "%array_length"
  external get: ('a : vec128). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : vec128). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : vec128). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : vec128). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : vec128). int -> 'a array =
    "caml_make_unboxed_vec128_vect_bytecode" "caml_make_unboxed_vec128_vect"
  external unsafe_blit : ('a : vec128).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_unboxed_vec128_vect_blit"
  let empty () = [||]
  external to_boxed : int64x2# -> (int64x2[@local_opt]) = "%box_vec128"
  let compare_element x y = Int64x2_I.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Int64x2_u_array = Gen_u_array.Make (Int64x2_u_array0)
module Int64x2_u_array_boxed = Test_gen_u_array.Make_boxed (struct
  module M = Int64x2_u_array
  module I = Int64x2_I
  module E = struct
    external to_boxed : int64x2# -> (int64x2[@local_opt]) = "%box_vec128"
    external of_boxed : int64x2 -> int64x2# = "%unbox_vec128"
    let to_boxed x = to_boxed (x ())
    let of_boxed x () = of_boxed x
  end
end)
module _ = Test_gen_u_array.Test (Int64x2_u_array_boxed)



(* Extra tests for array expressions and patterns *)
module A = Int64x2_u_array_boxed
module I = Int64x2_u_array_boxed.I

let check_i a =
  let rec check_i_upto a i =
    if i >= 0 then begin
      assert (Int64x2_I.compare (A.get a i) (I.of_int i) = 0);
      check_i_upto a (i - 1);
    end
  in
  check_i_upto a (A.length a - 1)

let check_eq_f f arr = A.iteri (fun i x -> assert (Int64x2_I.compare x (f i) = 0)) arr
let check_all_the_same v arr = A.iter (fun x -> assert (Int64x2_I.compare x v = 0)) arr

let check_inval f arg =
  match f arg with
  | _ -> assert false
  | exception (Invalid_argument _) -> ()
  | exception _ -> assert false

external const : int64# -> int64x2# = "" "caml_int64x2_const1"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  (* empty arrays *)
  let test_empty_array arr =
    check_inval (fun a -> A.get a 0) arr;
    check_inval (fun a -> A.get a 1) arr;
    check_inval (fun a -> A.get a (-1)) arr;
    check_inval (fun a -> A.set a 0 (I.of_int 0)) arr;
    check_inval (fun a -> A.set a 1 (I.of_int 0)) arr;
    check_inval (fun a -> A.set a (-1) (I.of_int 0)) arr
  in
  let r : A.t = [||] in
  test_empty_array r;
  let r = A.make 0 (I.of_int 0) in
  test_empty_array r;

  (* static blocks *)
  let r = [|
const #0L; const #1L; const #2L; const #3L; const #4L; const #5L; const #6L;
const #7L; const #8L;  const #9L; const #10L; const #11L; const #12L; const #13L;
const #14L; const #15L; const #16L; const #17L; const #18L; const #19L; const #20L;
const #21L; const #22L; const #23L; const #24L; const #25L; const #26L; const #27L;
const #28L; const #29L; const #30L; const #31L; const #32L; const #33L; const #34L;
const #35L; const #36L; const #37L; const #38L; const #39L; const #40L; const #41L;
const #42L; const #43L; const #44L; const #45L; const #46L; const #47L; const #48L;
const #49L; const #50L; const #51L; const #52L; const #53L; const #54L; const #55L;
const #56L; const #57L; const #58L; const #59L; const #60L; const #61L; const #62L;
const #63L; const #64L; const #65L; const #66L; const #67L; const #68L; const #69L;
const #70L; const #71L; const #72L; const #73L; const #74L; const #75L; const #76L;
const #77L; const #78L; const #79L; const #80L; const #81L; const #82L; const #83L;
const #84L; const #85L; const #86L; const #87L; const #88L; const #89L; const #90L;
const #91L; const #92L; const #93L; const #94L; const #95L; const #96L; const #97L;
const #98L; const #99L; |]
  in
  check_i r;
  let r = [|
const #0L; const #1L; const #2L; const #3L; const #4L; const #5L; const #6L;
const #7L; const #8L;  const #9L; const #10L; const #11L; const #12L; const #13L;
const #14L; const #15L; const #16L; const #17L; const #18L; const #19L; const #20L |]
  in
  check_i r;
  let r = [|
const (-#123L); const (-#123L); const (-#123L); const (-#123L); const (-#123L); const (-#123L);
const (-#123L); const (-#123L); const (-#123L); const (-#123L); const (-#123L); |] in
  check_all_the_same (I.of_int (-123)) r;
  let r = [|
const (-#1L); const #1L; const (-#1L); const #1L; const (-#1L); const #1L; const (-#1L);
const #1L; const (-#1L); |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r = [|
const #1L; const (-#1L); const #1L; const (-#1L); const #1L; const (-#1L); const #1L;
const (-#1L) |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
f (const #0L); f (const #1L); f (const #2L); f (const #3L); f (const #4L); f (const #5L);
f (const #6L); f (const #7L); f (const #8L); f (const #9L); f (const #10L); f (const #11L);
f (const #12L); f (const #13L); f (const #14L); f (const #15L); f (const #16L); f (const #17L);
f (const #18L); f (const #19L); f (const #20L); f (const #21L); f (const #22L); f (const #23L);
f (const #24L); f (const #25L); f (const #26L); f (const #27L); f (const #28L); f (const #29L);
f (const #30L); f (const #31L); f (const #32L); f (const #33L); f (const #34L); f (const #35L);
f (const #36L); f (const #37L); f (const #38L); f (const #39L); f (const #40L); f (const #41L);
f (const #42L); f (const #43L); f (const #44L); f (const #45L); f (const #46L); f (const #47L);
f (const #48L); f (const #49L); f (const #50L); f (const #51L); f (const #52L); f (const #53L);
f (const #54L); f (const #55L); f (const #56L); f (const #57L); f (const #58L); f (const #59L);
f (const #60L); f (const #61L); f (const #62L); f (const #63L); f (const #64L); f (const #65L);
f (const #66L); f (const #67L); f (const #68L); f (const #69L); f (const #70L); f (const #71L);
f (const #72L); f (const #73L); f (const #74L); f (const #75L); f (const #76L); f (const #77L);
f (const #78L); f (const #79L); f (const #80L); f (const #81L); f (const #82L); f (const #83L);
f (const #84L); f (const #85L); f (const #86L); f (const #87L); f (const #88L); f (const #89L);
f (const #90L); f (const #91L); f (const #92L); f (const #93L); f (const #94L); f (const #95L);
f (const #96L); f (const #97L); f (const #98L); f (const #99L) |]
  in
  check_i r;
  let r = [|
f (const #0L); f (const #1L); f (const #2L); f (const #3L); f (const #4L); f (const #5L);
f (const #6L); f (const #7L); f (const #8L); f (const #9L); f (const #10L); f (const #11L);
f (const #12L); f (const #13L); f (const #14L); f (const #15L); f (const #16L); f (const #17L);
f (const #18L); f (const #19L); f (const #20L); |]
  in
  check_i r;
  let r = [|
f (const (-#123L)); f (const (-#123L)); f (const (-#123L)); f (const (-#123L)); f (const (-#123L));
f (const (-#123L)); f (const (-#123L)); f (const (-#123L)); f (const (-#123L)) |]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| const #0L; ((fun x -> Int64x2_I.(of_i64s x x |> unbox)) 1L) |];
  check_i [| const #0L; ((fun x -> Int64x2_I.(of_i64s x x |> unbox)) 1L); const #2L |];
  let r = [|
f (const (-#1L)); f (const #1L); f (const (-#1L)); f (const #1L); f (const (-#1L));
f (const #1L); f (const (-#1L)); f (const #1L); f (const (-#1L)) |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r = [|
f (const #1L); f (const (-#1L)); f (const #1L); f (const (-#1L)); f (const #1L);
f (const (-#1L)); f (const #1L); f (const (-#1L)) |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) x y = Int64x2_I.(compare (box x) (box y)) = 0 in
  (* match statement *)
  let d = [| const #1L; const #2L |] in
  (match d with
    | [| a; b |] ->
      assert (a = const #1L);
      assert (b = const #2L)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| const #1L |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = const #1L);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| const #1L |] = const #1L);
  ()
