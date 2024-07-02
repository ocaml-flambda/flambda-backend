(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files}";
 include stdlib_upstream_compatible;
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }
*)
(* Test compilation correctness for array of unboxed int64s. General
   tests around type-checking should go to [basics.ml]. *)

module Int64_I = struct
  include Int64
  let max_val = max_int
  let min_val = min_int
  let rand = Random.int64
  let print = Format.printf "%Ld"
end

module Int64_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = int64
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Int64_I
end
module _ = Test_gen_u_array.Test (Int64_array)

module Int64_u_array0 : Gen_u_array.S0
                        with type element_t = int64#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = int64#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_unboxed_int64_array_length
  external length : ('a : bits64). 'a array -> int = "%array_length"
  external get: ('a : bits64). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : bits64). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : bits64). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : bits64). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : bits64). int -> 'a array =
    "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"
  external unsafe_blit : ('a : bits64).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_unboxed_int64_vect_blit"
  let empty () = [||]
  external to_boxed : ('a : bits64) -> (int64[@local_opt]) = "%box_int64"
  let compare_element x y = Int64.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Int64_u_array = Gen_u_array.Make (Int64_u_array0)
module Int64_u_array_boxed = Test_gen_u_array.Make_boxed (struct
  module M = Int64_u_array
  module I = Int64_I
  module E = struct
    open Stdlib_upstream_compatible.Int64_u
    let to_boxed x = to_int64 (x ())
    let of_boxed x () = of_int64 x
  end
end)
module _ = Test_gen_u_array.Test (Int64_u_array_boxed)



(* Extra tests for array expressions and patterns *)
module A = Int64_u_array_boxed
module I = Int64_u_array_boxed.I

let check_i a =
  let rec check_i_upto a i =
    if i >= 0 then begin
      assert (A.get a i = I.of_int i);
      check_i_upto a (i - 1);
    end
  in
  check_i_upto a (A.length a - 1)

let check_eq_f f arr = A.iteri (fun i x -> assert (x = f i)) arr
let check_all_the_same v arr = A.iter (fun x -> assert (x = v)) arr

let check_inval f arg =
  match f arg with
  | _ -> assert false
  | exception (Invalid_argument _) -> ()
  | exception _ -> assert false

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
#0L;#1L;#2L;#3L;#4L;#5L;#6L;#7L;#8L;#9L;#10L;#11L;#12L;#13L;#14L;#15L;#16L;#17L;
#18L;#19L;#20L;#21L;#22L;#23L;#24L;#25L;#26L;#27L;#28L;#29L;#30L;#31L;#32L;#33L;#34L;#35L;
#36L;#37L;#38L;#39L;#40L;#41L;#42L;#43L;#44L;#45L;#46L;#47L;#48L;#49L;#50L;#51L;#52L;#53L;
#54L;#55L;#56L;#57L;#58L;#59L;#60L;#61L;#62L;#63L;#64L;#65L;#66L;#67L;#68L;#69L;#70L;#71L;
#72L;#73L;#74L;#75L;#76L;#77L;#78L;#79L;#80L;#81L;#82L;#83L;#84L;#85L;#86L;#87L;#88L;#89L;
#90L;#91L;#92L;#93L;#94L;#95L;#96L;#97L;#98L;#99L; |]
  in
  check_i r;
  let r = [|
#0L;#1L;#2L;#3L;#4L;#5L;#6L;#7L;#8L;#9L;#10L;#11L;#12L;#13L;#14L;#15L;#16L;#17L;
#18L;#19L;#20L;#21L;#22L;#23L;#24L;#25L;#26L;#27L;#28L;#29L;#30L;#31L;#32L;#33L;#34L;#35L;
#36L;#37L;#38L;#39L;#40L;#41L;#42L;#43L;#44L;#45L;#46L;#47L;#48L;#49L;#50L;#51L;#52L;#53L;
#54L;#55L;#56L;#57L;#58L;#59L;#60L;#61L;#62L;#63L;#64L;#65L;#66L;#67L;#68L;#69L;#70L;#71L;
#72L;#73L;#74L;#75L;#76L;#77L;#78L;#79L;#80L;#81L;#82L;#83L;#84L;#85L;#86L;#87L;#88L;#89L;
#90L;#91L;#92L;#93L;#94L;#95L;#96L;#97L;#98L;#99L;#100L; |]
  in
  check_i r;
  let r = [|-#123L;-#123L;-#123L;-#123L;-#123L;-#123L;-#123L;-#123L;-#123L;-#123L;-#123L;|] in
  check_all_the_same (I.of_int (-123)) r;
  let r =
    [|-#1L; #1L; -#1L; #1L; -#1L; #1L; -#1L; #1L; -#1L;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|#1L; -#1L; #1L; -#1L; #1L; -#1L; #1L; -#1L;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
    f #0L;f #1L;f #2L;f #3L;f #4L;f #5L;f #6L;f #7L;f #8L;f #9L;f #10L;f #11L;f #12L;f #13L;f #14L;
    f #15L;f #16L;f #17L;f #18L;f #19L;f #20L;f #21L;f #22L;f #23L;f #24L;f #25L;f #26L;f #27L;f #28L;f #29L;
    f #30L;f #31L;f #32L;f #33L;f #34L;f #35L;f #36L;f #37L;f #38L;f #39L;f #40L;f #41L;f #42L;f #43L;f #44L;
    f #45L;f #46L;f #47L;f #48L;f #49L;f #50L;f #51L;f #52L;f #53L;f #54L;f #55L;f #56L;f #57L;f #58L;f #59L;
    f #60L;f #61L;f #62L;f #63L;f #64L;f #65L;f #66L;f #67L;f #68L;f #69L;f #70L;f #71L;f #72L;f #73L;f #74L;
    f #75L;f #76L;f #77L;f #78L;f #79L;f #80L;f #81L;f #82L;f #83L;f #84L;f #85L;f #86L;f #87L;f #88L;f #89L;
    f #90L;f #91L;f #92L;f #93L;f #94L;f #95L;f #96L;f #97L;f #98L;f #99L; |]
  in
  check_i r;
  let r = [|
    f #0L;f #1L;f #2L;f #3L;f #4L;f #5L;f #6L;f #7L;f #8L;f #9L;f #10L;f #11L;f #12L;f #13L;f #14L;
    f #15L;f #16L;f #17L;f #18L;f #19L;f #20L;f #21L;f #22L;f #23L;f #24L;f #25L;f #26L;f #27L;f #28L;f #29L;
    f #30L;f #31L;f #32L;f #33L;f #34L;f #35L;f #36L;f #37L;f #38L;f #39L;f #40L;f #41L;f #42L;f #43L;f #44L;
    f #45L;f #46L;f #47L;f #48L;f #49L;f #50L;f #51L;f #52L;f #53L;f #54L;f #55L;f #56L;f #57L;f #58L;f #59L;
    f #60L;f #61L;f #62L;f #63L;f #64L;f #65L;f #66L;f #67L;f #68L;f #69L;f #70L;f #71L;f #72L;f #73L;f #74L;
    f #75L;f #76L;f #77L;f #78L;f #79L;f #80L;f #81L;f #82L;f #83L;f #84L;f #85L;f #86L;f #87L;f #88L;f #89L;
    f #90L;f #91L;f #92L;f #93L;f #94L;f #95L;f #96L;f #97L;f #98L;f #99L;f #100L; |]
  in
  check_i r;
  let r =
    [|f (-#123L);f (-#123L);f (-#123L);f (-#123L);f (-#123L);f (-#123L);f (-#123L);f (-#123L);f (-#123L);|]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| #0L; ((fun x -> x) #1L)|];
  check_i [| #0L; ((fun x -> x) #1L); #2L|];
  let r =
    [|f (-#1L);f (#1L);f (-#1L);f (#1L);f (-#1L);f (#1L);f (-#1L);f (#1L);f (-#1L);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|f (#1L);f (-#1L);f (#1L);f (-#1L);f (#1L);f (-#1L);f (#1L);f (-#1L);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) = Stdlib_upstream_compatible.Int64_u.equal in
  (* match statement *)
  let d = [| #1L; #2L |] in
  (match d with
    | [| a; b |] ->
      assert (a = #1L);
      assert (b = #2L)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| #1L |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1L);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1L |] = #1L);
  ()
