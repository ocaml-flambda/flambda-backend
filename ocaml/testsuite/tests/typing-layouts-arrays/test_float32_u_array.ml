(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files}";
 include beta;
 flambda2;
 {
   flags = "-extension small_numbers";
   bytecode;
 }{
   flags = "-extension small_numbers";
   native;
 }{
   flags = "-extension layouts_beta -extension small_numbers";
   bytecode;
 }{
   flags = "-extension layouts_beta -extension small_numbers";
   native;
 }
*)
(* Test compilation correctness for array of unboxed float32s. General
   tests around type-checking should go to [basics.ml]. *)

module Float32_I = struct
  include Beta.Float32
  let max_val = max_float
  let min_val = min_float
  let rand f = of_float (Random.float (to_float f))
  let print f = Format.printf "%f" (to_float f)
end

module Float32_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = float32
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Float32_I
end
module _ = Test_gen_u_array.Test (Float32_array)

module Float32_u_array0 : Gen_u_array.S0
                        with type element_t = float32#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = float32#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  external length : ('a : float32). 'a array -> int = "%array_length"
  external get: ('a : float32). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : float32). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : float32). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : float32). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : float32). int -> 'a array =
    "caml_make_unboxed_float32_vect_bytecode" "caml_make_unboxed_float32_vect"
  external unsafe_blit : ('a : float32).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_unboxed_float32_vect_blit"
  let empty () = [||]
  external to_boxed : ('a : float32) -> (float32[@local_opt]) = "%box_float32"
  let compare_element x y = Beta.Float32.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Float32_u_array = Gen_u_array.Make (Float32_u_array0)
module Float32_u_array_boxed : Test_gen_u_array.S with type t = float32# array = Test_gen_u_array.Make_boxed (struct
  module M = Float32_u_array
  module I = Float32_I
  module E = struct
    open Beta.Float32_u
    let to_boxed x = to_float32 (x ())
    let of_boxed x () = of_float32 x
  end
end)
module _ = Test_gen_u_array.Test (Float32_u_array_boxed)


(* Extra tests for array expressions and patterns *)
module A = Float32_u_array_boxed
module I = Float32_u_array_boxed.I

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
  let r = A.make (Sys.opaque_identity 0) (I.of_int 0) in
  test_empty_array r;

  (* static blocks *)
  let r = [|
#0.s;#1.s;#2.s;#3.s;#4.s;#5.s;#6.s;#7.s;#8.s;#9.s;#10.s;#11.s;#12.s;#13.s;#14.s;#15.s;#16.s;#17.s;
#18.s;#19.s;#20.s;#21.s;#22.s;#23.s;#24.s;#25.s;#26.s;#27.s;#28.s;#29.s;#30.s;#31.s;#32.s;#33.s;#34.s;#35.s;
#36.s;#37.s;#38.s;#39.s;#40.s;#41.s;#42.s;#43.s;#44.s;#45.s;#46.s;#47.s;#48.s;#49.s;#50.s;#51.s;#52.s;#53.s;
#54.s;#55.s;#56.s;#57.s;#58.s;#59.s;#60.s;#61.s;#62.s;#63.s;#64.s;#65.s;#66.s;#67.s;#68.s;#69.s;#70.s;#71.s;
#72.s;#73.s;#74.s;#75.s;#76.s;#77.s;#78.s;#79.s;#80.s;#81.s;#82.s;#83.s;#84.s;#85.s;#86.s;#87.s;#88.s;#89.s;
#90.s;#91.s;#92.s;#93.s;#94.s;#95.s;#96.s;#97.s;#98.s;#99.s; |]
  in
  check_i r;
  let r = [|
#0.s;#1.s;#2.s;#3.s;#4.s;#5.s;#6.s;#7.s;#8.s;#9.s;#10.s;#11.s;#12.s;#13.s;#14.s;#15.s;#16.s;#17.s;
#18.s;#19.s;#20.s;#21.s;#22.s;#23.s;#24.s;#25.s;#26.s;#27.s;#28.s;#29.s;#30.s;#31.s;#32.s;#33.s;#34.s;#35.s;
#36.s;#37.s;#38.s;#39.s;#40.s;#41.s;#42.s;#43.s;#44.s;#45.s;#46.s;#47.s;#48.s;#49.s;#50.s;#51.s;#52.s;#53.s;
#54.s;#55.s;#56.s;#57.s;#58.s;#59.s;#60.s;#61.s;#62.s;#63.s;#64.s;#65.s;#66.s;#67.s;#68.s;#69.s;#70.s;#71.s;
#72.s;#73.s;#74.s;#75.s;#76.s;#77.s;#78.s;#79.s;#80.s;#81.s;#82.s;#83.s;#84.s;#85.s;#86.s;#87.s;#88.s;#89.s;
#90.s;#91.s;#92.s;#93.s;#94.s;#95.s;#96.s;#97.s;#98.s;#99.s;#100.s; |]
  in
  check_i r;
  let r = [|-#123.s;-#123.s;-#123.s;-#123.s;-#123.s;-#123.s;-#123.s;-#123.s;-#123.s;-#123.s;-#123.s;|] in
  check_all_the_same (I.of_int (-123)) r;
  let r =
    [|-#1.s; #1.s; -#1.s; #1.s; -#1.s; #1.s; -#1.s; #1.s; -#1.s;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|#1.s; -#1.s; #1.s; -#1.s; #1.s; -#1.s; #1.s; -#1.s;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
    f #0.s;f #1.s;f #2.s;f #3.s;f #4.s;f #5.s;f #6.s;f #7.s;f #8.s;f #9.s;f #10.s;f #11.s;f #12.s;f #13.s;f #14.s;
    f #15.s;f #16.s;f #17.s;f #18.s;f #19.s;f #20.s;f #21.s;f #22.s;f #23.s;f #24.s;f #25.s;f #26.s;f #27.s;f #28.s;f #29.s;
    f #30.s;f #31.s;f #32.s;f #33.s;f #34.s;f #35.s;f #36.s;f #37.s;f #38.s;f #39.s;f #40.s;f #41.s;f #42.s;f #43.s;f #44.s;
    f #45.s;f #46.s;f #47.s;f #48.s;f #49.s;f #50.s;f #51.s;f #52.s;f #53.s;f #54.s;f #55.s;f #56.s;f #57.s;f #58.s;f #59.s;
    f #60.s;f #61.s;f #62.s;f #63.s;f #64.s;f #65.s;f #66.s;f #67.s;f #68.s;f #69.s;f #70.s;f #71.s;f #72.s;f #73.s;f #74.s;
    f #75.s;f #76.s;f #77.s;f #78.s;f #79.s;f #80.s;f #81.s;f #82.s;f #83.s;f #84.s;f #85.s;f #86.s;f #87.s;f #88.s;f #89.s;
    f #90.s;f #91.s;f #92.s;f #93.s;f #94.s;f #95.s;f #96.s;f #97.s;f #98.s;f #99.s; |]
  in
  check_i r;
  let r = [|
    f #0.s;f #1.s;f #2.s;f #3.s;f #4.s;f #5.s;f #6.s;f #7.s;f #8.s;f #9.s;f #10.s;f #11.s;f #12.s;f #13.s;f #14.s;
    f #15.s;f #16.s;f #17.s;f #18.s;f #19.s;f #20.s;f #21.s;f #22.s;f #23.s;f #24.s;f #25.s;f #26.s;f #27.s;f #28.s;f #29.s;
    f #30.s;f #31.s;f #32.s;f #33.s;f #34.s;f #35.s;f #36.s;f #37.s;f #38.s;f #39.s;f #40.s;f #41.s;f #42.s;f #43.s;f #44.s;
    f #45.s;f #46.s;f #47.s;f #48.s;f #49.s;f #50.s;f #51.s;f #52.s;f #53.s;f #54.s;f #55.s;f #56.s;f #57.s;f #58.s;f #59.s;
    f #60.s;f #61.s;f #62.s;f #63.s;f #64.s;f #65.s;f #66.s;f #67.s;f #68.s;f #69.s;f #70.s;f #71.s;f #72.s;f #73.s;f #74.s;
    f #75.s;f #76.s;f #77.s;f #78.s;f #79.s;f #80.s;f #81.s;f #82.s;f #83.s;f #84.s;f #85.s;f #86.s;f #87.s;f #88.s;f #89.s;
    f #90.s;f #91.s;f #92.s;f #93.s;f #94.s;f #95.s;f #96.s;f #97.s;f #98.s;f #99.s;f #100.s; |]
  in
  check_i r;
  let r =
    [|f (-#123.s);f (-#123.s);f (-#123.s);f (-#123.s);f (-#123.s);f (-#123.s);f (-#123.s);f (-#123.s);f (-#123.s);|]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| #0.s; ((fun x -> x) #1.s)|];
  check_i [| #0.s; ((fun x -> x) #1.s); #2.s|];
  let r =
    [|f (-#1.s);f (#1.s);f (-#1.s);f (#1.s);f (-#1.s);f (#1.s);f (-#1.s);f (#1.s);f (-#1.s);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|f (#1.s);f (-#1.s);f (#1.s);f (-#1.s);f (#1.s);f (-#1.s);f (#1.s);f (-#1.s);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) = Beta.Float32_u.equal in
  (* match statement *)
  let d = [| #1.s; #2.s |] in
  (match d with
    | [| a; b |] ->
      assert (a = #1.s);
      assert (b = #2.s)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| #1.s |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1.s);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1.s |] = #1.s);
  ()
