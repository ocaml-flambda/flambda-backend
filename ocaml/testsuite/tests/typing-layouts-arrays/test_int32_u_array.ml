(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files}";
 include stable;
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
(* Test compilation correctness for array of unboxed int32s. General
   tests around type-checking should go to [basics.ml]. *)

module Int32_I = struct
  include Int32
  let max_val = max_int
  let min_val = min_int
  let rand = Random.int32
  let print = Format.printf "%ld"
end

module Int32_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = int32
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Int32_I
end
module _ = Test_gen_u_array.Test (Int32_array)

module Int32_u_array0 : Gen_u_array.S0
                        with type element_t = int32#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = int32#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array

  let max_length =
    match Sys.backend_type with
    | Bytecode -> Sys.max_array_length
    | Native -> Sys.max_unboxed_int32_array_length
    | Other _ -> assert false

  external length : ('a : bits32). 'a array -> int = "%array_length"
  external get: ('a : bits32). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : bits32). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : bits32). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : bits32). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : bits32). int -> 'a array =
    "caml_make_unboxed_int32_vect_bytecode" "caml_make_unboxed_int32_vect"
  external unsafe_blit : ('a : bits32).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_unboxed_int32_vect_blit"
  let empty () = [||]
  external to_boxed : ('a : bits32) -> (int32[@local_opt]) = "%box_int32"
  let compare_element x y = Int32.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Int32_u_array = Gen_u_array.Make (Int32_u_array0)
module Int32_u_array_boxed : Test_gen_u_array.S with type t = int32# array = Test_gen_u_array.Make_boxed (struct
  module M = Int32_u_array
  module I = Int32_I
  module E = struct
    open Stable.Int32_u
    let to_boxed x = to_int32 (x ())
    let of_boxed x () = of_int32 x
  end
end)
module _ = Test_gen_u_array.Test (Int32_u_array_boxed)


(* Extra tests for array expressions and patterns *)
module A = Int32_u_array_boxed
module I = Int32_u_array_boxed.I

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
#0l;#1l;#2l;#3l;#4l;#5l;#6l;#7l;#8l;#9l;#10l;#11l;#12l;#13l;#14l;#15l;#16l;#17l;
#18l;#19l;#20l;#21l;#22l;#23l;#24l;#25l;#26l;#27l;#28l;#29l;#30l;#31l;#32l;#33l;#34l;#35l;
#36l;#37l;#38l;#39l;#40l;#41l;#42l;#43l;#44l;#45l;#46l;#47l;#48l;#49l;#50l;#51l;#52l;#53l;
#54l;#55l;#56l;#57l;#58l;#59l;#60l;#61l;#62l;#63l;#64l;#65l;#66l;#67l;#68l;#69l;#70l;#71l;
#72l;#73l;#74l;#75l;#76l;#77l;#78l;#79l;#80l;#81l;#82l;#83l;#84l;#85l;#86l;#87l;#88l;#89l;
#90l;#91l;#92l;#93l;#94l;#95l;#96l;#97l;#98l;#99l; |]
  in
  check_i r;
  let r = [|
#0l;#1l;#2l;#3l;#4l;#5l;#6l;#7l;#8l;#9l;#10l;#11l;#12l;#13l;#14l;#15l;#16l;#17l;
#18l;#19l;#20l;#21l;#22l;#23l;#24l;#25l;#26l;#27l;#28l;#29l;#30l;#31l;#32l;#33l;#34l;#35l;
#36l;#37l;#38l;#39l;#40l;#41l;#42l;#43l;#44l;#45l;#46l;#47l;#48l;#49l;#50l;#51l;#52l;#53l;
#54l;#55l;#56l;#57l;#58l;#59l;#60l;#61l;#62l;#63l;#64l;#65l;#66l;#67l;#68l;#69l;#70l;#71l;
#72l;#73l;#74l;#75l;#76l;#77l;#78l;#79l;#80l;#81l;#82l;#83l;#84l;#85l;#86l;#87l;#88l;#89l;
#90l;#91l;#92l;#93l;#94l;#95l;#96l;#97l;#98l;#99l;#100l; |]
  in
  check_i r;
  let r = [|-#123l;-#123l;-#123l;-#123l;-#123l;-#123l;-#123l;-#123l;-#123l;-#123l;-#123l;|] in
  check_all_the_same (I.of_int (-123)) r;
  let r =
    [|-#1l; #1l; -#1l; #1l; -#1l; #1l; -#1l; #1l; -#1l;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|#1l; -#1l; #1l; -#1l; #1l; -#1l; #1l; -#1l;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
    f #0l;f #1l;f #2l;f #3l;f #4l;f #5l;f #6l;f #7l;f #8l;f #9l;f #10l;f #11l;f #12l;f #13l;f #14l;
    f #15l;f #16l;f #17l;f #18l;f #19l;f #20l;f #21l;f #22l;f #23l;f #24l;f #25l;f #26l;f #27l;f #28l;f #29l;
    f #30l;f #31l;f #32l;f #33l;f #34l;f #35l;f #36l;f #37l;f #38l;f #39l;f #40l;f #41l;f #42l;f #43l;f #44l;
    f #45l;f #46l;f #47l;f #48l;f #49l;f #50l;f #51l;f #52l;f #53l;f #54l;f #55l;f #56l;f #57l;f #58l;f #59l;
    f #60l;f #61l;f #62l;f #63l;f #64l;f #65l;f #66l;f #67l;f #68l;f #69l;f #70l;f #71l;f #72l;f #73l;f #74l;
    f #75l;f #76l;f #77l;f #78l;f #79l;f #80l;f #81l;f #82l;f #83l;f #84l;f #85l;f #86l;f #87l;f #88l;f #89l;
    f #90l;f #91l;f #92l;f #93l;f #94l;f #95l;f #96l;f #97l;f #98l;f #99l; |]
  in
  check_i r;
  let r = [|
    f #0l;f #1l;f #2l;f #3l;f #4l;f #5l;f #6l;f #7l;f #8l;f #9l;f #10l;f #11l;f #12l;f #13l;f #14l;
    f #15l;f #16l;f #17l;f #18l;f #19l;f #20l;f #21l;f #22l;f #23l;f #24l;f #25l;f #26l;f #27l;f #28l;f #29l;
    f #30l;f #31l;f #32l;f #33l;f #34l;f #35l;f #36l;f #37l;f #38l;f #39l;f #40l;f #41l;f #42l;f #43l;f #44l;
    f #45l;f #46l;f #47l;f #48l;f #49l;f #50l;f #51l;f #52l;f #53l;f #54l;f #55l;f #56l;f #57l;f #58l;f #59l;
    f #60l;f #61l;f #62l;f #63l;f #64l;f #65l;f #66l;f #67l;f #68l;f #69l;f #70l;f #71l;f #72l;f #73l;f #74l;
    f #75l;f #76l;f #77l;f #78l;f #79l;f #80l;f #81l;f #82l;f #83l;f #84l;f #85l;f #86l;f #87l;f #88l;f #89l;
    f #90l;f #91l;f #92l;f #93l;f #94l;f #95l;f #96l;f #97l;f #98l;f #99l;f #100l; |]
  in
  check_i r;
  let r =
    [|f (-#123l);f (-#123l);f (-#123l);f (-#123l);f (-#123l);f (-#123l);f (-#123l);f (-#123l);f (-#123l);|]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| #0l; ((fun x -> x) #1l)|];
  check_i [| #0l; ((fun x -> x) #1l); #2l|];
  let r =
    [|f (-#1l);f (#1l);f (-#1l);f (#1l);f (-#1l);f (#1l);f (-#1l);f (#1l);f (-#1l);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|f (#1l);f (-#1l);f (#1l);f (-#1l);f (#1l);f (-#1l);f (#1l);f (-#1l);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) = Stable.Int32_u.equal in
  (* match statement *)
  let d = [| #1l; #2l |] in
  (match d with
    | [| a; b |] ->
      assert (a = #1l);
      assert (b = #2l)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| #1l |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1l);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1l |] = #1l);
  ()
