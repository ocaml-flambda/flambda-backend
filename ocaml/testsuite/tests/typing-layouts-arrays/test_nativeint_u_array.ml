(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files}";
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
(* Test compilation correctness for array of unboxed nativeints.
   General tests around type-checking should go to [basics.ml]. *)

module Nativeint_I = struct
  include Nativeint
  let max_val = max_int
  let min_val = min_int
  let rand = Random.nativeint
  let print = Format.printf "%nd"
end

module Nativeint_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = nativeint
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Nativeint_I
end
module _ = Test_gen_u_array.Test (Nativeint_array)


module Nativeint_u_array0 : Gen_u_array.S0
                            with type element_t = nativeint#
                            and type ('a : any) array_t = 'a array = struct

  type element_t = nativeint#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  external length : ('a : word). 'a array -> int = "%array_length"
  external get: ('a : word). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : word). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : word). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : word). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : word). int -> 'a array =
    "caml_make_unboxed_nativeint_vect_bytecode" "caml_make_unboxed_nativeint_vect"
  external unsafe_blit : ('a : word).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_unboxed_nativeint_vect_blit"
  let empty () = [||]
  external to_boxed : ('a : word) -> (nativeint[@local_opt]) = "%box_nativeint"
  let compare_element x y = Nativeint.compare (to_boxed (x ())) (to_boxed (y ()))
end


module Nativeint_u_array = Gen_u_array.Make (Nativeint_u_array0)
module Nativeint_u_array_boxed = Test_gen_u_array.Make_boxed (struct
  module M = Nativeint_u_array
  module I = Nativeint_I
  module E = struct
    open Stdlib__Nativeint_u
    let to_boxed x = to_nativeint (x ())
    let of_boxed x () = of_nativeint x
  end
end)
module _ = Test_gen_u_array.Test (Nativeint_u_array_boxed)



(* Extra tests for array expressions and patterns *)
module A = Nativeint_u_array_boxed
module I = Nativeint_u_array_boxed.I

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
#0n;#1n;#2n;#3n;#4n;#5n;#6n;#7n;#8n;#9n;#10n;#11n;#12n;#13n;#14n;#15n;#16n;#17n;
#18n;#19n;#20n;#21n;#22n;#23n;#24n;#25n;#26n;#27n;#28n;#29n;#30n;#31n;#32n;#33n;#34n;#35n;
#36n;#37n;#38n;#39n;#40n;#41n;#42n;#43n;#44n;#45n;#46n;#47n;#48n;#49n;#50n;#51n;#52n;#53n;
#54n;#55n;#56n;#57n;#58n;#59n;#60n;#61n;#62n;#63n;#64n;#65n;#66n;#67n;#68n;#69n;#70n;#71n;
#72n;#73n;#74n;#75n;#76n;#77n;#78n;#79n;#80n;#81n;#82n;#83n;#84n;#85n;#86n;#87n;#88n;#89n;
#90n;#91n;#92n;#93n;#94n;#95n;#96n;#97n;#98n;#99n; |]
  in
  check_i r;
  let r = [|
#0n;#1n;#2n;#3n;#4n;#5n;#6n;#7n;#8n;#9n;#10n;#11n;#12n;#13n;#14n;#15n;#16n;#17n;
#18n;#19n;#20n;#21n;#22n;#23n;#24n;#25n;#26n;#27n;#28n;#29n;#30n;#31n;#32n;#33n;#34n;#35n;
#36n;#37n;#38n;#39n;#40n;#41n;#42n;#43n;#44n;#45n;#46n;#47n;#48n;#49n;#50n;#51n;#52n;#53n;
#54n;#55n;#56n;#57n;#58n;#59n;#60n;#61n;#62n;#63n;#64n;#65n;#66n;#67n;#68n;#69n;#70n;#71n;
#72n;#73n;#74n;#75n;#76n;#77n;#78n;#79n;#80n;#81n;#82n;#83n;#84n;#85n;#86n;#87n;#88n;#89n;
#90n;#91n;#92n;#93n;#94n;#95n;#96n;#97n;#98n;#99n;#100n; |]
  in
  check_i r;
  let r = [|-#123n;-#123n;-#123n;-#123n;-#123n;-#123n;-#123n;-#123n;-#123n;-#123n;-#123n;|] in
  check_all_the_same (I.of_int (-123)) r;
  let r =
    [|-#1n; #1n; -#1n; #1n; -#1n; #1n; -#1n; #1n; -#1n;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|#1n; -#1n; #1n; -#1n; #1n; -#1n; #1n; -#1n;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
    f #0n;f #1n;f #2n;f #3n;f #4n;f #5n;f #6n;f #7n;f #8n;f #9n;f #10n;f #11n;f #12n;f #13n;f #14n;
    f #15n;f #16n;f #17n;f #18n;f #19n;f #20n;f #21n;f #22n;f #23n;f #24n;f #25n;f #26n;f #27n;f #28n;f #29n;
    f #30n;f #31n;f #32n;f #33n;f #34n;f #35n;f #36n;f #37n;f #38n;f #39n;f #40n;f #41n;f #42n;f #43n;f #44n;
    f #45n;f #46n;f #47n;f #48n;f #49n;f #50n;f #51n;f #52n;f #53n;f #54n;f #55n;f #56n;f #57n;f #58n;f #59n;
    f #60n;f #61n;f #62n;f #63n;f #64n;f #65n;f #66n;f #67n;f #68n;f #69n;f #70n;f #71n;f #72n;f #73n;f #74n;
    f #75n;f #76n;f #77n;f #78n;f #79n;f #80n;f #81n;f #82n;f #83n;f #84n;f #85n;f #86n;f #87n;f #88n;f #89n;
    f #90n;f #91n;f #92n;f #93n;f #94n;f #95n;f #96n;f #97n;f #98n;f #99n; |]
  in
  check_i r;
  let r = [|
    f #0n;f #1n;f #2n;f #3n;f #4n;f #5n;f #6n;f #7n;f #8n;f #9n;f #10n;f #11n;f #12n;f #13n;f #14n;
    f #15n;f #16n;f #17n;f #18n;f #19n;f #20n;f #21n;f #22n;f #23n;f #24n;f #25n;f #26n;f #27n;f #28n;f #29n;
    f #30n;f #31n;f #32n;f #33n;f #34n;f #35n;f #36n;f #37n;f #38n;f #39n;f #40n;f #41n;f #42n;f #43n;f #44n;
    f #45n;f #46n;f #47n;f #48n;f #49n;f #50n;f #51n;f #52n;f #53n;f #54n;f #55n;f #56n;f #57n;f #58n;f #59n;
    f #60n;f #61n;f #62n;f #63n;f #64n;f #65n;f #66n;f #67n;f #68n;f #69n;f #70n;f #71n;f #72n;f #73n;f #74n;
    f #75n;f #76n;f #77n;f #78n;f #79n;f #80n;f #81n;f #82n;f #83n;f #84n;f #85n;f #86n;f #87n;f #88n;f #89n;
    f #90n;f #91n;f #92n;f #93n;f #94n;f #95n;f #96n;f #97n;f #98n;f #99n;f #100n; |]
  in
  check_i r;
  let r =
    [|f (-#123n);f (-#123n);f (-#123n);f (-#123n);f (-#123n);f (-#123n);f (-#123n);f (-#123n);f (-#123n);|]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| #0n; ((fun x -> x) #1n)|];
  check_i [| #0n; ((fun x -> x) #1n); #2n|];
  let r =
    [|f (-#1n);f (#1n);f (-#1n);f (#1n);f (-#1n);f (#1n);f (-#1n);f (#1n);f (-#1n);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|f (#1n);f (-#1n);f (#1n);f (-#1n);f (#1n);f (-#1n);f (#1n);f (-#1n);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) = Stdlib__Nativeint_u.equal in
  (* match statement *)
  let d = [| #1n; #2n |] in
  (match d with
    | [| a; b |] ->
      assert (a = #1n);
      assert (b = #2n)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| #1n |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1n);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1n |] = #1n);
  ()
