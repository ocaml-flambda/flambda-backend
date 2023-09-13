(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)
(* CR layouts v5: when we add the ability to make actual void types, eliminate
   the uses of Obj.magic from this file *)

(* CR layouts v5: eliminate various restructions about how void is used from
   this file. *)

type t_void : void

type void_rec = { v : t_void } [@@unboxed];;
[%%expect{|
type t_void : void
type void_rec = { v : t_void; } [@@unboxed]
|}]

(**************************************************)
(* Test 1: Evaluation order of records with voids *)
type baz = { a1 : void_rec;
             a2 : void_rec;
             x : int;
             v : void_rec;
             z : int;
             b1 : void_rec;
             b2 : void_rec}

let r = ref []

let cons_r x = r := x :: !r

let id1 {a1; a2; x; v; z; b1; b2} =
  {a1 = (cons_r 11; {v = ((cons_r 12; a1).v)});
   a2 = (cons_r 9; {v = ((cons_r 10; a2).v)});
   x = (cons_r 8; x);
   v = (cons_r 6; {v = ((cons_r 7; v).v)});
   z = (cons_r 5; z);
   b1 = (cons_r 3; {v = ((cons_r 4; b1).v)});
   b2 = (cons_r 1; {v = ((cons_r 2; b2).v)});
  }

type bar = { x' : int; z' : int }

let b : bar = { x' = 3; z' = 42 }

let b' : baz = Obj.magic b

let b' = id1 b'

let _ = assert (List.for_all2 (=) !r [12;11;10;9;8;7;6;5;4;3;2;1]);;

[%%expect{|
type baz = {
  a1 : void_rec;
  a2 : void_rec;
  x : int;
  v : void_rec;
  z : int;
  b1 : void_rec;
  b2 : void_rec;
}
val r : '_weak1 list ref = {contents = []}
val cons_r : '_weak1 -> unit = <fun>
Lines 14-21, characters 2-3:
14 | ..{a1 = (cons_r 11; {v = ((cons_r 12; a1).v)});
15 |    a2 = (cons_r 9; {v = ((cons_r 10; a2).v)});
16 |    x = (cons_r 8; x);
17 |    v = (cons_r 6; {v = ((cons_r 7; v).v)});
18 |    z = (cons_r 5; z);
19 |    b1 = (cons_r 3; {v = ((cons_r 4; b1).v)});
20 |    b2 = (cons_r 1; {v = ((cons_r 2; b2).v)});
21 |   }
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of void_rec is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of void_rec must be a sublayout of value, because
         it has to be value for the V1 safety check.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * type baz = {
 *   a1 : void_rec;
 *   a2 : void_rec;
 *   x : int;
 *   v : void_rec;
 *   z : int;
 *   b1 : void_rec;
 *   b2 : void_rec;
 * }
 * val r : '_weak1 list ref = {contents = []}
 * val cons_r : '_weak1 -> unit = <fun>
 * val id1 : baz -> baz = <fun>
 * type bar = { x' : int; z' : int; }
 * val b : bar = {x' = 3; z' = 42}
 * val b' : baz =
 *   {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
 *    b2 = <void>}
 * val b' : baz =
 *   {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
 *    b2 = <void>}
 * - : unit = ()
 * |}];; *)

(* Same thing, but showing that it's the order of the declaration that matters
   *)

let () = r := []

let id1' {a1; a2; x; v; z; b1; b2} =
  {a2 = (cons_r 9; {v = ((cons_r 10; a2).v)});
   b2 = (cons_r 1; {v = ((cons_r 2; b2).v)});
   x = (cons_r 8; x);
   a1 = (cons_r 11; {v = ((cons_r 12; a1).v)});
   z = (cons_r 5; z);
   b1 = (cons_r 3; {v = ((cons_r 4; b1).v)});
   v = (cons_r 6; {v = ((cons_r 7; v).v)});
  }

let b' = id1' b'

let _ = assert (List.for_all2 (=) !r [12;11;10;9;8;7;6;5;4;3;2;1]);;
[%%expect{|
Lines 4-11, characters 2-3:
 4 | ..{a2 = (cons_r 9; {v = ((cons_r 10; a2).v)});
 5 |    b2 = (cons_r 1; {v = ((cons_r 2; b2).v)});
 6 |    x = (cons_r 8; x);
 7 |    a1 = (cons_r 11; {v = ((cons_r 12; a1).v)});
 8 |    z = (cons_r 5; z);
 9 |    b1 = (cons_r 3; {v = ((cons_r 4; b1).v)});
10 |    v = (cons_r 6; {v = ((cons_r 7; v).v)});
11 |   }
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of void_rec is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of void_rec must be a sublayout of value, because
         it has to be value for the V1 safety check.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val id1' : baz -> baz = <fun>
 * val b' : baz =
 *   {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
 *    b2 = <void>}
 * - : unit = ()
 * |}];; *)

(***************************************************)
(* Test 2: evaluation order of variants with voids *)
type void_variant =
    A of t_void * void_rec * int * void_rec * int * void_rec * t_void
  | B of t_void
  | C of void_rec * t_void
  | D of { a1 : t_void;
           a2 : void_rec;
           x : int;
           v : void_rec;
           z : int;
           b1 : void_rec;
           b2 : t_void }

let r = ref []

let cons_r x = r := x :: !r

let id1 = function
  | A (a1, a2, x, v, z, b1, b2) ->
     A ((cons_r 10; a1),
        (cons_r 8; {v = ((cons_r 9; a2).v)}),
        (cons_r 7; x),
        (cons_r 5; {v = ((cons_r 6; v).v)}),
        (cons_r 4; z),
        (cons_r 2; {v = ((cons_r 3; b1).v)}),
        (cons_r 1; b2))
  | B v -> cons_r 1; B (cons_r 2; v)
  | C (vr,v) -> cons_r 1; C ({v = (cons_r 3; vr).v}, (cons_r 2; v))
  | D {a1; a2; x; v; z; b1; b2} ->
    D {a1 = (cons_r 10; a1);
       a2 = (cons_r 8; {v = ((cons_r 9; a2).v)});
       x = (cons_r 7; x);
       v = (cons_r 5; {v = ((cons_r 6; v).v)});
       z = (cons_r 4; z);
       b1 = (cons_r 2; {v = ((cons_r 3; b1).v)});
       b2 = (cons_r 1; b2)}

type for_magic =
  | MA of int * int
  | MB
  | MC
  | MD of { x' : int;
            z' : int }

let magic_A : void_variant = Obj.magic (MA (3,42))
let magic_A = id1 magic_A

let _ = assert (List.for_all2 (=) !r [10;9;8;7;6;5;4;3;2;1]);;

[%%expect{|
type void_variant =
    A of t_void * void_rec * int * void_rec * int * void_rec * t_void
  | B of t_void
  | C of void_rec * t_void
  | D of { a1 : t_void; a2 : void_rec; x : int; v : void_rec; z : int;
      b1 : void_rec; b2 : t_void;
    }
val r : '_weak2 list ref = {contents = []}
val cons_r : '_weak2 -> unit = <fun>
Lines 19-25, characters 5-23:
19 | .....A ((cons_r 10; a1),
20 |         (cons_r 8; {v = ((cons_r 9; a2).v)}),
21 |         (cons_r 7; x),
22 |         (cons_r 5; {v = ((cons_r 6; v).v)}),
23 |         (cons_r 4; z),
24 |         (cons_r 2; {v = ((cons_r 3; b1).v)}),
25 |         (cons_r 1; b2))
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * type void_variant =
 *     A of t_void * void_rec * int * void_rec * int * void_rec * t_void
 *   | B of t_void
 *   | C of void_rec * t_void
 *   | D of { a1 : t_void; a2 : void_rec; x : int; v : void_rec; z : int;
 *       b1 : void_rec; b2 : t_void;
 *     }
 * val r : '_weak2 list ref = {contents = []}
 * val cons_r : '_weak2 -> unit = <fun>
 * val id1 : void_variant -> void_variant = <fun>
 * type for_magic = MA of int * int | MB | MC | MD of { x' : int; z' : int; }
 * val magic_A : void_variant =
 *   A (<void>, <void>, 3, <void>, 42, <void>, <void>)
 * val magic_A : void_variant =
 *   A (<void>, <void>, 3, <void>, 42, <void>, <void>)
 * - : unit = ()
 * |}] *)

let _ = r := []
let magic_B : void_variant = Obj.magic MB
let magic_B = id1 magic_B
let _ = assert (List.for_all2 (=) !r [2;1]);;
[%%expect{|
- : unit = ()
Line 2, characters 39-41:
2 | let magic_B : void_variant = Obj.magic MB
                                           ^^
Error: Unbound constructor MB
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * - : unit = ()
 * val magic_B : void_variant = B <void>
 * val magic_B : void_variant = B <void>
 * - : unit = ()
 * |}];; *)

let _ = r := []
let magic_C : void_variant = Obj.magic MC
let magic_C = id1 magic_C
let _ = assert (List.for_all2 (=) !r [3;2;1]);;
[%%expect{|
- : unit = ()
Line 2, characters 39-41:
2 | let magic_C : void_variant = Obj.magic MC
                                           ^^
Error: Unbound constructor MC
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * - : unit = ()
 * val magic_C : void_variant = C (<void>, <void>)
 * val magic_C : void_variant = C (<void>, <void>)
 * - : unit = ()
 * |}];; *)

let _ = r := []
let magic_D : void_variant = Obj.magic (MD {x' = 3; z' = 42})
let magic_D = id1 magic_D
let _ = assert (List.for_all2 (=) !r [10;9;8;7;6;5;4;3;2;1]);;
[%%expect{|
- : unit = ()
Line 2, characters 40-42:
2 | let magic_D : void_variant = Obj.magic (MD {x' = 3; z' = 42})
                                            ^^
Error: Unbound constructor MD
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * - : unit = ()
 * val magic_D : void_variant =
 *   D
 *    {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
 *     b2 = <void>}
 * val magic_D : void_variant =
 *   D
 *    {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
 *     b2 = <void>}
 * - : unit = ()
 * |}];; *)

(******************************************)
(* Test 3: top-level void bindings banned *)

let x : t_void = assert false;;
[%%expect {|
Line 1, characters 4-5:
1 | let x : t_void = assert false;;
        ^
Error: Types of top-level module bindings must have layout value, but
       the type of x has layout void.
|}];;

module M3_1 = struct
  let x : t_void = assert false;;
end;;
[%%expect {|
Line 2, characters 6-7:
2 |   let x : t_void = assert false;;
          ^
Error: Types of top-level module bindings must have layout value, but
       the type of x has layout void.
|}];;

module M3_2 = struct
  let x =
    match magic_B with
    | B v -> v
    | _ -> assert false
end;;
[%%expect {|
Line 3, characters 10-17:
3 |     match magic_B with
              ^^^^^^^
Error: Unbound value magic_B
|}];;
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * Line 2, characters 6-7:
 * 2 |   let x =
 *           ^
 * Error: Top-level module bindings must have layout value, but x has layout
 *        void.
 * |}];; *)

module M3_3 = struct
  let {x} = b'

  let {z; v} = b'
end;;
[%%expect {|
Line 2, characters 12-14:
2 |   let {x} = b'
                ^^
Error: Unbound value b'
|}];;
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * Line 4, characters 10-11:
 * 4 |   let {z; v} = b'
 *               ^
 * Error: Top-level module bindings must have layout value, but v has layout
 *        void.
 * |}];; *)

let () = r := []
module M3_4 = struct
  (* But it's fine if you don't bind it *)
  let _ =
    match cons_r 1; magic_B with
    | B v -> cons_r 2; v
    | _ -> assert false
end;;
let _ = assert (List.for_all2 (=) !r [2;1]);;
[%%expect{|
Line 5, characters 20-27:
5 |     match cons_r 1; magic_B with
                        ^^^^^^^
Error: Unbound value magic_B
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * module M3_4 : sig end
 * - : unit = ()
 * |}];; *)

(*************************************)
(* Test 4: Void to left of semicolon *)
let () = r := []

type void_holder = V of t_void
type vh_formagic = VM
let vh : void_holder = Obj.magic VM

let [@warning "-10"] f4 (V v) =
  v;
  cons_r 1;
  (cons_r 2; { v = (cons_r 3; v) });
  cons_r 4;
  (cons_r 5; v);
  cons_r 6

let _ = f4 vh

let _ = assert (List.for_all2 (=) !r [6;5;4;3;2;1]);;
[%%expect{|
type void_holder = V of t_void
type vh_formagic = VM
Line 5, characters 4-6:
5 | let vh : void_holder = Obj.magic VM
        ^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * type void_holder = V of t_void
 * type vh_formagic = VM
 * val vh : void_holder = V <void>
 * val f4 : void_holder -> unit = <fun>
 * - : unit = ()
 * - : unit = ()
 * |}];; *)

(**********************************************************************)
(* Test 5: local binding of void things is allowed and works sensibly *)
let () = r := []

let local_void_bindings_1 vh =
  let V v = cons_r 1; vh in
  {a1 = {v = (cons_r 8; v)};
   a2 = {v = (cons_r 7; v)};
   x = (cons_r 6; 12);
   v = (cons_r 5; {v});
   z = (cons_r 4; 13);
   b1 = {v = (cons_r 3; v)};
   b2 = (cons_r 2; {v})}

let _ = local_void_bindings_1 vh

let _ = assert (List.for_all2 (=) !r [8;7;6;5;4;3;2;1]);;
[%%expect{|
Lines 4-11, characters 2-24:
 4 | ..let V v = cons_r 1; vh in
 5 |   {a1 = {v = (cons_r 8; v)};
 6 |    a2 = {v = (cons_r 7; v)};
 7 |    x = (cons_r 6; 12);
 8 |    v = (cons_r 5; {v});
 9 |    z = (cons_r 4; 13);
10 |    b1 = {v = (cons_r 3; v)};
11 |    b2 = (cons_r 2; {v})}
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
<<<<<<< HEAD
       void_rec has layout void, which is not a sublayout of value.
||||||| parent of c77b6af8 (Enable layout histories (#1823))
       t_void has layout void, which is not a sublayout of value.
=======
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
>>>>>>> c77b6af8 (Enable layout histories (#1823))
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * val local_void_bindings_1 : void_holder -> baz = <fun>
 * - : baz =
 * {a1 = <void>; a2 = <void>; x = 12; v = <void>; z = 13; b1 = <void>;
 *  b2 = <void>}
 * - : unit = ()
 * |}] *)

let local_void_bindings_2 b =
  let {z; a1; b1; x; b2} = b in
  (x, V b2.v, V b1.v, z, V a1.v)

let (x, _, vh2, z, _) = local_void_bindings_2 b'

let _ = assert (x = 3 && z = 42)
[%%expect{|
Lines 2-3, characters 2-32:
2 | ..let {z; a1; b1; x; b2} = b in
3 |   (x, V b2.v, V b1.v, z, V a1.v)
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
<<<<<<< HEAD
       t_void has layout void, which is not a sublayout of value.
||||||| parent of c77b6af8 (Enable layout histories (#1823))
       void_rec has layout void, which is not a sublayout of value.
=======
       The layout of void_rec is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of void_rec must be a sublayout of value, because
         it has to be value for the V1 safety check.
>>>>>>> c77b6af8 (Enable layout histories (#1823))
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect {|
 * val local_void_bindings_2 :
 *   baz -> int * void_holder * void_holder * int * void_holder = <fun>
 * val x : int = 3
 * val vh2 : void_holder = V <void>
 * val z : int = 42
 * - : unit = ()
 * |}] *)

let () = r := []

let local_void_bindings_3 vh1 x y =
  let v1 =
    cons_r 1;
    match vh1 with
    | V v -> v
  in
  let x = cons_r 2; x + y in
  let v2 =
    cons_r 3;
    let _ =
      match {v = v1} with
      | {v} -> cons_r 4; v
    in
    match vh2 with
    | V v -> cons_r 5; v
  in
  let vr = {v = (cons_r 6; v2)} in
  let {v = v3} : void_rec = cons_r 7; vr in
  let z = cons_r 8; y + x in
  cons_r 9;
  {a1 = {v = v1};
   a2 = {v = let V v = vh in v};
   x;
   v = {v = v2};
   z = z;
   b1 = vr;
   b2 = {v = v3}}

let {x;z} = local_void_bindings_3 vh 3 42

let () =
  assert (x = 45 && z = 87)

let _ = assert (List.for_all2 (=) !r [9;8;7;6;5;4;3;2;1]);;
[%%expect{|
Line 16, characters 10-13:
16 |     match vh2 with
               ^^^
Error: Unbound value vh2
Hint: Did you mean vh1?
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val local_void_bindings_3 : void_holder -> int -> int -> baz = <fun>
 * val x : int = 45
 * val z : int = 87
 * - : unit = ()
 * |}];; *)

(**************************************************************)
(* Test 6: Compilation of exception patterns in void matches. *)
exception Ex1 of int
exception Ex2 of string
exception Ex3 of bool
exception Ex4 of t_void;;
[%%expect{|
exception Ex1 of int
exception Ex2 of string
exception Ex3 of bool
exception Ex4 of t_void
|}];;

let [@warning "-10"] exnmatch1 (V v) =
  match
    {v = (if true then raise (Ex1 42); v)};
    if true then raise (Ex2 "test");
    {v = ((if true then raise (Ex3 true)); v)}
  with
  | {v} -> 0
  | exception Ex1 42 -> 1
  | exception Ex1 _ -> 2
  | exception Ex2 "test" -> 3
  | exception Ex2 _ -> 4
  | exception Ex3 true -> 5
  | exception Ex3 _ -> 6

let _ = assert ((exnmatch1 vh) = 1);;
[%%expect{|
<<<<<<< HEAD
Lines 3-5, characters 4-46:
3 | ....{v = (if true then raise (Ex1 42); v)};
4 |     if true then raise (Ex2 "test");
5 |     {v = ((if true then raise (Ex3 true)); v)}
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       void_rec. Please report this error to the Jane Street compilers team.
||||||| parent of c77b6af8 (Enable layout histories (#1823))
Lines 1-13, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = (if true then raise (Ex1 42); v)};
 4 |     if true then raise (Ex2 "test");
 5 |     {v = ((if true then raise (Ex3 true)); v)}
...
10 |   | exception Ex2 "test" -> 3
11 |   | exception Ex2 _ -> 4
12 |   | exception Ex3 true -> 5
13 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       t_void has layout void, which is not a sublayout of value.
=======
Lines 1-13, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = (if true then raise (Ex1 42); v)};
 4 |     if true then raise (Ex2 "test");
 5 |     {v = ((if true then raise (Ex3 true)); v)}
...
10 |   | exception Ex2 "test" -> 3
11 |   | exception Ex2 _ -> 4
12 |   | exception Ex3 true -> 5
13 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
>>>>>>> c77b6af8 (Enable layout histories (#1823))
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val exnmatch1 : void_holder -> int = <fun>
 * - : unit = ()
 * |}];; *)

let [@warning "-10"] exnmatch2 (V v) =
  match
    {v = v};
    if true then raise (Ex2 "test");
    {v = ((if true then raise (Ex3 true)); v)}
  with
  | {v} -> 0
  | exception Ex1 42 -> 1
  | exception Ex1 _ -> 2
  | exception Ex2 "test" -> 3
  | exception Ex2 _ -> 4
  | exception Ex3 true -> 5
  | exception Ex3 _ -> 6

let _ = assert ((exnmatch2 vh) = 3);;
[%%expect{|
<<<<<<< HEAD
Lines 3-5, characters 4-46:
3 | ....{v = v};
4 |     if true then raise (Ex2 "test");
5 |     {v = ((if true then raise (Ex3 true)); v)}
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       void_rec. Please report this error to the Jane Street compilers team.
||||||| parent of c77b6af8 (Enable layout histories (#1823))
Lines 1-13, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = v};
 4 |     if true then raise (Ex2 "test");
 5 |     {v = ((if true then raise (Ex3 true)); v)}
...
10 |   | exception Ex2 "test" -> 3
11 |   | exception Ex2 _ -> 4
12 |   | exception Ex3 true -> 5
13 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       t_void has layout void, which is not a sublayout of value.
=======
Lines 1-13, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = v};
 4 |     if true then raise (Ex2 "test");
 5 |     {v = ((if true then raise (Ex3 true)); v)}
...
10 |   | exception Ex2 "test" -> 3
11 |   | exception Ex2 _ -> 4
12 |   | exception Ex3 true -> 5
13 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
>>>>>>> c77b6af8 (Enable layout histories (#1823))
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val exnmatch2 : void_holder -> int = <fun>
 * - : unit = ()
 * |}];; *)

let [@warning "-10"] exnmatch3 (V v) =
  match
    {v = v};
    {v = ((if true then raise (Ex3 true)); v)}
  with
  | {v} -> 0
  | exception Ex1 42 -> 1
  | exception Ex1 _ -> 2
  | exception Ex2 "test" -> 3
  | exception Ex2 _ -> 4
  | exception Ex3 true -> 5
  | exception Ex3 _ -> 6

let _ = assert ((exnmatch3 vh) = 5);;
[%%expect{|
<<<<<<< HEAD
Lines 3-4, characters 4-46:
3 | ....{v = v};
4 |     {v = ((if true then raise (Ex3 true)); v)}
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       void_rec. Please report this error to the Jane Street compilers team.
||||||| parent of c77b6af8 (Enable layout histories (#1823))
Lines 1-12, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = v};
 4 |     {v = ((if true then raise (Ex3 true)); v)}
 5 |   with
...
 9 |   | exception Ex2 "test" -> 3
10 |   | exception Ex2 _ -> 4
11 |   | exception Ex3 true -> 5
12 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       t_void has layout void, which is not a sublayout of value.
=======
Lines 1-12, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = v};
 4 |     {v = ((if true then raise (Ex3 true)); v)}
 5 |   with
...
 9 |   | exception Ex2 "test" -> 3
10 |   | exception Ex2 _ -> 4
11 |   | exception Ex3 true -> 5
12 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
>>>>>>> c77b6af8 (Enable layout histories (#1823))
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val exnmatch3 : void_holder -> int = <fun>
 * - : unit = ()
 * |}];; *)

let [@warning "-10"] exnmatch4 (V v) =
  match
    {v = v};
    {v = v}
  with
  | {v} -> 0
  | exception Ex1 42 -> 1
  | exception Ex1 _ -> 2
  | exception Ex2 "test" -> 3
  | exception Ex2 _ -> 4
  | exception Ex3 true -> 5
  | exception Ex3 _ -> 6

let _ = assert ((exnmatch4 vh) = 0);;
[%%expect{|
<<<<<<< HEAD
Lines 3-4, characters 4-11:
3 | ....{v = v};
4 |     {v = v}
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       void_rec. Please report this error to the Jane Street compilers team.
||||||| parent of c77b6af8 (Enable layout histories (#1823))
Lines 1-12, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = v};
 4 |     {v = v}
 5 |   with
...
 9 |   | exception Ex2 "test" -> 3
10 |   | exception Ex2 _ -> 4
11 |   | exception Ex3 true -> 5
12 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       t_void has layout void, which is not a sublayout of value.
=======
Lines 1-12, characters 31-24:
 1 | ...............................(V v) =
 2 |   match
 3 |     {v = v};
 4 |     {v = v}
 5 |   with
...
 9 |   | exception Ex2 "test" -> 3
10 |   | exception Ex2 _ -> 4
11 |   | exception Ex3 true -> 5
12 |   | exception Ex3 _ -> 6
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
>>>>>>> c77b6af8 (Enable layout histories (#1823))
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val exnmatch4 : void_holder -> int = <fun>
 * - : unit = ()
 * |}];; *)

let () = r := []
let [@warning "-10-21"] exnmatch5 (V v) =
  match
    {v = (cons_r 1; v)};
    (match vh with
     | V v -> raise (Ex4 (cons_r 2; v)));
    {v = (cons_r 99; v)}
  with
  | {v} -> V (cons_r 98; v)
  | exception Ex4 v -> V (cons_r 3; v)

let _ = exnmatch5 vh
let l = !r
let _ = assert (List.for_all2 (=) l [3;2;1]);;
[%%expect{|
Line 5, characters 11-13:
5 |     (match vh with
               ^^
Error: Unbound value vh
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val exnmatch5 : void_holder -> void_holder = <fun>
 * - : void_holder = V <void>
 * val l : int list = [3; 2; 1]
 * - : unit = ()
 * |}];; *)

(*******************************************************)
(* Test 7: compilation of unboxed inlined void records *)
let () = r := []

type unboxed_inlined_void_rec =
  | UIVR of { uivr_v : t_void } [@@unboxed]

type uivr_holder = {uivrh_x : int; uivrh_v : unboxed_inlined_void_rec }

let make_uivr_holder vh =
  let uivrh =
    cons_r 1;
    match cons_r 2; vh with
    | V v -> begin
        cons_r 3;
        { uivrh_x = (cons_r 6; 7);
          uivrh_v = (cons_r 4; UIVR { uivr_v = (cons_r 5; v) }) }
      end
  in
  cons_r uivrh.uivrh_x; uivrh

let _ = make_uivr_holder vh
let _ = assert (List.for_all2 (=) !r [7;6;5;4;3;2;1]);;
[%%expect{|
type unboxed_inlined_void_rec = UIVR of { uivr_v : t_void; } [@@unboxed]
type uivr_holder = { uivrh_x : int; uivrh_v : unboxed_inlined_void_rec; }
Lines 9-18, characters 2-29:
 9 | ..let uivrh =
10 |     cons_r 1;
11 |     match cons_r 2; vh with
12 |     | V v -> begin
13 |         cons_r 3;
14 |         { uivrh_x = (cons_r 6; 7);
15 |           uivrh_v = (cons_r 4; UIVR { uivr_v = (cons_r 5; v) }) }
16 |       end
17 |   in
18 |   cons_r uivrh.uivrh_x; uivrh
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
<<<<<<< HEAD
       unboxed_inlined_void_rec has layout void,
         which is not a sublayout of value.
||||||| parent of c77b6af8 (Enable layout histories (#1823))
       t_void has layout void, which is not a sublayout of value.
=======
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
>>>>>>> c77b6af8 (Enable layout histories (#1823))
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * type unboxed_inlined_void_rec = UIVR of { uivr_v : t_void; } [@@unboxed]
 * type uivr_holder = { uivrh_x : int; uivrh_v : unboxed_inlined_void_rec; }
 * val make_uivr_holder : void_holder -> uivr_holder = <fun>
 * - : uivr_holder = {uivrh_x = 7; uivrh_v = <void>}
 * - : unit = ()
 * |}] *)

(*****************************************************************************)
(* Test 8: void bindings in or patterns that include both normal and exception
   patterns *)
exception Test8 of int * void_holder

type test8_rec = {t8_x : int; t8_v : t_void}

let test8 (f : unit -> test8_rec) : int * void_holder =
  match cons_r 1; f () with
  | ({t8_x = x; t8_v = v} | exception (Test8 (x, V v))) ->
    begin
      cons_r 3;
      x, V (cons_r 4; v)
    end

let () = r := []

let (x, _) = test8 (fun () -> let V v = vh in cons_r 2; {t8_x = 42; t8_v = v})

let () = assert (x = 42)
let () = assert (List.for_all2 (=) !r [4;3;2;1]);;
[%%expect{|
exception Test8 of int * void_holder
type test8_rec = { t8_x : int; t8_v : t_void; }
Lines 6-11, characters 2-7:
 6 | ..match cons_r 1; f () with
 7 |   | ({t8_x = x; t8_v = v} | exception (Test8 (x, V v))) ->
 8 |     begin
 9 |       cons_r 3;
10 |       x, V (cons_r 4; v)
11 |     end
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * exception Test8 of int * void_holder
 * type test8_rec = { t8_x : int; t8_v : t_void; }
 * val test8 : (unit -> test8_rec) -> int * void_holder = <fun>
 * val x : int = 42
 * |}];; *)

let () = r := []

let (x, _) = test8 (fun () -> cons_r 2; raise (Test8 (3,vh)))

let () = assert (x = 3)
let () = assert (List.for_all2 (=) !r [4;3;2;1]);;
[%%expect{|
Line 3, characters 13-18:
3 | let (x, _) = test8 (fun () -> cons_r 2; raise (Test8 (3,vh)))
                 ^^^^^
Error: Unbound value test8
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val x : int = 3
 * |}];; *)

(***********************************)
(* Test 9: voids in let rec groups *)
(* CR layouts v2.5: hard to do much interesting here, considering the restrictions
   on non-function let-recs.  Revisit when we have more interesting functions?
*)
let () = r := []

let let_rec_of_void_1 vh x =
  let v = match vh with
    | V v -> v
  in
  (* not all void *)
  let rec y = (cons_r 1; x)
  and v' = (cons_r 2; v)
  in
  (y, V v')

let (x, _) = let_rec_of_void_1 vh 42

let () = assert (x = 42);;
let () = assert (List.for_all2 (=) !r [2;1]);;

[%%expect{|
Lines 4-11, characters 2-11:
 4 | ..let v = match vh with
 5 |     | V v -> v
 6 |   in
 7 |   (* not all void *)
 8 |   let rec y = (cons_r 1; x)
 9 |   and v' = (cons_r 2; v)
10 |   in
11 |   (y, V v')
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val let_rec_of_void_1 : void_holder -> 'a -> 'a * void_holder = <fun>
 * val x : int = 42
 * |}];; *)

let () = r := []

let let_rec_of_void_2 vh x =
  let v = match vh with
    | V v -> v
  in
  (* all void *)
  let rec v1 = cons_r 1; v
  and v2 = cons_r 2; v
  and v3 = cons_r 3; v
  in
  (x, V v1, V v2, V v3)

let (x, _, _, _) = let_rec_of_void_2 vh 42

let () = assert (x = 42);;
let () = assert (List.for_all2 (=) !r [3;2;1]);;

[%%expect{|
Lines 4-12, characters 2-23:
 4 | ..let v = match vh with
 5 |     | V v -> v
 6 |   in
 7 |   (* all void *)
 8 |   let rec v1 = cons_r 1; v
 9 |   and v2 = cons_r 2; v
10 |   and v3 = cons_r 3; v
11 |   in
12 |   (x, V v1, V v2, V v3)
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void, because
         of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         it has to be value for the V1 safety check.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   void is handled properly.  *)
(* [%%expect{|
 * val let_rec_of_void_2 :
 *   void_holder -> 'a -> 'a * void_holder * void_holder * void_holder = <fun>
 * val x : int = 42
 * |}];; *)



(* CR layouts v5: When we allow void at the module level, we'll want
   test cases, including cases where the term has an unrepresentable
   layout, and the signature a) mentions the term and specifies its layout, b)
   mentions the term and doesn't specify its layout, and c) doesn't mention the
   term. Do we want to allow "empty" modules?
*)
