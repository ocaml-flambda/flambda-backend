(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* CR layouts v5: eliminate various restructions about how void is used from
   this file. *)

type t_void : void

type void_rec = { v : t_void } [@@unboxed];;

external t_void : unit -> t_void = "%unbox_unit"

let void_rec () = { v = t_void () }
[%%expect{|
type t_void : void
type void_rec = { v : t_void; } [@@unboxed]
external t_void : unit -> t_void = "%unbox_unit"
val void_rec : unit -> void_rec = <fun>
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

let b' : baz = {
  a1 = void_rec ();
  a2 = void_rec ();
  x = 3;
  v = void_rec ();
  z = 42;
  b1 = void_rec ();
  b2 = void_rec ();
}

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
val id1 : baz -> baz = <fun>
val b' : baz =
  {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
   b2 = <void>}
val b' : baz =
  {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
   b2 = <void>}
- : unit = ()
|}]

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
val id1' : baz -> baz = <fun>
val b' : baz =
  {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
   b2 = <void>}
- : unit = ()
|}]

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

let magic_A = A (t_void (), void_rec (), 3, void_rec (), 42, void_rec (), t_void ())
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
val id1 : void_variant -> void_variant = <fun>
val magic_A : void_variant =
  A (<void>, <void>, 3, <void>, 42, <void>, <void>)
val magic_A : void_variant =
  A (<void>, <void>, 3, <void>, 42, <void>, <void>)
- : unit = ()
|}]

let _ = r := []
let magic_B = B (t_void ())
let magic_B = id1 magic_B
let _ = assert (List.for_all2 (=) !r [2;1]);;
[%%expect{|
- : unit = ()
val magic_B : void_variant = B <void>
val magic_B : void_variant = B <void>
- : unit = ()
|}]

let _ = r := []
let magic_C : void_variant = C (void_rec (), t_void ())
let magic_C = id1 magic_C
let _ = assert (List.for_all2 (=) !r [3;2;1]);;
[%%expect{|
- : unit = ()
val magic_C : void_variant = C (<void>, <void>)
val magic_C : void_variant = C (<void>, <void>)
- : unit = ()
|}]

let _ = r := []

let magic_D = D {
  a1 = t_void ();
  a2 = void_rec ();
  x = 3;
  v = void_rec ();
  z = 42;
  b1 = void_rec ();
  b2 = t_void ();
}
let magic_D = id1 magic_D
let _ = assert (List.for_all2 (=) !r [10;9;8;7;6;5;4;3;2;1]);;
[%%expect{|
- : unit = ()
val magic_D : void_variant =
  D
   {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
    b2 = <void>}
val magic_D : void_variant =
  D
   {a1 = <void>; a2 = <void>; x = 3; v = <void>; z = 42; b1 = <void>;
    b2 = <void>}
- : unit = ()
|}]

(******************************************)
(* Test 3: top-level void bindings banned *)

let x : t_void = assert false;;
[%%expect {|
Line 1, characters 4-5:
1 | let x : t_void = assert false;;
        ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x" has layout "void".
|}];;

module M3_1 = struct
  let x : t_void = assert false;;
end;;
[%%expect {|
Line 2, characters 6-7:
2 |   let x : t_void = assert false;;
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x" has layout "void".
|}];;

module M3_2 = struct
  let x =
    match magic_B with
    | B v -> v
    | _ -> assert false
end;;
[%%expect {|
Line 2, characters 6-7:
2 |   let x =
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x" has layout "void".
|}];;

module M3_3 = struct
  let {x} = b'

  let {z; v} = b'
end;;
[%%expect {|
Line 4, characters 10-11:
4 |   let {z; v} = b'
              ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "v" has layout "void".
|}];;

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
module M3_4 : sig end
- : unit = ()
|}]

(*************************************)
(* Test 4: Void to left of semicolon *)
let () = r := []

type void_holder = V of t_void
let vh : void_holder = V (t_void ())

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
val vh : void_holder = V <void>
val f4 : void_holder -> unit = <fun>
- : unit = ()
- : unit = ()
|}]

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
val local_void_bindings_1 : void_holder -> baz = <fun>
- : baz =
{a1 = <void>; a2 = <void>; x = 12; v = <void>; z = 13; b1 = <void>;
 b2 = <void>}
- : unit = ()
|}]

let local_void_bindings_2 b =
  let {z; a1; b1; x; b2} = b in
  (x, V b2.v, V b1.v, z, V a1.v)

let (x, _, vh2, z, _) = local_void_bindings_2 b'

let _ = assert (x = 3 && z = 42)
[%%expect{|
val local_void_bindings_2 :
  baz -> int * void_holder * void_holder * int * void_holder = <fun>
val x : int = 3
val vh2 : void_holder = V <void>
val z : int = 42
- : unit = ()
|}]

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
val local_void_bindings_3 : void_holder -> int -> int -> baz = <fun>
val x : int = 45
val z : int = 87
- : unit = ()
|}]

(**************************************************************)
(* Test 6: Compilation of exception patterns in void matches. *)
exception Ex1 of int
exception Ex2 of string
exception Ex3 of bool
[%%expect{|
exception Ex1 of int
exception Ex2 of string
exception Ex3 of bool
|}]
exception Ex4 of t_void;;
[%%expect{|
Line 1, characters 0-23:
1 | exception Ex4 of t_void;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}];;
(* CR layouts v5: once we allow non-values in extensible variants, [Ex4]
   should typecheck *)

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
val exnmatch1 : void_holder -> int = <fun>
- : unit = ()
|}]

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
val exnmatch2 : void_holder -> int = <fun>
- : unit = ()
|}]

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
val exnmatch3 : void_holder -> int = <fun>
- : unit = ()
|}]

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
val exnmatch4 : void_holder -> int = <fun>
- : unit = ()
|}]

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
Line 6, characters 21-24:
6 |      | V v -> raise (Ex4 (cons_r 2; v)));
                         ^^^
Error: This variant expression is expected to have type "exn"
       There is no constructor "Ex4" within type "exn"
Hint: Did you mean "Ex1", "Ex2" or "Ex3"?
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   we allow non-values in extensible variants.  *)
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
val make_uivr_holder : void_holder -> uivr_holder = <fun>
- : uivr_holder = {uivrh_x = 7; uivrh_v = <void>}
- : unit = ()
|}]

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
val test8 : (unit -> test8_rec) -> int * void_holder = <fun>
val x : int = 42
|}]

let () = r := []

let (x, _) = test8 (fun () -> cons_r 2; raise (Test8 (3,vh)))

let () = assert (x = 3)
let () = assert (List.for_all2 (=) !r [4;3;2;1]);;
[%%expect{|
val x : int = 3
|}]

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
Line 9, characters 22-23:
9 |   and v' = (cons_r 2; v)
                          ^
Error: This expression has type "t_void" but an expression was expected of type
         "('a : value_or_null)"
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value
         because it's the type of the recursive variable v'.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   non-values are allowed in let recs. *)
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
Line 8, characters 25-26:
8 |   let rec v1 = cons_r 1; v
                             ^
Error: This expression has type "t_void" but an expression was expected of type
         "('a : value_or_null)"
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value
         because it's the type of the recursive variable v1.
|}]
(* CR layouts v5: This was the expected behavior before removing the handling of
   void for lambda, and we expected it to be the expected behavior again after
   non-values are allowed in let recs. *)
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
