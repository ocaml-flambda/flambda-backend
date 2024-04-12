<<<<<<< HEAD
(* TEST_BELOW
(* Blank lines added here to preserve locations. *)






=======
(* TEST

flags = "-w +A-60-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output
>>>>>>> fb471efa (Parse `zero_alloc` attributes into the typed tree (#2400))

*)

(* [@@@zero_alloc all] affects the meaning of other zero_alloc attributes.  This
   test, which is a copy of the zero_alloc bits of the main w53.ml, shows that
   it doesn't affect their w53 behavior. *)

[@@@zero_alloc all]

module type TestZeroAllocSig = sig
  type 'a t1 = 'a [@@zero_alloc] (* rejected *)
  type s1 = Foo1 [@zero_alloc] (* rejected *)
  val f : int -> int [@@zero_alloc] (* accepted *)

  external y : (int [@zero_alloc]) -> (int [@zero_alloc]) = "x" (* rejected *)
  external z : int -> int = "x" "y" [@@zero_alloc] (* rejected *)
  external[@zero_alloc] q : int -> int = "x" "y" (* rejected *)

  class[@zero_alloc] c : (* rejected *)
    object
      val[@zero_alloc] foo : int * int (* rejected *)
      val[@zero_alloc] bar : int -> int (* rejected *)
      method[@zero_alloc] baz : int * int (* rejected *)
      method[@zero_alloc] boz : int -> int (* rejected *)
    end
end

module TestZeroAllocStruct = struct
  type 'a t1 = 'a [@@zero_alloc] (* rejected *)
  type s1 = Foo1 [@zero_alloc] (* rejected *)
  let x : int = 42 [@@zero_alloc] (* rejected *)

  let[@zero_alloc] w = 42 (* rejected *)

  let[@zero_alloc] f x = x (* accepted *)

  external y : (int [@zero_alloc]) -> (int [@zero_alloc]) = "x" (* rejected *)
  external z : int -> int = "x" "y" [@@zero_alloc] (* rejected *)
  external[@zero_alloc] q : int -> int = "x" "y" (* rejected *)

  class[@zero_alloc] foo _y = (* rejected *)
    let[@inline never][@zero_alloc] f x = (x, x) in (* accepted *)
    (fun[@zero_alloc] z -> (* rejected *)
    object
      val[@zero_alloc] bar = (4, 5) (* rejected *)

      method[@zero_alloc] baz x = (f (z+10), x+1) (* rejected *)
    end)

  let[@zero_alloc] f1 = fun x y -> (x,y) (* accepted *)
  let f2 = fun [@zero_alloc] x y -> (x,y) (* accepted *)

  let[@zero_alloc ignore] f3 = fun x y -> (x,y) (* accepted *)
  let f4 = fun [@zero_alloc ignore] x y -> (x,y) (* accepted *)

  (* assume on calls goes on the function being called *)
  let[@inline never] boz x = (x,x)
  let[@zero_alloc] fiz x =
    ((boz x)[@zero_alloc assume]) (* rejected *)
  let[@zero_alloc] fuz x =
    ((boz[@zero_alloc assume]) x) (* accepted *)

  (* Triggers w53 on non-function lets *)
  let[@zero_alloc assume] foo = (* rejected *)
    let x = 42 in
    fun z -> z + x

  let[@zero_alloc] bar = (* rejected *)
    let x = 42 in
    fun z -> z + x
end
<<<<<<< HEAD

(* TEST
 flags = "-w +A-60-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
=======
>>>>>>> fb471efa (Parse `zero_alloc` attributes into the typed tree (#2400))
