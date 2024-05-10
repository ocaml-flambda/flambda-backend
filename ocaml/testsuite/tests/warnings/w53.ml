(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

let h x = x [@inline] (* rejected *)
let h x = x [@ocaml.inline] (* rejected *)

let i x = x [@inlined] (* rejected *)
let j x = x [@ocaml.inlined] (* rejected *)
let k x = (h [@inlined]) x (* accepted *)
let k' x = (h [@ocaml.inlined]) x (* accepted *)
let l x = h x [@inlined] (* rejected *)

let m x = x [@tailcall] (* rejected *)
let n x = x [@ocaml.tailcall] (* rejected *)
let o x = (h [@tailcall]) x (* accepted *)
let p x = (h [@ocaml.tailcall]) x (* accepted *)
let q x = h x [@tailcall] (* rejected *)

module type E = sig end

module A(E:E) = struct end [@@inline] (* accepted *)
module A'(E:E) = struct end [@@ocaml.inline] (* accepted *)
module B = ((functor (E:E) -> struct end) [@inline]) (* accepted *)
module B' = ((functor (E:E) -> struct end) [@ocaml.inline]) (* accepted *)
module C = struct end [@@inline] (* rejected *)
module C' = struct end [@@ocaml.inline] (* rejected *)
module D = struct end [@@inlined] (* rejected *)
module D' = struct end [@@ocaml.inlined] (* rejected *)

module F = (A [@inlined])(struct end) (* accepted *)
module F' = (A [@ocaml.inlined])(struct end) (* accepted *)
module G = (A [@inline])(struct end) (* rejected *)
module G' = (A [@ocaml.inline])(struct end) (* rejected *)

module H = Set.Make [@inlined] (Int32) (* GPR#1808 *)

module I = Set.Make [@inlined]
module I' = Set.Make [@ocaml.inlined]

module J = Set.Make [@@inlined]
module J' = Set.Make [@@ocaml.inlined]

module type K = sig
  val a1 : int [@deprecated]   (* rejected *)
  val a2 : int [@@deprecated]  (* accepted *)
  [@@@deprecated] (* accepted*)
end

let [@unrolled 42] rec test_unrolled x = (* rejected *)
  match x with
  | 0 -> ()
  | n -> test_unrolled (n - 1)

let () = (test_unrolled [@unrolled 42]) 10 (* accepted *)

let test_ppwarning = 42 [@@ppwarning "warning"]
  (* accepted (but issues its own warning *)

type test_literal_pattern =
  | Lit_pat1 of int [@warn_on_literal_pattern]  (* accepted *)
  | Lit_pat2 of int [@@warn_on_literal_pattern] (* rejected *)

module type TestImmediate = sig
  type t1 [@@immediate]    (* accepted *)
  type t2 [@@@immediate]   (* rejected *)
  type t3 [@@immediate64]  (* accepted *)
  type t4 [@@@immediate64] (* rejected *)
end

module TestImmediate2 = struct
  let x = (4 [@immediate], 42 [@immediate64]) (* rejected *)
end

module type TestBoxed = sig
  type t1 = {x : int} [@@boxed]    (* accepted *)
  type t2 = {x : int} [@@@boxed]   (* rejected *)
  type t3 = {x : int} [@@unboxed]  (* accepted *)
  type t4 = {x : int} [@@@unboxed] (* rejected *)
  val x : int [@@unboxed]          (* rejected *)
end

module TestBoxed2 = struct
  let x = (5 [@unboxed], 42 [@boxed]) (* rejected *)
end

module type TestPrincipalSig = sig
  type 'a t1 = 'a [@@principal] (* rejected *)
  type 'a t2 = 'a [@@noprincipal] (* rejected *)

  type s1 = Foo1 [@principal] (* rejected *)
  type s2 = Foo2 [@noprincipal] (* rejected *)

  val x : int [@principal] (* rejected *)
  val y : int [@noprincipal] (* rejected *)

  [@@@principal] (* accepted *)
  [@@@noprincipal] (* accepted *)
end

module TestPrincipalStruct = struct
  type 'a t1 = 'a [@@principal] (* rejected *)
  type 'a t2 = 'a [@@noprincipal] (* rejected *)

  type s1 = Foo1 [@principal] (* rejected *)
  type s2 = Foo2 [@noprincipal] (* rejected *)

  let x = 5 [@principal] (* rejected *)
  let y = 42 [@noprincipal] (* rejected *)

  [@@@principal] (* accepted *)
  [@@@noprincipal] (* accepted *)
end

module type TestNolabelsSig = sig
  type 'a t1 = 'a [@@nolabels] (* rejected *)

  type s1 = Foo1 [@nolabels] (* rejected *)

  val x : int [@nolabels] (* rejected *)

  [@@@nolabels] (* accepted *)
end

module TestNolabelsStruct = struct
  type 'a t1 = 'a [@@nolabels] (* rejected *)

  type s1 = Foo1 [@nolabels] (* rejected *)

  let x = 5 [@nolabels] (* rejected *)

  [@@@nolabels] (* accepted *)
end

module type TestFlambdaSig = sig
  type 'a t1 = 'a [@@flambda_o3] (* rejected *)
  type 'a t2 = 'a [@@flambda_oclassic] (* rejected *)

  type s1 = Foo1 [@flambda_o3] (* rejected *)
  type s2 = Foo2 [@flambda_oclassic] (* rejected *)

  val x : int [@flambda_o3] (* rejected *)
  val y : int [@flambda_oclassic] (* rejected *)

  [@@@flambda_o3] (* rejected *)
  [@@@flambda_oclassic] (* rejected *)
end

module TestFlambdaStruct = struct
  type 'a t1 = 'a [@@flambda_o3] (* rejected *)
  type 'a t2 = 'a [@@flambda_oclassic] (* rejected *)

  type s1 = Foo1 [@flambda_o3] (* rejected *)
  type s2 = Foo2 [@flambda_oclassic] (* rejected *)

  let x = 5 [@flambda_o3] (* rejected *)
  let y = 42 [@flambda_oclassic] (* rejected *)

  [@@@flambda_o3] (* accepted *)
  [@@@flambda_oclassic] (* accepted *)
end

module type TestAflInstRatioSig = sig
  type 'a t1 = 'a [@@afl_inst_ratio 42] (* rejected *)

  type s1 = Foo1 [@afl_inst_ratio 42] (* rejected *)

  val x : int [@afl_inst_ratio 42] (* rejected *)

  [@@@afl_inst_ratio 42] (* rejected *)
end

module TestAflInstRatioStruct = struct
  type 'a t1 = 'a [@@afl_inst_ratio 42] (* rejected *)

  type s1 = Foo1 [@afl_inst_ratio 42] (* rejected *)

  let x = 5 [@afl_inst_ratio 42] (* rejected *)

  [@@@afl_inst_ratio 42] (* accepted *)
end

(* No "accepted" test for curry because the user shouldn't write it *)
module type TestCurry = sig
  type 'a t1 = 'a [@@curry 42] (* rejected *)

  type s1 = Foo1 [@curry 42] (* rejected *)

  val x : int [@curry 42] (* rejected *)

  [@@@curry 42] (* rejected *)
end

module TestCurryStruct = struct
  type 'a t1 = 'a [@@curry 42] (* rejected *)

  type s1 = Foo1 [@curry 42] (* rejected *)

  let x = 5 [@curry 42] (* rejected *)

  [@@@curry 42] (* rejected *)
end

module type TestLocalOptSig = sig
  type 'a t1 = 'a [@@local_opt] (* rejected *)
  type s1 = Foo1 [@local_opt] (* rejected *)
  val x : int64 [@@local_opt] (* rejected *)

  external y : (int64 [@local_opt]) -> (int64 [@local_opt]) = "x" (* accepted *)
  external z : int64 -> int64 = "x" [@@local_opt] (* rejected *)
end

module TestLocalOptStruct = struct
  type 'a t1 = 'a [@@local_opt] (* rejected *)
  type s1 = Foo1 [@local_opt] (* rejected *)
  let x : int64 = 42L [@@local_opt] (* rejected *)

  external y : (int64 [@local_opt]) -> (int64 [@local_opt]) = "x" (* accepted *)
  external z : int64 -> int64 = "x" [@@local_opt] (* rejected *)
end

module type TestLocalGlobalSig = sig
  (* All will be rejected, as we no longer support mode attributes *)
  type 'a t1 = 'a [@local]
  type 'a t1' = 'a [@global]

  type t2 = { x : int [@local] }
  type t2' = { x : int [@global] }

  val x : 'a list -> ('a [@local]) list
  val x' : 'a list -> ('a [@global]) list

  val y : 'a -> f:(('a -> 'b) [@local]) -> 'b
  val y' : 'a -> f:(('a -> 'b) [@global]) -> 'b

  val z : 'a [@@local]
  val z' : 'a [@@global]

  val w : 'a [@@@local]
  val w' : 'a [@@@global]
end

module TestLocalGlobalStruct = struct
  (* All will be rejected, as we no longer support mode attributes *)
  type 'a t1 = 'a [@local]
  type 'a t1' = 'a [@global]

  type t2 = { x : int [@local] }
  type t2' = { x : int [@global] }

  let f (a [@local]) = a
  let g (a [@global]) = a
end


module type TestTail = sig
  type 'a t1 = 'a [@tail] (* rejected *)
  type 'a t1' = 'a [@nontail] (* rejected *)

  type t2 = { x : int [@tail] } (* rejected *)
  type t2' = { x : int [@nontail] } (* rejected *)

  val y : 'a -> f:(('a -> 'b) [@tail]) -> 'b (* rejected *)
  val y' : 'a -> f:(('a -> 'b) [@nontail]) -> 'b (* rejected *)

  val z : 'a [@@tail] (* rejected *)
  val z' : 'a [@@nontail] (* rejected *)

  [@@@tail] (* rejected *)
  [@@@nontail] (* rejected *)
end

module TestTail = struct
  let f (a [@tail]) = a (* rejected *)
  let f' (a [@nontail]) = a (* rejected *)

  let [@tail] g a = a (* rejected *)
  let [@nontail] g' a = a (* rejected *)

  let h a = a [@tail] (* rejected *)
  let h' a = a [@nontail] (* rejected *)

  let rec k x = k x [@tail] (* accepted *)
  let rec k' x = k' x [@nontail] (* accepted *)
end

module type TestNoallocSig = sig
  type 'a t1 = 'a [@@noalloc] (* rejected *)
  type s1 = Foo1 [@noalloc] (* rejected *)
  val x : int64 [@@noalloc] (* rejected *)

  external y : (int64 [@noalloc]) -> (int64 [@noalloc]) = "x" (* rejected *)
  external z : int64 -> int64 = "x" [@@noalloc] (* accepted *)
end

module TestNoallocStruct = struct
  type 'a t1 = 'a [@@noalloc] (* rejected *)
  type s1 = Foo1 [@noalloc] (* rejected *)
  let x : int64 = 42L [@@noalloc] (* rejected *)

  external y : (int64 [@noalloc]) -> (int64 [@noalloc]) = "x" (* rejected *)
  external z : int64 -> int64 = "x" [@@noalloc] (* accepted *)
end

module type TestUntaggedSig = sig
  type 'a t1 = 'a [@@untagged] (* rejected *)
  type s1 = Foo1 [@untagged] (* rejected *)
  val x : int [@@untagged] (* rejected *)

  external y : (int [@untagged]) -> (int [@untagged]) = "x" "y" (* accepted *)
  external z : int -> int = "x" "y" [@@untagged] (* accepted *)
end

module TestUntaggedStruct = struct
  type 'a t1 = 'a [@@untagged] (* rejected *)
  type s1 = Foo1 [@untagged] (* rejected *)
  let x : int = 42 [@@untagged] (* rejected *)

  external y : (int [@untagged]) -> (int [@untagged]) = "x" "y" (* accepted *)
  external z : int -> int = "x" "y" [@@untagged] (* accepted *)
end

module type TestPollSig = sig
  type 'a t1 = 'a [@@poll error] (* rejected *)
  type s1 = Foo1 [@poll error] (* rejected *)
  val x : int64 [@@poll error] (* rejected *)

  external y : (int64 [@poll error]) -> (int64 [@poll error]) = (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@poll error] (* rejected *)
end

module TestPollStruct = struct
  type 'a t1 = 'a [@@poll error] (* rejected *)
  type s1 = Foo1 [@poll error] (* rejected *)
  let x : int64 = 42L [@@poll error] (* rejected *)
  let [@poll error] f x = x (* accepted *)

  external y : (int64 [@poll error]) -> (int64 [@poll error]) =  (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@poll error] (* rejected *)
end

(* Attributes in attributes shouldn't be tracked for w53 *)
[@@@foo [@@@deprecated]]

module TestNewtypeAttr = struct
  (* Check for handling of attributes on Pexp_newtype *)
  let f1 = fun [@inline] (type a) (x : a) -> x (* accepted *)

  let f2 = fun [@boxed] (type a) (x : a) -> x (* rejected *)
end

module type TestBuiltinSig = sig
  type 'a t1 = 'a [@@builtin] (* rejected *)
  type s1 = Foo1 [@builtin] (* rejected *)
  val x : int [@@builtin] (* rejected *)

  external y : (int [@builtin]) -> (int [@builtin]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@builtin] (* accepted *)
end

module TestBuiltinStruct = struct
  type 'a t1 = 'a [@@builtin] (* rejected *)
  type s1 = Foo1 [@builtin] (* rejected *)
  let x : int = 42 [@@builtin] (* rejected *)

  external y : (int [@builtin]) -> (int [@builtin]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@builtin] (* accepted *)
end

module type TestNoEffectsSig = sig
  type 'a t1 = 'a [@@no_effects] (* rejected *)
  type s1 = Foo1 [@no_effects] (* rejected *)
  val x : int [@@no_effects] (* rejected *)

  external y : (int [@no_effects]) -> (int [@no_effects]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@no_effects] (* accepted *)
end

module TestNoEffectsStruct = struct
  type 'a t1 = 'a [@@no_effects] (* rejected *)
  type s1 = Foo1 [@no_effects] (* rejected *)
  let x : int = 42 [@@no_effects] (* rejected *)

  external y : (int [@no_effects]) -> (int [@no_effects]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@no_effects] (* accepted *)
end

module type TestNoCoeffectsSig = sig
  type 'a t1 = 'a [@@no_coeffects] (* rejected *)
  type s1 = Foo1 [@no_coeffects] (* rejected *)
  val x : int [@@no_coeffects] (* rejected *)

  external y : (int [@no_coeffects]) -> (int [@no_coeffects]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@no_coeffects] (* accepted *)
end

module TestNoCoeffectsStruct = struct
  type 'a t1 = 'a [@@no_coeffects] (* rejected *)
  type s1 = Foo1 [@no_coeffects] (* rejected *)
  let x : int = 42 [@@no_coeffects] (* rejected *)

  external y : (int [@no_coeffects]) -> (int [@no_coeffects]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@no_coeffects] (* accepted *)
end

module type TestOnlyGenerativeEffectsSig = sig
  type 'a t1 = 'a [@@only_generative_effects] (* rejected *)
  type s1 = Foo1 [@only_generative_effects] (* rejected *)
  val x : int [@@only_generative_effects] (* rejected *)

  external y : (int [@only_generative_effects]) -> (int [@only_generative_effects]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@only_generative_effects] (* accepted *)
end

module TestOnlyGenerativeEffectsStruct = struct
  type 'a t1 = 'a [@@only_generative_effects] (* rejected *)
  type s1 = Foo1 [@only_generative_effects] (* rejected *)
  let x : int = 42 [@@only_generative_effects] (* rejected *)

  external y : (int [@only_generative_effects]) -> (int [@only_generative_effects]) = "x" "y" (* rejected *)
  external z : int -> int = "x" "y" [@@only_generative_effects] (* accepted *)
end

module type TestErrorMessageSig = sig
  type 'a t1 = 'a [@@error_message ""] (* rejected *)
  type s1 = Foo1 [@error_message ""] (* rejected *)
  val x : int [@@error_message ""] (* rejected *)

  external y : (int [@error_message ""]) -> (int [@error_message ""]) = (* rejected *)
    "x" "y"
  external z : int -> int = "x" "y" [@@error_message ""] (* rejected *)

  val f : int ->
    (int as ('a:value)[@error_message ""][@error_message ""]) (* reject second *)
end

module TestErrorMessageStruct = struct
  type 'a t1 = 'a [@@error_message ""] (* rejected *)
  type s1 = Foo1 [@error_message ""] (* rejected *)
  let x : int = 42 [@@error_message ""] (* rejected *)

  external y : (int [@error_message ""]) -> (int [@error_message ""]) = (* rejected *)
    "x" "y"
  external z : int -> int = "x" "y" [@@error_message ""] (* rejected *)

  let f1 v: ((_ : value)[@error_message ""][@error_message ""]) = v (* reject second *)
  let f2 v: (('a : value)[@error_message ""][@error_message ""]) = v (* reject second *)
end

module type TestLayoutPolySig = sig
  type 'a t1 = 'a [@@layout_poly] (* rejected *)
  type s1 = Foo1 [@layout_poly] (* rejected *)
  val x : int64 [@@layout_poly] (* rejected *)

  external y : (int64 [@layout_poly]) -> (int64 [@layout_poly]) = "%identity" (* rejected *)
  external z : ('a : any). 'a -> 'a = "%identity" [@@layout_poly] (* accepted *)
end

module TestLayoutPolyStruct = struct
  type 'a t1 = 'a [@@layout_poly] (* rejected *)
  type s1 = Foo1 [@layout_poly] (* rejected *)
  let x : int64 = 42L [@@layout_poly] (* rejected *)

  external y : (int64 [@layout_poly]) -> (int64 [@layout_poly]) = "%identity" (* rejected *)
  external z : ('a : any). 'a -> 'a = "%identity" [@@layout_poly] (* accepted *)
end

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
(* TEST
 flags = "-w +A-60-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
