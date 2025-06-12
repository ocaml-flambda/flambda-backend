(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

let run s =
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression (Lazy.force Env.initial) pe in
  let ute = Untypeast.untype_expression te in
  Format.asprintf "%a" Pprintast.expression ute
;;

[%%expect{|
val run : string -> string = <fun>
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
- : string = "match None with | Some (Some _) -> () | _ -> ()"
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
- : string = "fun x y z -> function | w -> x y z w"
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
- : string = "fun x y z -> (function | w -> x y z w)"
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
- : string = "match None with | Some (Some _) -> () | _ -> ()"
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
- : string = "fun x y z -> function | w -> x y z w"
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
- : string = "fun x y z -> (function | w -> x y z w)"
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle value binding type annotations. *)

run {| let foo : 'a. 'a -> 'a = fun x -> x in foo |}

[%%expect{|
- : string = "let foo : ('a : value) . 'a -> 'a = fun x -> x in foo"
|}];;

run {| let foo : type a . a -> a = fun x -> x in foo |}

[%%expect{|
- : string =
"let foo : ('a : value) . 'a -> 'a = fun (type a) -> ( (fun x -> x : a -> a)) in\nfoo"
|}];;

(* CR: untypeast/pprintast are totally busted on programs with modes in value
   bindings. Fix this. *)
run {| let foo : ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
- : string =
"let (foo : 'a -> 'a) = ((fun x -> x : 'a -> 'a) : _ @ portable) in foo"
|}];;

run {| let foo : 'a . ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
- : string = "let foo : ('a : value) . 'a -> 'a = fun x -> x in foo"
|}];;
