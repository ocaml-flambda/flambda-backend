(* TEST
   flags = "-I ${ocamlsrcdir}/typing \
    -I ${ocamlsrcdir}/parsing \
    -I ${ocamlsrcdir}/utils"
   include ocamlcommon
   * expect
*)

let res =
  let s = {| match None with Some (Some _) -> () | _ -> () |} in
  let pe = Parse.expression (Lexing.from_string s) in
<<<<<<< HEAD
  let te = Typecore.type_expression (Lazy.force Env.initial_safe_string) pe in
||||||| merged common ancestors
  let te = Typecore.type_expression (Env.initial_safe_string) pe in
=======
  let te = Typecore.type_expression Env.initial pe in
>>>>>>> ocaml/5.1
  let ute = Untypeast.untype_expression te in
  Format.asprintf "%a" Pprintast.expression ute

[%%expect{|
val res : string = "match None with | Some (Some _) -> () | _ -> ()"
|}]
