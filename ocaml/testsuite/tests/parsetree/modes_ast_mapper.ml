(* TEST
    include ocamlcommon
*)

let mode_to_string (m : Jane_syntax.Mode_expr.t) =
  List.map (fun m ->
    (m : Jane_syntax.Mode_expr.Const.t :> _ Location.loc).txt
  ) m.txt
  |> String.concat " "
let mapper: Ast_mapper.mapper =
  let open Ast_mapper in
  { default_mapper with
  modes = fun sub m ->
    Format.printf "%s [%a]\n"
      (mode_to_string m)
      Location.print_loc m.loc;
    default_mapper.modes sub m}

let test mapper s =
  let p = Lexing.from_string s |> Parse.implementation in
  ignore (mapper.Ast_mapper.structure mapper p);
  Format.printf "------------------------------\n"

let () =
  test mapper "let f (local_ x) = x";
  test mapper "let unique_ f (local_ x) = x";
  test mapper "let local_ f x: int -> int = x";
  test mapper "module M : sig val x : string -> string @ foo @@ bar hello end = struct end";
  ()
