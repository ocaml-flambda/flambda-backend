(* TEST
 include ocamlcommon;
*)

let mode_to_string (modes : Asttypes.mode Location.loc list) =
  List.map
    (fun (m : _ Location.loc) ->
       let (Asttypes.Mode s) = m.txt in
       Format.asprintf "%s [%a]" s Location.print_loc m.loc
    )
    modes
  |> String.concat " "

let mapper: Ast_mapper.mapper =
  let open Ast_mapper in
  { default_mapper with
  modes = fun sub m ->
    (match m with
    | [] -> ();
    | _ ->
    Format.printf "%s\n"
      (mode_to_string m));
    default_mapper.modes sub m}

let test mapper s =
  let p = Lexing.from_string s |> Parse.implementation in
  ignore (mapper.Ast_mapper.structure mapper p);
  Format.printf "%a\n" (Printast.structure 0) p;
  Format.printf "------------------------------\n"

let () =
  test mapper "let f (local_ x) = x";
  test mapper "let unique_ f (local_ x) = x";
  test mapper "let local_ f x: int -> int = x";
  test mapper "module M : sig val x : string -> string @ foo @@ bar hello end = struct end";
  ()
