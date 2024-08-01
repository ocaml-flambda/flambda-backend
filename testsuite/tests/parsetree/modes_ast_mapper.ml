(* TEST
 include ocamlcommon;
*)

let locs_to_string (locs : 'a Location.loc list) (f : 'a -> string) : string =
  List.map
    (fun (m : _ Location.loc) ->
       Format.asprintf "%s [%a]" (f m.txt) Location.print_loc m.loc
    )
    locs
  |> String.concat " "

let mapper: Ast_mapper.mapper =
  let open Ast_mapper in
  { default_mapper with
    modes = (fun sub m ->
      (match m with
      | [] -> ();
      | _ ->
        Format.printf "modes: %s\n"
          (locs_to_string m (fun (Mode s) -> s))
      );
      default_mapper.modes sub m
    );
    modalities = (fun sub m ->
      (match m with
        | [] -> ();
        | _ ->
          Format.printf "modalities: %s\n"
            (locs_to_string m (fun (Modality s) -> s))
      );
      default_mapper.modalities sub m
    );
  }

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
