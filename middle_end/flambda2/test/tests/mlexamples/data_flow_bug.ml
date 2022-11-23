let id = Sys.opaque_identity

let prefix_symbols = ['!'; '?'; '~']
(* let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/';
   '$'; '%'; '#' ]

   let special_infix_strings = ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor";
   "mod"; "or"; ":="; "!="; "::" ]

   let letop s = String.length s > 3 && s.[0] = 'l' && s.[1] = 'e' && s.[2] =
   't' && List.mem s.[3] infix_symbols

   let andop s = String.length s > 3 && s.[0] = 'a' && s.[1] = 'n' && s.[2] =
   'd' && List.mem s.[3] infix_symbols *)

let[@inline never] fixity_of_string = function
  (* | "" -> `Normal *)
  (* | s when id false (* List.mem s special_infix_strings *) -> `Infix s | s
     when id false (* List.mem s.[0] infix_symbols *) -> `Infix s | s when id
     false (* List.mem s.[0] prefix_symbols *) -> `Prefix s | s when s.[0] = '.'
     -> `Mixfix s | s when id false (* letop s *) -> `Letop s *)
  | s when id true (* andop s *) -> `Andop s
  | _ -> `Normal

let[@inline never] is_infix = function `Infix _ -> true | _ -> false

let is_mixfix = function `Mixfix _ -> true | _ -> false

let is_kwdop = function `Letop _ | `Andop _ -> true | _ -> false

let first_is_in cs str = str <> "" && List.mem str.[0] cs

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  let fix = fixity_of_string txt in
  is_infix fix || is_mixfix fix || is_kwdop fix
  || first_is_in prefix_symbols txt

let () =
  let _ = (needs_parens [@inlined never]) (id "") in
  ()
