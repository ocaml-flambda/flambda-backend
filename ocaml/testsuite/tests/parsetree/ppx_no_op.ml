open Ast_mapper

(* This PPX rewriter does nothing. *)

let () =
  Language_extension.set_universe_and_enable_all
    Language_extension.Universe.maximal;
  Ast_mapper.register "no-op" (fun _ -> Ast_mapper.default_mapper);
;;
