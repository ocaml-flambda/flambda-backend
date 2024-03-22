open Ast_mapper

(* This PPX rewriter does nothing. *)

let () =
  Language_extension.set_universe Alpha;
  Ast_mapper.register "no-op" (fun _ -> Ast_mapper.default_mapper);
;;
