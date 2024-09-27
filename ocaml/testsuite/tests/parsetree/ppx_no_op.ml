open Ast_mapper

(* This PPX rewriter erases [%%expect] blocks to 'X' expressions. *)

let () =
  Language_extension.set_universe_and_enable_all
    Language_extension.Universe.maximal;
  Ast_mapper.register "no-op"
    (fun _ ->
       let structure_item mapper ({ Parsetree.pstr_desc; _ } as item) =
         match pstr_desc with
         | Pstr_extension (({ txt = "expect"; loc }, _payload), attrs) ->
           { item with pstr_desc =
                         Pstr_eval (Ast_helper.Exp.mk ~loc
                            (Pexp_constant (Pconst_char 'X')), attrs) }
         | _ -> Ast_mapper.default_mapper.structure_item mapper item
       in
       { Ast_mapper.default_mapper with structure_item })
;;
