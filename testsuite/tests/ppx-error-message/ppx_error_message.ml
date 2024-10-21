open Ast_mapper

(* Assert statically that a string that appears in the source text
   is alphabetically ordered. This is a bit contrived so that we
   can exercise [@ocaml.error_message].
*)

let () =
  register "sorted" (fun _ ->
    { default_mapper with expr = fun self expr ->
      match expr.pexp_desc with
      | Pexp_extension
         ( { txt = "sorted" },
           PStr
             [ { pstr_desc =
                   Pstr_eval
                     ( { pexp_desc = Pexp_constant (Pconst_string (str, loc, _)) }
                     , _ ) } ] )
         ->
        (* Use a ghost location, as is typical for ppxes *)
        let loc = { loc with loc_ghost = true } in
        let sorted =
          String.to_seq str
          |> List.of_seq
          |> List.sort Char.compare
          |> List.to_seq
          |> String.of_seq
        in
        Ast_helper.with_default_loc loc (fun () ->
          Ast_helper.Exp.apply
            (Ast_helper.Exp.ident { txt = Lident "ignore"; loc})
            [ Nolabel,
                Ast_helper.Exp.attr
                  (Ast_helper.Exp.constraint_
                    (Ast_helper.Exp.variant sorted None)
                    (Some (Ast_helper.Typ.variant
                      [ Ast_helper.Rf.tag { txt = str; loc } true [] ]
                      Closed
                      None ))
                    [])
                  (Ast_helper.Attr.mk
                     { txt = "ocaml.error_message"; loc }
                     (PStr
                       [ Ast_helper.Exp.constant
                           (Ast_helper.Const.string
                             (Printf.sprintf
                               "The %s string is not in alphabetical order."
                               str))
                         |> Ast_helper.Str.eval
                        ]))
            ])
      | _ -> default_mapper.expr self expr
    }
  )
