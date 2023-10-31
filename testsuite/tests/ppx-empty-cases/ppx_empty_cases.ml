open Ast_mapper

(* PPXes could have empty cases. *)

let () =
  register "empty_cases" (fun _ ->
    { default_mapper with cases = fun _ cases ->
      match cases with
      | [ { pc_lhs = { ppat_desc = Ppat_extension ({ txt = "empty" }, _) };
            pc_rhs = { pexp_desc = Pexp_unreachable };
          }
        ] -> []
      | _ -> cases
    }
  )
