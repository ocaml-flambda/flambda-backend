(* rem-att : remove attributes *)

open Tast_mapper
open Utils
open Typedtree
open Compat

let is_inline = is_attr [ "inline never"; "inline always"; "inline" ]
let is_local = is_attr [ "local never"; "local always"; "local" ]

let handle_attributes should_remove attributes =
  List.filter
    (fun attr -> is_inline attr || (is_local attr && not (should_remove ())))
    attributes

let minimize should_remove map cur_name =
  let remove_attributes_mapper =
    {
      Tast_mapper.default with
      structure =
        (fun mapper str ->
          {
            str with
            str_items =
              List.rev
                (List.fold_left
                   (fun l str_it ->
                     match str_it.str_desc with
                     | Tstr_attribute _ ->
                         if should_remove () then l else str_it :: l
                     | _ -> mapper.structure_item mapper str_it :: l)
                   [] str.str_items);
          });
      type_declaration =
        (fun mapper td ->
          Tast_mapper.default.type_declaration mapper
            (let td =
               {
                 td with
                 typ_attributes =
                   handle_attributes should_remove td.typ_attributes;
               }
             in
             match td.typ_kind with
             | Ttype_record ld_l ->
                 {
                   td with
                   typ_kind =
                     Ttype_record
                       (List.map
                          (fun ld ->
                            {
                              ld with
                              ld_attributes =
                                handle_attributes should_remove ld.ld_attributes;
                            })
                          ld_l);
                 }
             | Ttype_variant cd_l ->
                 {
                   td with
                   typ_kind =
                     Ttype_variant
                       (List.map
                          (fun cd ->
                            let cd =
                              {
                                cd with
                                cd_attributes =
                                  handle_attributes should_remove
                                    cd.cd_attributes;
                              }
                            in
                            match cd.cd_args with
                            | Cstr_record ld_l ->
                                {
                                  cd with
                                  cd_args =
                                    Cstr_record
                                      (List.map
                                         (fun ld ->
                                           {
                                             ld with
                                             ld_attributes =
                                               handle_attributes should_remove
                                                 ld.ld_attributes;
                                           })
                                         ld_l);
                                }
                            | _ -> cd)
                          cd_l);
                 }
             | _ -> td));
      value_binding =
        (fun mapper vb ->
          Tast_mapper.default.value_binding mapper
            {
              vb with
              vb_attributes = handle_attributes should_remove vb.vb_attributes;
            });
      expr =
        (fun mapper e ->
          Tast_mapper.default.expr mapper
            {
              e with
              exp_attributes = handle_attributes should_remove e.exp_attributes;
            });
      structure_item =
        (fun mapper str_it ->
          Tast_mapper.default.structure_item mapper
            (match view_tstr str_it.str_desc with
            | Tstr_eval (e, attrs, id) ->
                {
                  str_it with
                  str_desc =
                    mkTstr_eval ~id (e, handle_attributes should_remove attrs);
                }
            | _ -> str_it));
      module_binding =
        (fun mapper mb ->
          Tast_mapper.default.module_binding mapper
            {
              mb with
              mb_attributes = handle_attributes should_remove mb.mb_attributes;
            });
      module_expr =
        (fun mapper me ->
          Tast_mapper.default.module_expr mapper
            {
              me with
              mod_attributes = handle_attributes should_remove me.mod_attributes;
            });
      module_substitution =
        (fun mapper ms ->
          Tast_mapper.default.module_substitution mapper
            {
              ms with
              ms_attributes = handle_attributes should_remove ms.ms_attributes;
            });
      module_type_declaration =
        (fun mapper mtd ->
          Tast_mapper.default.module_type_declaration mapper
            {
              mtd with
              mtd_attributes =
                handle_attributes should_remove mtd.mtd_attributes;
            });
      module_type =
        (fun mapper mty ->
          Tast_mapper.default.module_type mapper
            {
              mty with
              mty_attributes =
                handle_attributes should_remove mty.mty_attributes;
            });
      value_description =
        (fun mapper vd ->
          Tast_mapper.default.value_description mapper
            {
              vd with
              val_attributes = handle_attributes should_remove vd.val_attributes;
            });
    }
  in
  let mapper = remove_attributes_mapper in
  let nstr = mapper.structure mapper (Smap.find cur_name map) in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "remove-attributes"; minimizer_func = minimize }
