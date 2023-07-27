(* Minimizer inl-never *)

open Utils
open Dummy
open Types
open Stdlib
open Typedtree
open Tast_mapper

let is_function typ = match get_desc typ with Tarrow _ -> true | _ -> false

let rec maybe_replace_attr should_remove consider prevent new_attr attrs =
  match attrs with
  | [] -> if should_remove () then [ new_attr ] else []
  | attr :: nattrs ->
      if is_attr prevent attr then attrs
      else if is_attr consider attr then
        if should_remove () then new_attr :: nattrs else attrs
      else
        attr
        :: maybe_replace_attr should_remove consider prevent new_attr nattrs

let minimize_attrs should_remove attrs =
  let attrs =
    maybe_replace_attr should_remove
      [ "inline always"; "inline" ]
      [ "inline never" ] inline_never attrs
  in
  let attrs =
    maybe_replace_attr should_remove []
      [ "inline never"; "inline always"; "inline" ]
      inline_always attrs
  in
  let attrs =
    maybe_replace_attr should_remove
      [ "local always"; "local" ]
      [ "local never" ] local_never attrs
  in
  let attrs =
    maybe_replace_attr should_remove []
      [ "local always"; "local"; "local never" ]
      local_always attrs
  in
  attrs

let minimize should_remove map cur_name =
  let minimize_vb_attrs vb =
    if is_function vb.vb_pat.pat_type then
      { vb with vb_attributes = minimize_attrs should_remove vb.vb_attributes }
    else vb
  in
  let mapper =
    {
      Tast_mapper.default with
      structure_item =
        (fun mapper str_it ->
          Tast_mapper.default.structure_item mapper
            (match str_it.str_desc with
            | Tstr_value (r_f, vb_l) ->
                {
                  str_it with
                  str_desc = Tstr_value (r_f, List.map minimize_vb_attrs vb_l);
                }
            | _ -> str_it));
      expr =
        (fun mapper e ->
          match e.exp_desc with
          | Texp_let (r_f, vb_l, e) ->
              {
                e with
                exp_desc =
                  Texp_let
                    ( r_f,
                      List.map minimize_vb_attrs vb_l,
                      Tast_mapper.default.expr mapper e );
              }
          | _ -> Tast_mapper.default.expr mapper e);
    }
  in
  let str = Smap.find cur_name map in
  let nstr = mapper.structure mapper str in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "inline-never"; minimizer_func = minimize }
