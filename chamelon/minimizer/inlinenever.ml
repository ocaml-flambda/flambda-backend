(* Minimizer inl-never *)

open Utils
open Dummy
open Types
open Stdlib
open Typedtree
open Tast_mapper

let is_function typ = 
  match get_desc typ with 
  | Tarrow _ -> true 
  | _ -> false

let is_inline (attr : attribute) = 
    attr.attr_name.txt = "inline never" 
  || attr.attr_name.txt = "inline always"
  || attr.attr_name.txt = "inline"

  let is_local (attr : attribute) = 
    attr.attr_name.txt = "local never" 
  || attr.attr_name.txt = "local always"

let try_minimize mapper1 mapper2 name str c = 
  let nstr1 = mapper1.structure mapper1 str in 
  update_single name str ; 
  if not(raise_error c) then (
    let nstr2 = mapper2.structure mapper2 str in 
    update_single name str ;
    if not(raise_error c) then 
      str 
    else nstr2 )
  else nstr1
  


let minimize c (inf, sup) map cur_name =
  let has_changed = ref false in 
  let find_nth_vb vb_l attr check_attr nth_expr =  
    List.fold_left
      (fun l vb -> 
      if is_function vb.vb_pat.pat_type then incr nth_expr;
      if inf <= !nth_expr && !nth_expr < sup && not (List.exists check_attr vb.vb_attributes) then 
        (has_changed := true ;
        { vb with vb_attributes =
        attr :: vb.vb_attributes } :: l)
      else vb :: l)
    []
    vb_l
    in 
  let add_attr_mapper attr check_attr nth_expr = 
    {
      Tast_mapper.default with
      structure_item = (fun mapper str_it -> if !nth_expr >= sup then str_it else
        Tast_mapper.default.structure_item mapper
          (match str_it.str_desc with 
          | Tstr_value(r_f, vb_l) -> 
            { str_it with str_desc = 
              Tstr_value(r_f, find_nth_vb vb_l attr check_attr nth_expr) }
          | _ ->  str_it)
          );
         expr = fun mapper e ->
          match e.exp_desc with 
          | Texp_let(r_f,vb_l,e) ->
            { e with exp_desc =
              Texp_let(r_f, find_nth_vb vb_l attr check_attr nth_expr, Tast_mapper.default.expr mapper e)}
          | _ -> e ;
    }
  in 
  let str = Smap.find cur_name map in 

  let inline_never_mapper = add_attr_mapper inline_never is_inline (ref 0) in 
  let inline_always_mapper = add_attr_mapper inline_always is_inline (ref 0) in 
  let nstr = try_minimize inline_never_mapper inline_always_mapper cur_name str c in 

  let local_never_mapper = add_attr_mapper local_never is_local (ref 0)  in 
  let local_always_mapper = add_attr_mapper local_always is_local (ref 0) in 
  let nstr = try_minimize local_never_mapper local_always_mapper cur_name nstr c in 

  Smap.add cur_name nstr map, !has_changed

(*
let minimizer = {
  minimizer_name = "inline-never" ;
  minimizer_func = minimize ; (* TODO *)
}
*)
