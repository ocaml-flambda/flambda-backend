(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type layout_atom =
  | Value
  | Value_int
  | Unboxed_float
  | Unboxed_int of Lambda.boxed_integer

type ('visible, 'invisible) decomposition' =
  | Gc_visible of ('visible * layout_atom)
  | Gc_invisible of ('invisible * layout_atom)
  | Product of ('visible, 'invisible) decomposition' array

type decomposition =
  | Atom of (int * layout_atom)
  | Product of decomposition array
type parts = decomposition

let equal_parts (p1 : parts) p2 = p1 = p2
let print_parts ppf _p =
  Format.fprintf ppf "TODO offset parts"

type result = {
  function_offsets : int Closure_id.Map.t;
  free_variable_offsets : parts Var_within_closure.Map.t;
}

let rec decompose (layout : Lambda.layout) : _ decomposition' =
  match layout with
  | Ptop ->
    Misc.fatal_error "[Ptop] can't be stored in a closure."
  | Pbottom ->
    Misc.fatal_error
      "[Pbottom] should have been eliminated as dead code \
       and not stored in a closure."
  | Punboxed_float -> Gc_invisible ((), Unboxed_float)
  | Punboxed_int bi -> Gc_invisible ((), Unboxed_int bi)
  | Pvalue Pintval -> Gc_invisible ((), Value_int)
  | Pvalue _ -> Gc_visible ((), Value)
  | Punboxed_product l ->
    Product (Array.of_list (List.map decompose l))

let rec solidify (dec : (int, int) decomposition') : decomposition =
  match dec with
  | Gc_visible (off, layout) -> Atom (off, layout)
  | Gc_invisible (off, layout) -> Atom (off, layout)
  | Product a ->
    Product (Array.map solidify a)

let rec fold_decompose
    (f1 : 'acc -> 'a -> layout_atom -> 'acc * 'b) (f2 : 'acc -> 'c -> layout_atom -> 'acc * 'd)
    (acc : 'acc) (d : ('a, 'c) decomposition') :
  'acc * ('b, 'd) decomposition' =
  match d with
  | Gc_visible (v, layout) ->
    let acc, v = f1 acc v layout in
    acc, Gc_visible (v, layout)
  | Gc_invisible (v, layout) ->
    let acc, v = f2 acc v layout in
    acc, Gc_invisible (v, layout)
  | Product elts ->
    let acc, elts = Array.fold_left_map (fold_decompose f1 f2) acc elts in
    acc, Product elts

let layout_atom_size (layout : layout_atom) =
  match layout with
  | Value
  | Value_int
  | Unboxed_float
  | Unboxed_int _ -> 1

let assign_visible_offsets init_pos (var, dec) =
  let f_visible acc () layout =
    acc + layout_atom_size layout, acc
  in
  let f_invisible acc () _layout =
    acc, ()
  in
  let acc, dec = fold_decompose f_visible f_invisible init_pos dec in
  acc, (var, dec)

let assign_invisible_offsets init_pos (var, dec) =
  let f_visible acc off _layout =
    acc, off
  in
  let f_invisible acc () layout =
    acc + layout_atom_size layout, acc
  in
  let acc, dec = fold_decompose f_visible f_invisible init_pos dec in
  acc, (var, solidify dec)

let add_closure_offsets
      { function_offsets; free_variable_offsets }
      ({ function_decls; free_vars } : Flambda.set_of_closures) =
  (* Build the table mapping the functions declared by the set of closures
     to the positions of their individual "infix" closures inside the runtime
     closure block.  (All of the environment entries will come afterwards.) *)
  let assign_function_offset id function_decl (map, env_pos) =
    let pos = env_pos + 1 in
    let env_pos =
      let arity = Flambda_utils.function_arity function_decl in
      env_pos
        + 1  (* GC header; either [Closure_tag] or [Infix_tag] *)
        + 1  (* full application code pointer *)
        + 1  (* arity *)
        + (if arity > 1 then 1 else 0)  (* partial application code pointer *)
    in
    let closure_id = Closure_id.wrap id in
    if Closure_id.Map.mem closure_id map then begin
      Misc.fatal_errorf "Closure_offsets.add_closure_offsets: function \
          offset for %a would be defined multiple times"
        Closure_id.print closure_id
    end;
    let map = Closure_id.Map.add closure_id pos map in
    (map, env_pos)
  in
  let function_offsets, free_variable_pos =
    Variable.Map.fold assign_function_offset
      function_decls.funs (function_offsets, -1)
  in
  (* Adds the mapping of free variables to their offset.  Recall that
     projections of [Var_within_closure]s are only currently used when
     compiling accesses to the closure of a function from outside that
     function (in particular, as a result of inlining).  Accesses to
     a function's own closure are compiled directly via normal [Var]
     accesses. *)
  (* CR-someday mshinwell: As discussed with lwhite, maybe this isn't
     ideal, and the self accesses should be explicitly marked too. *)
  let free_vars = Variable.Map.bindings free_vars in
  let free_vars = List.map (fun (var, (free_var : Flambda.specialised_to)) ->
      var, decompose free_var.kind) free_vars in
  let free_variable_pos, free_vars =
    List.fold_left_map assign_visible_offsets free_variable_pos free_vars
  in
  let _free_variable_pos, free_vars =
    List.fold_left_map assign_invisible_offsets free_variable_pos free_vars
  in
  let free_variable_offsets =
    List.fold_left (fun map (var, dec) ->
        let var_within_closure = Var_within_closure.wrap var in
        Var_within_closure.Map.add var_within_closure dec map)
      free_variable_offsets free_vars
  in
  { function_offsets;
    free_variable_offsets;
  }

let compute (program:Flambda.program) =
  let init : result =
    { function_offsets = Closure_id.Map.empty;
      free_variable_offsets = Var_within_closure.Map.empty;
    }
  in
  let r =
    List.fold_left add_closure_offsets
      init (Flambda_utils.all_sets_of_closures program)
  in
  r
