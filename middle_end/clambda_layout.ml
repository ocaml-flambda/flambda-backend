type atom =
  | Value
  | Value_int
  | Unboxed_float
  | Unboxed_int of Lambda.boxed_integer

let rec fold_left_layout (f : 'acc -> 'e -> atom -> 'acc)
    (acc : 'acc) (expr : Clambda.ulambda) (layout : Clambda_primitives.layout) : 'acc =
  match layout with
  | Ptop ->
    Misc.fatal_error "[Ptop] can't be stored in a closure."
  | Pbottom ->
    Misc.fatal_error
      "[Pbottom] should have been eliminated as dead code \
       and not stored in a closure."
  | Punboxed_float -> f acc expr Unboxed_float
  | Punboxed_int bi -> f acc expr (Unboxed_int bi)
  | Pvalue Pintval -> f acc expr Value_int
  | Pvalue _ -> f acc expr Value
  | Punboxed_product layouts ->
    List.fold_left (fun acc (field, layout) ->
        let expr : Clambda.ulambda =
          Uprim (Punboxed_product_field (field, layouts), [expr], Debuginfo.none) in
        fold_left_layout f acc expr layout) acc
      (List.mapi (fun i v -> i, v) layouts)

type ('visible, 'invisible) decomposition' =
  | Gc_visible of ('visible * atom)
  | Gc_invisible of ('invisible * atom)
  | Product of ('visible, 'invisible) decomposition' array

type decomposition =
  | Atom of { offset : int; layout : atom }
  | Product of decomposition array

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
  | Gc_visible (offset, layout) -> Atom { offset; layout }
  | Gc_invisible (offset, layout) -> Atom { offset; layout }
  | Product a ->
    Product (Array.map solidify a)

let rec fold_decompose
    (f1 : 'acc -> 'a -> atom -> 'acc * 'b) (f2 : 'acc -> 'c -> atom -> 'acc * 'd)
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

let atom_size (layout : atom) =
  match layout with
  | Value
  | Value_int
  | Unboxed_float
  | Unboxed_int _ -> 1

let assign_invisible_offsets init_pos (var, dec) =
  let f_visible acc () _layout =
    acc, ()
  in
  let f_invisible acc () layout =
    acc + atom_size layout, acc
  in
  let acc, dec = fold_decompose f_visible f_invisible init_pos dec in
  acc, (var, dec)

let assign_visible_offsets init_pos (var, dec) =
  let f_visible acc () layout =
    acc + atom_size layout, acc
  in
  let f_invisible acc off _layout =
    acc, off
  in
  let acc, dec = fold_decompose f_visible f_invisible init_pos dec in
  acc, (var, solidify dec)

let decompose_free_vars ~base_offset ~free_vars =
  let free_vars =
    List.map (fun (var, kind) -> var, decompose kind) free_vars
  in
  let base_offset, free_vars =
    List.fold_left_map assign_invisible_offsets base_offset free_vars
  in
  let _base_offset, free_vars =
    List.fold_left_map assign_visible_offsets base_offset free_vars
  in
  free_vars
