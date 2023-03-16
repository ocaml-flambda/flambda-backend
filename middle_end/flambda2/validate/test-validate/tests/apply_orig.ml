let $camlApply__first_const19 = Block 0 () in
let code size(1) f_0 (x) my_closure my_region my_depth -> k * k1 = cont k (x)
in
let code size(5) g_1 (x) my_closure my_region my_depth -> k * k1 =
  let f = %project_value_slot g.f my_closure in
  apply direct(f_0) f (x) -> k * k1
in
(let f = closure f_0 @f in
 let g = closure g_1 @g with { f = f } in
let Pmakeblock = %Block 0 (f, g) in
cont k (Pmakeblock))
where k define_root_symbol (module_block) =
let field_0 = %block_load tag(0) size(2) (module_block, 0) in
let field_1 = %block_load tag(0) size(2) (module_block, 1) in
let $camlApply = Block 0 (field_0, field_1) in
cont done ($camlApply)
