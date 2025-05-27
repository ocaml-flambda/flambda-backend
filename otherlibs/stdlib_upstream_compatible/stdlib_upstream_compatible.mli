module Float_u = Float_u
module Int32_u = Int32_u
module Int64_u = Int64_u
module Nativeint_u = Nativeint_u

(** OCaml code that depends on the layout of mixed blocks (via [Obj.magic] or
    similar) should include a reference to this value.  The value will be
    removed and replaced with one with an incremented name when the
    representation of mixed blocks changes, alerting maintainers of code that
    reference this value to consider whether updates are needed. It is similar
    in purpose to [Assert_mixed_block_layout_v3], a macro used by C code that
    depends on the mixed block representation for the same reason. *)
val mixed_block_layout_v3 : unit
