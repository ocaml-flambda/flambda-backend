type modifiers =
  { modal_upper_bounds : Mode.Alloc.Const.Option.t;
    externality_upper_bound : Jkind_types.Externality.t option;
    nullability_upper_bound : Jkind_types.Nullability.t option
  }

(** Interpret a list of modes *)
val transl_mode_annots :
  Parsetree.modes -> Mode.Alloc.Const.Option.t

(** Interpret a list of modifiers.
    A "modifier" is any keyword coming after a `mod` in a jkind *)
val transl_modifier_annots :
  Parsetree.modes -> modifiers
