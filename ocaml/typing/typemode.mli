(** Interpret mode syntax as mode annotation, where axes can be left unspecified *)
val transl_mode_annots : Asttypes.mode list -> Mode.Alloc.Const.Option.t

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Jane_syntax.Mode_expr.t -> Mode.Alloc.Const.t

(** Interpret mode syntax as modalities *)
val transl_global_flags : Jane_syntax.Mode_expr.t -> Mode.Global_flag.t
