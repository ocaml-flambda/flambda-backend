(** Interpret mode syntax as mode annotation, where axes can be left unspecified *)
val transl_mode_annots : Jane_syntax.Mode_expr.t -> Mode.Alloc.Const.Option.t

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Jane_syntax.Mode_expr.t -> Mode.Alloc.Const.t

(** Interpret modality annotation *)
val transl_global_flags :
  Modality.modality Location.loc list -> Mode.Global_flag.t Location.loc
