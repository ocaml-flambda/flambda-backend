open Location

(** Interpret mode syntax as mode annotation, where axes can be left unspecified *)
val transl_mode_annots : Parsetree.mode loc list -> Mode.Alloc.Const.Option.t

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Parsetree.mode loc list -> Mode.Alloc.Const.t

(** Interpret modality annotation *)
val transl_global_flags :
  Parsetree.modality loc list -> Mode.Global_flag.t loc
