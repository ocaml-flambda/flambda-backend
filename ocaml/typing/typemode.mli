(** Interpret mode syntax as mode annotation, where axes can be left unspecified *)
val transl_mode_annots : Parsetree.modes -> Mode.Alloc.Const.Option.t

val untransl_mode_annots :
  loc:Location.t -> Mode.Alloc.Const.Option.t -> Parsetree.modes

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Parsetree.modes -> Mode.Alloc.Const.t

(** Interpret mode syntax as modalities *)
val transl_modalities :
  has_mutable_implied_modalities:bool ->
  Parsetree.modalities ->
  Mode.Modality.Value.t

val untransl_modalities :
  loc:Location.t -> Mode.Modality.Value.t -> Parsetree.modalities

val is_mutable_implied_modality : Mode.Modality.t -> bool

val mutable_implied_modalities : Mode.Modality.Value.t
