(** Interpret mode syntax as mode annotation, where axes can be left unspecified *)
val transl_mode_annots : Parsetree.modes -> Mode.Alloc.Const.Option.t

val untransl_mode_annots : Mode.Alloc.Const.Option.t -> Parsetree.modes

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Parsetree.modes -> Mode.Alloc.Const.t

(** Interpret mode syntax as modalities. Modalities occuring at different places
    requires different levels of maturity. Also takes the mutability and
    attributes on the field and insert mutable-implied modalities accordingly.
    *)
val transl_modalities :
  maturity:Language_extension.maturity ->
  Types.mutability ->
  Parsetree.modalities ->
  Mode.Modality.Value.Const.t

val untransl_modality : Mode.Modality.t -> Parsetree.modality Location.loc

(** Un-interpret modalities back to parsetree. Takes the mutability and
    attributes on the field and remove mutable-implied modalities accordingly.
    *)
val untransl_modalities :
  Types.mutability -> Mode.Modality.Value.Const.t -> Parsetree.modalities

(** Interpret a mod-bounds. *)
val transl_mod_bounds : Parsetree.modes -> Types.Jkind_mod_bounds.t

val idx_expected_modalities : mut:bool -> Mode.Modality.Value.Const.t
