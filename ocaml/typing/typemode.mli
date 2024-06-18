(** Interpret mode syntax as mode annotation, where axes can be left unspecified *)
val transl_mode_annots : Jane_syntax.Mode_expr.t -> Mode.Alloc.Const.Option.t

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Jane_syntax.Mode_expr.t -> Mode.Alloc.Const.t

(** Interpret mode syntax as modalities *)
val transl_modalities :
  Types.mutability ->
  Parsetree.modality Location.loc list ->
  Mode.Modality.Value.t

val untransl_modalities :
  loc:Location.t ->
  Mode.Modality.Value.t ->
  Parsetree.modality Location.loc list

val is_mutable_implied_modality : Mode.Modality.t -> bool

val mutable_implied_modalities : Mode.Modality.Value.t
