type type_shape =
  | Any (** anything of type [value] *)
  | Imm (** immediate, tagged with a one at the end *)
  | Nativeint (** block of a native word integer, e.g., 64-bit integer on amd64 target *)
  | Double (** block of a native double *)
  | Int64 (** block of a 64-bit integer *)
  | Int32 (** block of a 32-bit integer *)
  | String
  (** block of a char pointer with a size, representing both Bytes.t and String.t *)
  | FloatArray (** block containing native doubles *)
  | Block of (int * type_shape list) option
  (** Block whose tag is below no-scan tag (i.e., a normal ocaml block value). If the
      argment is [None], then the block could have any tag and any elements. If the
      argument is [Some (t, shs)], then [t] is the tag of the block and [shs] contains the
      shapes of its fields. Fittingly, in the case of [Some (t, shs)], the number of
      fields is known statically (i.e., the length of the list [shs]).

      To represent arrays (which are blocks with tag 0 at run time, but whose size is not
      statically known), there is a separate construtor, [Array sh], which keeps track of
      the shapes of the elements. *)
  | Array of type_shape
  (** Block with tag 0 and a fixed size (not known statically). The shape of the elements
      is given by the argument. *)
  | Closure (** Block with closure tag. *)
  | Obj (** Block with object tag. *)
  | Or of type_shape * type_shape
  (** Disjunction between two shapes for (e.g., variant types) *)

type fn_type_shapes =
  { arguments : type_shape list
  ; return : type_shape
  }

(** An [extfun_desc] describes the information that we know about an external function. To
    enable extensions in the future, we define it as a record of options. This enables
    adding new, optional fields in the future without breaking the serialized form. *)
type extfun_desc =
  { shape : fn_type_shapes option
  (** If the shape is not present, then we fallback on the arity of the C code. *)
  }

type extfun =
  { name : string (** C name of the function *)
  ; desc : extfun_desc
  }

type extfuns =
  { version : string
  ; extfuns : extfun list
  }

val pp_shape : Format.formatter -> type_shape -> unit
val pp_extfun_desc : Format.formatter -> extfun_desc -> unit
val pp_ext_fun : Format.formatter -> extfun -> unit
val pp_extfuns : Format.formatter -> extfuns -> unit

(* functions for serializing/unserializing an external function *)
val serialize_extfuns : extfuns -> string
