type type_shape =
  | Any (** anything of C type [value] *)
  | Imm (** immediate, tagged with a one at the end *)
  | Nativeint (** block to a native word (= 64-bit) integer *)
  | Double (** block to a native double *)
  | Int64 (** block to a 64-bit integer *)
  | Int32 (** block to a 32-bit integer *)
  | String (** block to a char pointer with a size *)
  | FloatArray (** block containing native doubles *)
  | Block of (int * type_shape list) option
  (** Block below no-scan tag. If the argment is [None], then the block could have any tag
      and any elements. If the argument is [Some (t, shs)], then [t] is the tag of the
      block and [shs] contains the shapes of its fields. The length of the block is known
      statically. Otherwise, it must be an array (see below). *)
  | Array of type_shape
  (** Block with tag 0 and a fixed size (not known statically). The shape of the elements
      is given by the argument. *)
  | Closure (** Block with closure tag. *)
  | Obj (** Block with object tag. *)
  | Or of type_shape * type_shape
  (** Disjunction between two shapes for (e.g., variant types) *)

let rec print_shapes ppf (sh : type_shape) =
  match sh with
  | Any -> Format.fprintf ppf "@[<hov 1>Any@]"
  | Imm -> Format.fprintf ppf "@[<hov 1>Imm@]"
  | Nativeint -> Format.fprintf ppf "@[<hov 1>Nativeint@]"
  | Double -> Format.fprintf ppf "@[<hov 1>Double@]"
  | Int64 -> Format.fprintf ppf "@[<hov 1>Int64@]"
  | Int32 -> Format.fprintf ppf "@[<hov 1>Int32@]"
  | String -> Format.fprintf ppf "@[<hov 1>String@]"
  | FloatArray -> Format.fprintf ppf "@[<hov 1>FloatArray@]"
  | Block None -> Format.fprintf ppf "@[<hov 1>(Block@ ())@]"
  | Block (Some (tag, shapes)) ->
    Format.fprintf ppf "@[<hov 1>(Block@ ((%d@ %a)))@]" tag print_shape_list shapes
  | Array s -> Format.fprintf ppf "@[<hov 1>(Array@ %a)@]" print_shapes s
  | Closure -> Format.fprintf ppf "@[<hov 1>Closure@]"
  | Obj -> Format.fprintf ppf "@[<hov 1>Obj@]"
  | Or (s1, s2) ->
    Format.fprintf ppf "@[<hov 1>(Or@ %a@ %a)@]" print_shapes s1 print_shapes s2

and print_shape_list ppf shapes =
  Format.fprintf
    ppf
    "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") print_shapes)
    shapes
;;

(* TODO: check which shapes exceptions should have *)
type fn_type_shapes =
  { arguments : type_shape list
  ; return : type_shape
  }

let print_fn_type_shapes ppf { arguments; return } =
  Format.fprintf
    ppf
    "@[<hov 1>(@[<hov 1>(arguments@ %a)@]@ @[<hov 1>(return@ %a)@])@]"
    print_shape_list
    arguments
    print_shapes
    return
;;

(** An [extfun_desc] describes the information that we know about an external function. To
    enable extensions in the future, we define it as a record of options. This enables
    adding new, optional fields in the future without breaking the serialized form. *)
type extfun_desc =
  { shape : fn_type_shapes option
  (** If the shape is not present, then we fallback on the arity of the C code. *)
  }

let print_extfun_desc ppf { shape } =
  match shape with
  | None -> Format.fprintf ppf "@[<hov 1>()@]"
  | Some shape ->
    Format.fprintf ppf "@[<hov 1>(@[<hov 1>(shape@ %a)@])@]" print_fn_type_shapes shape
;;

type extfun =
  { name : string (** C name of the function *)
  ; desc : extfun_desc
  }

let print_ext_fun ppf { name; desc } =
  Format.fprintf
    ppf
    "@[<hov 1>(@[<hov 1>(name@ %s)@]@ @[<hov 1>(desc@ %a)@])@]"
    name
    print_extfun_desc
    desc
;;

type extfuns =
  { version : string
  ; extfuns : extfun list
  }

let print_extfuns ppf { version; extfuns } =
  Format.fprintf
    ppf
    "@[<hov 1>(@[<hov 1>(version@ %s)@]@ @[<hov 1>(extfuns@ (%a))@])@]"
    version
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") print_ext_fun)
    extfuns
;;

let serialize_extfuns exts = Format.asprintf "%a" print_extfuns exts


(* FIXME: clean up the readable version to be parallel to the serialized version *)
let rec pp_shape fmt (sh : type_shape) =
  match sh with
  | Any -> Format.pp_print_string fmt "*"
  | Imm -> Format.pp_print_string fmt "imm"
  | Nativeint -> Format.pp_print_string fmt "native"
  | Double -> Format.pp_print_string fmt "double"
  | Int64 -> Format.pp_print_string fmt "int64"
  | Int32 -> Format.pp_print_string fmt "int32"
  | String -> Format.pp_print_string fmt "string"
  | Block None -> Format.pp_print_string fmt "block"
  | Block (Some (tag, shapes)) ->
    Format.fprintf
      fmt
      "block[%d](%a)"
      tag
      (Format.pp_print_list pp_shape ~pp_sep:(fun fmt () ->
         Format.pp_print_string fmt "; "))
      shapes
  | FloatArray -> Format.pp_print_string fmt "float_array"
  | Obj -> Format.pp_print_string fmt "object"
  | Array s -> Format.fprintf fmt "array(%a)" pp_shape s
  | Closure -> Format.pp_print_string fmt "closure"
  | Or (s1, s2) -> Format.fprintf fmt "%a ∨ %a" pp_shape s1 pp_shape s2
;;

let pp_extfun_desc fmt desc =
  match desc with
  | { shape = Some { arguments; return }; _ } ->
    Format.fprintf
      fmt
      "args(%a) -> %a"
      (Format.pp_print_list pp_shape ~pp_sep:(fun fmt () ->
         Format.pp_print_string fmt "; "))
      arguments
      pp_shape
      return
  | { shape = None; _ } -> Format.fprintf fmt "*"
;;

let pp_ext_fun fmt ext = Format.fprintf fmt "%s: %a" ext.name pp_extfun_desc ext.desc

let pp_extfuns fmt exts =
  Format.fprintf
    fmt
    "Version %s@\n%a"
    exts.version
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_ext_fun)
    exts.extfuns
;;
