(******************************************************************************
 *                             flambda-backend                                *
 *                        Simon Spies, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

type value_shape =
  | Value  (** anything of C type [value] *)
  | Imm  (** immediate, tagged with a one at the end *)
  | Nativeint
      (** block of a native word integer, e.g., 64-bit integer on amd64 target *)
  | Double  (** block of a native double *)
  | Int64  (** block of a 64-bit integer *)
  | Int32  (** block of a 32-bit integer *)
  | String
      (** block of a char pointer with a size, representing both Bytes.t and String.t *)
  | FloatArray  (** block containing native doubles *)
  | Block of (int * value_shape list) option
      (** Block whose tag is below no-scan tag (i.e., a normal ocaml block value). If the
     argment is [None], then the block could have any tag and any elements. If the
     argument is [Some (t, shs)], then [t] is the tag of the block and [shs] contains the
     shapes of its fields. In the case of [Some (t, shs)], the number of
     fields is known statically (i.e., the length of the list [shs]).

     To represent arrays (which are blocks with tag 0 at run time, but whose size is not
     statically known), there is a separate construtor, [Array sh], which keeps track of
     the shapes of the elements. *)
  | Array of value_shape
      (** Block with tag 0 and a fixed size (not known statically). The shape of the
         elements is given by the argument. *)
  | Closure  (** Block with closure tag. *)
  | Obj  (** Block with object tag. *)
  | Or of value_shape * value_shape
      (** Disjunction between two shapes for, e.g., variant types. *)

let rec print_shapes ppf (sh : value_shape) =
  match sh with
  | Value -> Format.fprintf ppf "@[<hov 1>Value@]"
  | Imm -> Format.fprintf ppf "@[<hov 1>Imm@]"
  | Nativeint -> Format.fprintf ppf "@[<hov 1>Nativeint@]"
  | Double -> Format.fprintf ppf "@[<hov 1>Double@]"
  | Int64 -> Format.fprintf ppf "@[<hov 1>Int64@]"
  | Int32 -> Format.fprintf ppf "@[<hov 1>Int32@]"
  | String -> Format.fprintf ppf "@[<hov 1>String@]"
  | FloatArray -> Format.fprintf ppf "@[<hov 1>FloatArray@]"
  | Block None -> Format.fprintf ppf "@[<hov 1>(Block@ ())@]"
  | Block (Some (tag, shapes)) ->
    Format.fprintf ppf "@[<hov 1>(Block@ ((%d@ %a)))@]" tag print_shape_list
      shapes
  | Array s -> Format.fprintf ppf "@[<hov 1>(Array@ %a)@]" print_shapes s
  | Closure -> Format.fprintf ppf "@[<hov 1>Closure@]"
  | Obj -> Format.fprintf ppf "@[<hov 1>Obj@]"
  | Or (s1, s2) ->
    Format.fprintf ppf "@[<hov 1>(Or@ %a@ %a)@]" print_shapes s1 print_shapes s2

and print_shape_list ppf shapes =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
       print_shapes)
    shapes

let rec print_shape_readable fmt (sh : value_shape) =
  match sh with
  | Value -> Format.pp_print_string fmt "*"
  | Imm -> Format.pp_print_string fmt "imm"
  | Nativeint -> Format.pp_print_string fmt "native"
  | Double -> Format.pp_print_string fmt "double"
  | Int64 -> Format.pp_print_string fmt "int64"
  | Int32 -> Format.pp_print_string fmt "int32"
  | String -> Format.pp_print_string fmt "string"
  | Block None -> Format.pp_print_string fmt "block"
  | Block (Some (tag, shapes)) ->
    Format.fprintf fmt "block[%d]%a" tag print_shapes_readable shapes
  | FloatArray -> Format.pp_print_string fmt "float_array"
  | Obj -> Format.pp_print_string fmt "object"
  | Array s -> Format.fprintf fmt "array(%a)" print_shape_readable s
  | Closure -> Format.pp_print_string fmt "closure"
  | Or (s1, s2) ->
    Format.fprintf fmt "%a ∨ %a" print_shape_readable s1 print_shape_readable s2

and print_shapes_readable fmt shapes =
  Format.fprintf fmt "(%a)"
    (Format.pp_print_list print_shape_readable ~pp_sep:(fun fmt () ->
         Format.pp_print_string fmt "; "))
    shapes

(* TODO: check which shapes exceptions should have *)
type fn_value_shapes =
  { arguments : value_shape list;
    return : value_shape
  }

let print_fn_value_shapes ppf { arguments; return } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(arguments@ %a)@]@ @[<hov 1>(return@ %a)@])@]"
    print_shape_list arguments print_shapes return

let print_fn_value_shapes_readable fmt { arguments; return } =
  Format.fprintf fmt "args%a -> %a" print_shapes_readable arguments
    print_shape_readable return

(** An [extfun_desc] describes the information that we know about an external function. To
    enable extensions in the future, we define it as a record of options. This enables
    adding new, optional fields in the future without breaking the serialized form. *)
type extfun_desc =
  { shape : fn_value_shapes option
        (** If the shape is not present, then we fallback on the arity of the C code. *)
  }

let print_extfun_desc ppf { shape } =
  match shape with
  | None -> Format.fprintf ppf "@[<hov 1>()@]"
  | Some shape ->
    Format.fprintf ppf "@[<hov 1>(@[<hov 1>(shape@ %a)@])@]"
      print_fn_value_shapes shape

let print_extfun_desc_readable fmt { shape } =
  match shape with
  | None -> Format.fprintf fmt "*"
  | Some shape -> Format.fprintf fmt "%a" print_fn_value_shapes_readable shape

type extfun =
  { name : string;  (** C name of the function *)
    desc : extfun_desc
  }

let print_ext_fun ppf { name; desc } =
  Format.fprintf ppf "@[<hov 1>(@[<hov 1>(name@ %s)@]@ @[<hov 1>(desc@ %a)@])@]"
    name print_extfun_desc desc

let print_ext_fun_readable fmt { name; desc } =
  Format.fprintf fmt "%s: %a" name print_extfun_desc_readable desc

type extfuns =
  { version : string;
    extfuns : extfun list
  }

let print_extfuns ppf { version; extfuns } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(version@ %s)@]@ @[<hov 1>(extfuns@ (%a))@])@]" version
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
       print_ext_fun)
    extfuns

let print_extfuns_readable fmt exts =
  Format.fprintf fmt "Version %s@\n%a" exts.version
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline print_ext_fun_readable)
    exts.extfuns
