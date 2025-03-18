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

type type_shape =
  | Any  (** anything of type [value] *)
  | Imm  (** immediate, tagged with a one at the end *)
  | Nativeint
      (** block of a native word integer, e.g., 64-bit integer on amd64 target *)
  | Double  (** block of a native double *)
  | Int64  (** block of a 64-bit integer *)
  | Int32  (** block of a 32-bit integer *)
  | String
      (** block of a char pointer with a size, representing both Bytes.t and String.t *)
  | FloatArray  (** block containing native doubles *)
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
      (** Block with tag 0 and a fixed size (not known statically). The shape of the
          elements is given by the argument. *)
  | Closure  (** Block with closure tag. *)
  | Obj  (** Block with object tag. *)
  | Or of type_shape * type_shape
      (** Disjunction between two shapes for (e.g., variant types) *)

type fn_type_shapes =
  { arguments : type_shape list;
    return : type_shape
  }

(** An [extfun_desc] describes the information that we know about an external function. To
    enable extensions in the future, we define it as a record of options. This enables
    adding new, optional fields in the future without breaking the serialized form. *)
type extfun_desc =
  { shape : fn_type_shapes option
        (** If the shape is not present, then we fallback on the arity of the C code. *)
  }

type extfun =
  { name : string;  (** C name of the function *)
    desc : extfun_desc
  }

type extfuns =
  { version : string;
    extfuns : extfun list
  }

(* functions for serializing external functions *)
val print_extfuns : Format.formatter -> extfuns -> unit

val print_extfuns_readable : Format.formatter -> extfuns -> unit
