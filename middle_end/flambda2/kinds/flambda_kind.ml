(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let print ppf t =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
end

type t =
  | Value
  | Naked_number of Naked_number_kind.t
  | Region
  | Rec_info

type kind = t

let value = Value

let naked_number number_kind = Naked_number number_kind

let naked_immediate = Naked_number Naked_immediate

let naked_float = Naked_number Naked_float

let naked_int32 = Naked_number Naked_int32

let naked_int64 = Naked_number Naked_int64

let naked_nativeint = Naked_number Naked_nativeint

let region = Region

let rec_info = Rec_info

include Container_types.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare

  let equal t1 t2 = compare t1 t2 = 0

  let hash = Hashtbl.hash

  let print ppf t =
    let colour = Flambda_colours.kind () in
    match t with
    | Value ->
      if Flambda_features.unicode ()
      then
        Format.fprintf ppf "@<0>%s@<1>\u{1d54d}@<0>%s" colour
          (Flambda_colours.normal ())
      else Format.fprintf ppf "Val"
    | Naked_number naked_number_kind ->
      if Flambda_features.unicode ()
      then
        match naked_number_kind with
        | Naked_immediate ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d55a}@<0>%s" colour
            (Flambda_colours.normal ())
        | Naked_float ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d557}@<0>%s" colour
            (Flambda_colours.normal ())
        | Naked_int32 ->
          Format.fprintf ppf
            "@<0>%s@<1>\u{2115}@<1>\u{1d7db}@<1>\u{1d7da}@<0>%s" colour
            (Flambda_colours.normal ())
        | Naked_int64 ->
          Format.fprintf ppf
            "@<0>%s@<1>\u{2115}@<1>\u{1d7de}@<1>\u{1d7dc}@<0>%s" colour
            (Flambda_colours.normal ())
        | Naked_nativeint ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{2115}@<0>%s" colour
            (Flambda_colours.normal ())
      else
        Format.fprintf ppf "(Naked_number %a)" Naked_number_kind.print
          naked_number_kind
    | Region ->
      if Flambda_features.unicode ()
      then
        Format.fprintf ppf "@<0>%s@<1>\u{1d53d}@<1>\u{1d558}@<0>%s" colour
          (Flambda_colours.normal ())
      else Format.fprintf ppf "Region"
    | Rec_info ->
      if Flambda_features.unicode ()
      then
        Format.fprintf ppf "@<0>%s@<1>\u{211d}@<0>%s" colour
          (Flambda_colours.normal ())
      else Format.fprintf ppf "Rec"
end)

let is_value t =
  match t with Value -> true | Naked_number _ | Region | Rec_info -> false

let is_naked_float t =
  match t with
  | Naked_number Naked_float -> true
  | Value
  | Naked_number (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint)
  | Region | Rec_info ->
    false

module Standard_int = struct
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value
    | Naked_immediate -> Naked_number Naked_immediate
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

  include Container_types.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare = Stdlib.compare

    let equal t1 t2 = compare t1 t2 = 0

    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Tagged_immediate -> Format.pp_print_string ppf "tagged_immediate"
    | Naked_immediate -> Format.pp_print_string ppf "naked_immediate"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Standard_int_or_float = struct
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value
    | Naked_immediate -> Naked_number Naked_immediate
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

  include Container_types.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare = Stdlib.compare

    let equal t1 t2 = compare t1 t2 = 0

    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Tagged_immediate -> Format.pp_print_string ppf "tagged_immediate"
    | Naked_immediate -> Format.pp_print_string ppf "naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Boxable_number = struct
  type t =
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let unboxed_kind t : kind =
    match t with
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

  include Container_types.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare = Stdlib.compare

    let equal t1 t2 = compare t1 t2 = 0

    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"

  let print_lowercase_short ppf t =
    match t with
    | Naked_float -> Format.pp_print_string ppf "float"
    | Naked_int32 -> Format.pp_print_string ppf "int32"
    | Naked_int64 -> Format.pp_print_string ppf "int64"
    | Naked_nativeint -> Format.pp_print_string ppf "nativeint"
end

module With_subkind = struct
  module Subkind = struct
    type t =
      | Anything
      | Boxed_float
      | Boxed_int32
      | Boxed_int64
      | Boxed_nativeint
      | Tagged_immediate
      | Block of
          { tag : Tag.t;
            fields : t list
          }
      | Float_block of { num_fields : int }
      | Float_array
      | Immediate_array
      | Value_array
      | Generic_array

    let rec compatible (t : t) ~(when_used_at : t) =
      match t, when_used_at with
      (* Simple equality cases: *)
      | Anything, Anything
      | Boxed_float, Boxed_float
      | Boxed_int32, Boxed_int32
      | Boxed_int64, Boxed_int64
      | Boxed_nativeint, Boxed_nativeint
      | Tagged_immediate, Tagged_immediate
      | Float_array, Float_array
      | Immediate_array, Immediate_array
      | Value_array, Value_array
      | Generic_array, Generic_array ->
        true
      | ( Block { tag = t1; fields = fields1 },
          Block { tag = t2; fields = fields2 } ) ->
        Tag.equal t1 t2
        && List.length fields1 = List.length fields2
        && List.for_all2
             (fun d when_used_at -> compatible d ~when_used_at)
             fields1 fields2
      | ( Float_block { num_fields = num_fields1 },
          Float_block { num_fields = num_fields2 } ) ->
        num_fields1 = num_fields2
      (* Subkinds of [Value] may always be used at [Value] (but not the
         converse): *)
      | ( ( Block _ | Float_block _ | Float_array | Immediate_array
          | Value_array | Generic_array | Boxed_float | Boxed_int32
          | Boxed_int64 | Boxed_nativeint | Tagged_immediate ),
          Anything ) ->
        true
      (* All specialised array kinds may be used at kind [Generic_array], and
         [Immediate_array] may be used at kind [Value_array]: *)
      | (Float_array | Immediate_array | Value_array), Generic_array
      | Immediate_array, Value_array ->
        true
      (* All other combinations are incompatible: *)
      | ( ( Anything | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
          | Tagged_immediate | Block _ | Float_block _ | Float_array
          | Immediate_array | Value_array | Generic_array ),
          _ ) ->
        false

    include Container_types.Make (struct
      type nonrec t = t

      let rec print ppf t =
        let colour = Flambda_colours.subkind () in
        match t with
        | Anything -> ()
        | Tagged_immediate ->
          Format.fprintf ppf "@<0>%s=tagged_@<1>\u{2115}@<1>\u{1d55a}@<0>%s"
            colour
            (Flambda_colours.normal ())
        | Boxed_float ->
          Format.fprintf ppf "@<0>%s=boxed_@<1>\u{2115}@<1>\u{1d557}@<0>%s"
            colour
            (Flambda_colours.normal ())
        | Boxed_int32 ->
          Format.fprintf ppf
            "@<0>%s=boxed_@<1>\u{2115}@<1>\u{1d7db}@<1>\u{1d7da}@<0>%s" colour
            (Flambda_colours.normal ())
        | Boxed_int64 ->
          Format.fprintf ppf
            "@<0>%s=boxed_@<1>\u{2115}@<1>\u{1d7de}@<1>\u{1d7dc}@<0>%s" colour
            (Flambda_colours.normal ())
        | Boxed_nativeint ->
          Format.fprintf ppf "@<0>%s=boxed_@<1>\u{2115}@<1>\u{2115}@<0>%s"
            colour
            (Flambda_colours.normal ())
        | Block { tag; fields } ->
          Format.fprintf ppf "@<0>%s=Block{%a: %a}@<0>%s" colour Tag.print tag
            (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
            fields
            (Flambda_colours.normal ())
        | Float_block { num_fields } ->
          Format.fprintf ppf "@<0>%s=Float_block(%d)@<0>%s" colour num_fields
            (Flambda_colours.normal ())
        | Float_array ->
          Format.fprintf ppf "@<0>%s=Float_array@<0>%s" colour
            (Flambda_colours.normal ())
        | Immediate_array ->
          Format.fprintf ppf "@<0>%s=Immediate_array@<0>%s" colour
            (Flambda_colours.normal ())
        | Value_array ->
          Format.fprintf ppf "@<0>%s=Value_array@<0>%s" colour
            (Flambda_colours.normal ())
        | Generic_array ->
          Format.fprintf ppf "@<0>%s=Generic_array@<0>%s" colour
            (Flambda_colours.normal ())

      let compare = Stdlib.compare

      let equal t1 t2 = compare t1 t2 = 0

      let hash = Hashtbl.hash
    end)
  end

  type kind = t

  type t =
    { kind : kind;
      subkind : Subkind.t
    }

  let create (kind : kind) (subkind : Subkind.t) =
    (match kind with
    | Value -> ()
    | Naked_number _ | Region | Rec_info -> (
      match subkind with
      | Anything -> ()
      | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
      | Tagged_immediate | Block _ | Float_block _ | Float_array
      | Immediate_array | Value_array | Generic_array ->
        Misc.fatal_errorf "Subkind %a is not valid for kind %a" Subkind.print
          subkind print kind));
    { kind; subkind }

  let kind t = t.kind

  let subkind t = t.subkind

  let any_value = create value Anything

  let naked_immediate = create naked_immediate Anything

  let naked_float = create naked_float Anything

  let naked_int32 = create naked_int32 Anything

  let naked_int64 = create naked_int64 Anything

  let naked_nativeint = create naked_nativeint Anything

  let boxed_float = create value Boxed_float

  let boxed_int32 = create value Boxed_int32

  let boxed_int64 = create value Boxed_int64

  let boxed_nativeint = create value Boxed_nativeint

  let tagged_immediate = create value Tagged_immediate

  let rec_info = create rec_info Anything

  let float_array = create value Float_array

  let immediate_array = create value Immediate_array

  let value_array = create value Value_array

  let generic_array = create value Generic_array

  let block tag fields =
    if List.exists (fun t -> not (equal t.kind Value)) fields
    then
      Misc.fatal_error
        "Block with fields of non-Value kind (use \
         [Flambda_kind.With_subkind.float_block] for float records)";
    let fields = List.map (fun t -> t.subkind) fields in
    create value (Block { tag; fields })

  let float_block ~num_fields = create value (Float_block { num_fields })

  let of_naked_number_kind (naked_number_kind : Naked_number_kind.t) =
    match naked_number_kind with
    | Naked_immediate -> naked_immediate
    | Naked_float -> naked_float
    | Naked_int32 -> naked_int32
    | Naked_int64 -> naked_int64
    | Naked_nativeint -> naked_nativeint

  let rec from_lambda (vk : Lambda.value_kind) =
    match vk with
    | Pgenval -> any_value
    | Pfloatval -> boxed_float
    | Pboxedintval Pint32 -> boxed_int32
    | Pboxedintval Pint64 -> boxed_int64
    | Pboxedintval Pnativeint -> boxed_nativeint
    | Pintval -> tagged_immediate
    | Pblock { tag; fields } ->
      (* If we have [Obj.double_array_tag] here, this is always an all-float
         block, not an array. *)
      if tag = Obj.double_array_tag
      then float_block ~num_fields:(List.length fields)
      else block (Tag.create_exn tag) (List.map from_lambda fields)
    | Parrayval Pfloatarray -> float_array
    | Parrayval Pintarray -> immediate_array
    | Parrayval Paddrarray -> value_array
    | Parrayval Pgenarray -> generic_array

  include Container_types.Make (struct
    type nonrec t = t

    let print ppf { kind; subkind } =
      match kind, subkind with
      | _, Anything -> print ppf kind
      | Value, subkind ->
        Format.fprintf ppf "@[%a%a@]" print kind Subkind.print subkind
      | ( (Naked_number _ | Region | Rec_info),
          ( Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
          | Tagged_immediate | Block _ | Float_block _ | Float_array
          | Immediate_array | Value_array | Generic_array ) ) ->
        assert false
    (* see [create] *)

    let compare { kind = kind1; subkind = subkind1 }
        { kind = kind2; subkind = subkind2 } =
      let c = compare kind1 kind2 in
      if c <> 0 then c else Subkind.compare subkind1 subkind2

    let equal t1 t2 = compare t1 t2 = 0

    let hash { kind; subkind } = Hashtbl.hash (hash kind, Subkind.hash subkind)
  end)

  let compatible t ~when_used_at =
    Subkind.compatible t.subkind ~when_used_at:when_used_at.subkind

  let has_useful_subkind_info t =
    match t.subkind with
    | Anything -> false
    | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
    | Tagged_immediate | Block _ | Float_block _ | Float_array | Immediate_array
    | Value_array | Generic_array ->
      true
end
