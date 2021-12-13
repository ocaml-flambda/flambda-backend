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

type value = private Value

type empty_naked_immediate = private Naked_immediate

type empty_naked_float = private Naked_float

type empty_naked_int32 = private Naked_int32

type empty_naked_int64 = private Naked_int64

type empty_naked_nativeint = private Naked_nativeint

type fabricated = private Fabricated

type rec_info = private Rec_info

type naked_immediate = empty_naked_immediate * Targetint_31_63.Set.t

type naked_float = empty_naked_float * Numeric_types.Float_by_bit_pattern.Set.t

type naked_int32 = empty_naked_int32 * Numeric_types.Int32.Set.t

type naked_int64 = empty_naked_int64 * Numeric_types.Int64.Set.t

type naked_nativeint = empty_naked_nativeint * Targetint_32_64.Set.t

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

  let to_int t =
    match t with
    | Naked_immediate -> 0
    | Naked_float -> 1
    | Naked_int32 -> 2
    | Naked_int64 -> 3
    | Naked_nativeint -> 4

  let compare t1 t2 = Int.compare (to_int t1) (to_int t2)

  let equal t1 t2 = compare t1 t2 = 0
end

type t =
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated
  | Rec_info

type kind = t

let value = Value

let naked_immediate = Naked_number Naked_immediate

let naked_float = Naked_number Naked_float

let naked_int32 = Naked_number Naked_int32

let naked_int64 = Naked_number Naked_int64

let naked_nativeint = Naked_number Naked_nativeint

let fabricated = Fabricated

let rec_info = Rec_info

let unit = Value

let unicode = true (* CR mshinwell: move elsewhere *)

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2
    then 0
    else
      match t1, t2 with
      | Value, Value -> 0
      | Naked_number n1, Naked_number n2 -> Naked_number_kind.compare n1 n2
      | Fabricated, Fabricated -> 0
      | Rec_info, Rec_info -> 0
      | Value, _ -> -1
      | _, Value -> 1
      | Naked_number _, _ -> -1
      | _, Naked_number _ -> 1
      | Fabricated, _ -> -1
      | _, Fabricated -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let hash = Hashtbl.hash

  let [@ocamlformat "disable"] print ppf t =
    let colour = Flambda_colours.kind () in
    match t with
    | Value ->
      if unicode then
        Format.fprintf ppf "@<0>%s@<1>\u{1d54d}@<0>%s" colour
          (Flambda_colours.normal ())
      else
        Format.fprintf ppf "Val"
    | Naked_number naked_number_kind ->
      if unicode then begin
        match naked_number_kind with
        | Naked_immediate ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d55a}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_float ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d557}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_int32 ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d7db}@<1>\u{1d7da}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_int64 ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d7de}@<1>\u{1d7dc}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_nativeint ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{2115}@<0>%s"
            colour (Flambda_colours.normal ())
      end else begin
        Format.fprintf ppf "(Naked_number %a)"
          Naked_number_kind.print naked_number_kind
      end
    | Fabricated ->
      if unicode then
        Format.fprintf ppf "@<0>%s@<1>\u{1d53d}@<0>%s"
          colour (Flambda_colours.normal ())
      else
        Format.fprintf ppf "Fab"
    | Rec_info ->
      if unicode then
        Format.fprintf ppf "@<0>%s@<1>\u{211d}@<0>%s"
          colour (Flambda_colours.normal ())
      else
        Format.fprintf ppf "Rec"
end)

let is_value t =
  match t with Value -> true | Naked_number _ | Fabricated | Rec_info -> false

let is_naked_float t =
  match t with
  | Naked_number Naked_float -> true
  | Value
  | Naked_number (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint)
  | Fabricated | Rec_info ->
    false

module Standard_int = struct
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_int t =
    match t with
    | Tagged_immediate -> 0
    | Naked_immediate -> 1
    | Naked_int32 -> 2
    | Naked_int64 -> 3
    | Naked_nativeint -> 4

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value
    | Naked_immediate -> Naked_number Naked_immediate
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare t1 t2 = to_int t1 - to_int t2

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

  let to_int t =
    match t with
    | Tagged_immediate -> 0
    | Naked_immediate -> 1
    | Naked_float -> 2
    | Naked_int32 -> 3
    | Naked_int64 -> 4
    | Naked_nativeint -> 5

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

    let [@ocamlformat "disable"] print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare t1 t2 = to_int t1 - to_int t2

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
    | Untagged_immediate

  let to_int t =
    match t with
    | Naked_float -> 0
    | Naked_int32 -> 1
    | Naked_int64 -> 2
    | Naked_nativeint -> 3
    | Untagged_immediate -> 4

  let to_kind t : kind =
    match t with
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint
    | Untagged_immediate -> Naked_number Naked_immediate

  let of_naked_number_kind k : t =
    match (k : Naked_number_kind.t) with
    | Naked_immediate -> Untagged_immediate
    | Naked_float -> Naked_float
    | Naked_int32 -> Naked_int32
    | Naked_int64 -> Naked_int64
    | Naked_nativeint -> Naked_nativeint

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf t =
      match t with
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
      | Untagged_immediate -> Format.pp_print_string ppf "Untagged_immediate"

    let compare t1 t2 = to_int t1 - to_int t2

    let equal t1 t2 = t1 == t2

    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
    | Untagged_immediate -> Format.pp_print_string ppf "untagged_immediate"

  let print_lowercase_short ppf t =
    match t with
    | Naked_float -> Format.pp_print_string ppf "float"
    | Naked_int32 -> Format.pp_print_string ppf "int32"
    | Naked_int64 -> Format.pp_print_string ppf "int64"
    | Naked_nativeint -> Format.pp_print_string ppf "nativeint"
    | Untagged_immediate -> Format.pp_print_string ppf "untagged_imm"
end

module Naked_number = struct
  type 'k t =
    | Naked_immediate : naked_immediate t
    | Naked_float : naked_float t
    | Naked_int32 : naked_int32 t
    | Naked_int64 : naked_int64 t
    | Naked_nativeint : naked_nativeint t

  let [@ocamlformat "disable"] print (type a) ppf (t : a t) =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
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

    include Container_types.Make (struct
      type nonrec t = t

      let [@ocamlformat "disable"] rec print ppf t =
        let colour = Flambda_colours.subkind () in
        match t with
        | Anything -> ()
        | Tagged_immediate ->
          Format.fprintf ppf "@<0>%s=tagged_@<1>\u{2115}@<1>\u{1d55a}@<0>%s"
            colour (Flambda_colours.normal ())
        | Boxed_float ->
          Format.fprintf ppf "@<0>%s=boxed_@<1>\u{2115}@<1>\u{1d557}@<0>%s"
            colour (Flambda_colours.normal ())
        | Boxed_int32 ->
          Format.fprintf ppf
            "@<0>%s=boxed_@<1>\u{2115}@<1>\u{1d7db}@<1>\u{1d7da}@<0>%s"
            colour (Flambda_colours.normal ())
        | Boxed_int64 ->
          Format.fprintf ppf
            "@<0>%s=boxed_@<1>\u{2115}@<1>\u{1d7de}@<1>\u{1d7dc}@<0>%s"
            colour (Flambda_colours.normal ())
        | Boxed_nativeint ->
          Format.fprintf ppf "@<0>%s=boxed_@<1>\u{2115}@<1>\u{2115}@<0>%s"
            colour (Flambda_colours.normal ())
        | Block { tag; fields } ->
          Format.fprintf ppf "@<0>%s=Block{%a: %a}@<0>%s"
            colour
            Tag.print tag
            (Format.pp_print_list ~pp_sep:Format.pp_print_space print) fields
            (Flambda_colours.normal ())
        | Float_block { num_fields; } ->
          Format.fprintf ppf "@<0>%s=Float_block(%d)@<0>%s"
            colour
            num_fields
            (Flambda_colours.normal ())
        | Float_array ->
          Format.fprintf ppf "@<0>%s=Float_array@<0>%s"
            colour
            (Flambda_colours.normal ())
        | Immediate_array ->
          Format.fprintf ppf "@<0>%s=Immediate_array@<0>%s"
            colour
            (Flambda_colours.normal ())
        | Value_array ->
          Format.fprintf ppf "@<0>%s=Value_array@<0>%s"
            colour
            (Flambda_colours.normal ())
        | Generic_array ->
          Format.fprintf ppf "@<0>%s=Generic_array@<0>%s"
            colour
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
    begin
      match kind with
      | Value -> ()
      | Naked_number _ | Fabricated | Rec_info -> (
        match subkind with
        | Anything -> ()
        | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
        | Tagged_immediate | Block _ | Float_block _ | Float_array
        | Immediate_array | Value_array | Generic_array ->
          Misc.fatal_errorf "Only subkind %a is valid for kind %a" Subkind.print
            subkind print kind)
    end;
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

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf { kind; subkind; } =
      match kind, subkind with
      | _, Anything -> print ppf kind
      | Value, subkind ->
        Format.fprintf ppf "@[%a%a@]"
          print kind
          Subkind.print subkind
      | (Naked_number _ | Fabricated | Rec_info),
        (Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
          | Tagged_immediate | Block _ | Float_block _ | Float_array | Immediate_array | Value_array | Generic_array) ->
        assert false
    (* see [create] *)

    let compare { kind = kind1; subkind = subkind1 }
        { kind = kind2; subkind = subkind2 } =
      let c = compare kind1 kind2 in
      if c <> 0 then c else Subkind.compare subkind1 subkind2

    let equal t1 t2 = compare t1 t2 = 0

    let hash { kind; subkind } = Hashtbl.hash (hash kind, Subkind.hash subkind)
  end)

  type descr =
    | Any_value
    | Naked_number of Naked_number_kind.t
    | Boxed_float
    | Boxed_int32
    | Boxed_int64
    | Boxed_nativeint
    | Tagged_immediate
    | Rec_info
    | Block of
        { tag : Tag.t;
          fields : descr list
        }
    | Float_block of { num_fields : int }
    | Float_array
    | Immediate_array
    | Value_array
    | Generic_array

  let rec subkind_descr (t : Subkind.t) : descr =
    match t with
    | Anything -> Any_value
    | Tagged_immediate -> Tagged_immediate
    | Boxed_float -> Boxed_float
    | Boxed_int32 -> Boxed_int32
    | Boxed_int64 -> Boxed_int64
    | Boxed_nativeint -> Boxed_nativeint
    | Block { tag; fields } ->
      Block { tag; fields = List.map subkind_descr fields }
    | Float_block { num_fields } -> Float_block { num_fields }
    | Float_array -> Float_array
    | Immediate_array -> Immediate_array
    | Value_array -> Value_array
    | Generic_array -> Generic_array

  let descr t : descr =
    match t.kind with
    | Value -> subkind_descr t.subkind
    | Naked_number naked_number_kind -> Naked_number naked_number_kind
    | Rec_info -> Rec_info
    | Fabricated -> Misc.fatal_error "Not implemented"

  let rec compatible_descr descr ~when_used_at =
    match descr, when_used_at with
    (* Simple equality cases: *)
    | Naked_number nn1, Naked_number nn2 -> Naked_number_kind.equal nn1 nn2
    | Any_value, Any_value
    | Boxed_float, Boxed_float
    | Boxed_int32, Boxed_int32
    | Boxed_int64, Boxed_int64
    | Boxed_nativeint, Boxed_nativeint
    | Tagged_immediate, Tagged_immediate
    | Float_array, Float_array
    | Immediate_array, Immediate_array
    | Value_array, Value_array
    | Generic_array, Generic_array
    | Rec_info, Rec_info ->
      true
    | Block { tag = t1; fields = fields1 }, Block { tag = t2; fields = fields2 }
      ->
      Tag.equal t1 t2
      && List.length fields1 = List.length fields2
      && List.for_all2
           (fun d when_used_at -> compatible_descr d ~when_used_at)
           fields1 fields2
    | ( Float_block { num_fields = num_fields1 },
        Float_block { num_fields = num_fields2 } ) ->
      num_fields1 = num_fields2
    (* Subkinds of [Value] may always be used at [Value], but not the
       converse: *)
    | (Block _ | Float_block _), Any_value
    | Float_array, Any_value
    | Immediate_array, Any_value
    | Value_array, Any_value
    | Generic_array, Any_value
    | Boxed_float, Any_value
    | Boxed_int32, Any_value
    | Boxed_int64, Any_value
    | Boxed_nativeint, Any_value
    | Tagged_immediate, Any_value
    (* All specialised array kinds may be used at kind [Generic_array], and
       [Immediate_array] may be used at kind [Value_array] *)
    | Float_array, Generic_array
    | Immediate_array, Value_array
    | Immediate_array, Generic_array
    | Value_array, Generic_array ->
      true
    (* All other combinations are incompatible. *)
    | ( ( Any_value | Naked_number _ | Boxed_float | Boxed_int32 | Boxed_int64
        | Boxed_nativeint | Tagged_immediate | Block _ | Float_block _
        | Float_array | Immediate_array | Value_array | Generic_array | Rec_info
          ),
        _ ) ->
      false

  let compatible t ~when_used_at =
    compatible_descr (descr t) ~when_used_at:(descr when_used_at)

  let has_useful_subkind_info t =
    match t.subkind with
    | Anything -> false
    | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
    | Tagged_immediate | Block _ | Float_block _ | Float_array | Immediate_array
    | Value_array | Generic_array ->
      true
end
