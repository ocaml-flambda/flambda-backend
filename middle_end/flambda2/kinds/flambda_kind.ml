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

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float32
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
    | Naked_vec128

  let print ppf t =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float32 -> Format.pp_print_string ppf "Naked_float32"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
    | Naked_vec128 -> Format.pp_print_string ppf "Naked_vec128"

  let equal t1 t2 =
    match t1, t2 with
    | Naked_immediate, Naked_immediate -> true
    | Naked_float32, Naked_float32 -> true
    | Naked_float, Naked_float -> true
    | Naked_int32, Naked_int32 -> true
    | Naked_int64, Naked_int64 -> true
    | Naked_nativeint, Naked_nativeint -> true
    | Naked_vec128, Naked_vec128 -> true
    | ( ( Naked_immediate | Naked_float32 | Naked_float | Naked_int32
        | Naked_int64 | Naked_nativeint | Naked_vec128 ),
        _ ) ->
      false
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

let naked_float32 = Naked_number Naked_float32

let naked_int32 = Naked_number Naked_int32

let naked_int64 = Naked_number Naked_int64

let naked_nativeint = Naked_number Naked_nativeint

let naked_vec128 = Naked_number Naked_vec128

let region = Region

let rec_info = Rec_info

let to_lambda (t : t) : Lambda.layout =
  match t with
  | Value -> Pvalue Pgenval
  | Naked_number Naked_immediate ->
    Misc.fatal_error "Can't convert kind [Naked_immediate] to lambda layout"
  | Naked_number Naked_float -> Punboxed_float Pfloat64
  | Naked_number Naked_float32 -> Punboxed_float Pfloat32
  | Naked_number Naked_int32 -> Punboxed_int Pint32
  | Naked_number Naked_int64 -> Punboxed_int Pint64
  | Naked_number Naked_nativeint -> Punboxed_int Pnativeint
  | Naked_number Naked_vec128 -> Punboxed_vector Pvec128
  | Region -> Misc.fatal_error "Can't convert kind [Region] to lambda layout"
  | Rec_info ->
    Misc.fatal_error "Can't convert kind [Rec_info] to lambda layout"

include Container_types.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare

  let equal t1 t2 = compare t1 t2 = 0

  let hash = Hashtbl.hash

  let print ppf t =
    let colour = Flambda_colours.kind in
    match t with
    | Value ->
      if Flambda_features.unicode ()
      then Format.fprintf ppf "%t@<1>\u{1d54d}%t" colour Flambda_colours.pop
      else Format.fprintf ppf "Val"
    | Naked_number naked_number_kind ->
      if Flambda_features.unicode ()
      then
        match naked_number_kind with
        | Naked_immediate ->
          Format.fprintf ppf "%t@<1>\u{2115}@<1>\u{1d55a}%t" colour
            Flambda_colours.pop
        | Naked_float ->
          Format.fprintf ppf "%t@<1>\u{2115}@<1>\u{1d557}%t" colour
            Flambda_colours.pop
        | Naked_float32 ->
          Format.fprintf ppf "%t@<1>\u{2115}@<1>\u{1d557}32%t" colour
            Flambda_colours.pop
        | Naked_int32 ->
          Format.fprintf ppf "%t@<1>\u{2115}@<1>\u{1d7db}@<1>\u{1d7da}%t" colour
            Flambda_colours.pop
        | Naked_int64 ->
          Format.fprintf ppf "%t@<1>\u{2115}@<1>\u{1d7de}@<1>\u{1d7dc}%t" colour
            Flambda_colours.pop
        | Naked_nativeint ->
          Format.fprintf ppf "%t@<1>\u{2115}@<1>\u{2115}%t" colour
            Flambda_colours.pop
        | Naked_vec128 ->
          Format.fprintf ppf "%t@<1>\u{2115}@<1>\u{1d54d}128%t" colour
            Flambda_colours.pop
      else
        Format.fprintf ppf "(Naked_number %a)" Naked_number_kind.print
          naked_number_kind
    | Region ->
      if Flambda_features.unicode ()
      then
        Format.fprintf ppf "%t@<1>\u{211d}@<1>\u{1d558}%t" colour
          Flambda_colours.pop
      else Format.fprintf ppf "Region"
    | Rec_info ->
      if Flambda_features.unicode ()
      then Format.fprintf ppf "%t@<1>\u{211d}%t" colour Flambda_colours.pop
      else Format.fprintf ppf "Rec"
end)

let is_value t =
  match t with Value -> true | Naked_number _ | Region | Rec_info -> false

let is_naked_float t =
  match t with
  | Naked_number Naked_float -> true
  | Value
  | Naked_number
      ( Naked_immediate | Naked_float32 | Naked_int32 | Naked_int64
      | Naked_nativeint | Naked_vec128 )
  | Region | Rec_info ->
    false

type flat_suffix_element =
  | Tagged_immediate
  | Naked_float
  | Naked_float32
  | Naked_int32
  | Naked_int64
  | Naked_nativeint
  | Naked_vec128

module Flat_suffix_element0 = struct
  type t = flat_suffix_element

  let kind t =
    match t with
    | Tagged_immediate -> value
    | Naked_float -> naked_float
    | Naked_float32 -> naked_float32
    | Naked_int32 -> naked_int32
    | Naked_int64 -> naked_int64
    | Naked_nativeint -> naked_nativeint
    | Naked_vec128 -> naked_vec128

  let naked_float = Naked_float

  let compare = Stdlib.compare

  let equal = Stdlib.( = )

  let size_in_words = function
    | Tagged_immediate | Naked_float | Naked_float32 | Naked_int32 | Naked_int64
    | Naked_nativeint ->
      1
    | Naked_vec128 -> 2

  let print ppf t =
    match t with
    | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_float32 -> Format.pp_print_string ppf "Naked_float32"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
    | Naked_vec128 -> Format.pp_print_string ppf "Naked_vec128"

  let from_lambda (elt : Lambda.flat_element) =
    match elt with
    | Imm -> Tagged_immediate
    | Float_boxed | Float64 -> Naked_float
    | Float32 -> Naked_float32
    | Bits32 -> Naked_int32
    | Bits64 -> Naked_int64
    | Vec128 -> Naked_vec128
    | Word -> Naked_nativeint
end

module Mixed_block_shape = struct
  type kind = t

  type t =
    { value_prefix_size : int;
      flat_suffix : Flat_suffix_element0.t array;
      field_kinds : kind array
          (* [field_kinds] is uniquely determined by [flat_suffix]. The kinds
             are the thing used during most of Flambda 2, but the [flat_suffix]
             is required for compilation to Cmm. We could also use a kind with
             subkind, but that would require significant restructuring in this
             file, and would provide an overly-permissive type in the face of
             the various restrictions as to what suffix elements are
             permitted. *)
    }

  let value_prefix_size t = t.value_prefix_size

  let flat_suffix t = t.flat_suffix

  let field_kinds t = t.field_kinds

  let offset_in_words t index =
    if index <= t.value_prefix_size
    then index
    else
      let o = ref t.value_prefix_size in
      let flat = index - t.value_prefix_size in
      for i = 0 to flat - 1 do
        o := !o + Flat_suffix_element0.size_in_words t.flat_suffix.(i)
      done;
      !o

  (* This function has two meanings. The first is to say whether two shapes are
     equivalent. The second is to tell whether two shapes are compatible.
     Currently this matches with equivalence, but if we introduce subkinds this
     will have to be split into two functions. *)
  let equal
      { value_prefix_size = value_prefix_size1;
        flat_suffix = flat_suffix1;
        field_kinds = _
      }
      { value_prefix_size = value_prefix_size2;
        flat_suffix = flat_suffix2;
        field_kinds = _
      } =
    Int.equal value_prefix_size1 value_prefix_size2
    && Int.equal (Array.length flat_suffix1) (Array.length flat_suffix2)
    && Array.for_all2 Flat_suffix_element0.equal flat_suffix1 flat_suffix2

  let print ppf t =
    Format.fprintf ppf "@[<hov 1>((fields@ %a)@ (value_prefix_size@ %d))@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Flat_suffix_element0.print)
      (Array.to_list t.flat_suffix)
      t.value_prefix_size

  let compare
      { value_prefix_size = value_prefix_size1;
        flat_suffix = flat_suffix1;
        field_kinds = _
      }
      { value_prefix_size = value_prefix_size2;
        flat_suffix = flat_suffix2;
        field_kinds = _
      } =
    let c = Int.compare value_prefix_size1 value_prefix_size2 in
    if c <> 0
    then c
    else
      Misc.Stdlib.Array.compare Flat_suffix_element0.compare flat_suffix1
        flat_suffix2

  let from_lambda ({ value_prefix_len; flat_suffix } : Lambda.mixed_block_shape)
      =
    let value_prefix_kinds = Array.init value_prefix_len (fun _ -> value) in
    let flat_suffix = Array.map Flat_suffix_element0.from_lambda flat_suffix in
    let flat_suffix_kinds = Array.map Flat_suffix_element0.kind flat_suffix in
    { flat_suffix;
      value_prefix_size = value_prefix_len;
      field_kinds = Array.concat [value_prefix_kinds; flat_suffix_kinds]
    }
end

module Scannable_block_shape = struct
  type t =
    | Value_only
    | Mixed_record of Mixed_block_shape.t

  (* Some users rely on shapes not being compatible if they're not equal. *)
  let equal t1 t2 =
    match t1, t2 with
    | Value_only, Value_only -> true
    | Mixed_record t1, Mixed_record t2 -> Mixed_block_shape.equal t1 t2
    | (Value_only | Mixed_record _), _ -> false

  let compare t1 t2 =
    match t1, t2 with
    | Value_only, Value_only -> 0
    | Value_only, _ -> -1
    | _, Value_only -> 1
    | Mixed_record kinds1, Mixed_record kinds2 ->
      Mixed_block_shape.compare kinds1 kinds2

  let print ppf t =
    match t with
    | Value_only -> Format.fprintf ppf "Value_only"
    | Mixed_record mixed ->
      Format.fprintf ppf "(Mixed_record@ %a)" Mixed_block_shape.print mixed

  let element_kind t index =
    match t with
    | Value_only -> Value
    | Mixed_record t -> (Mixed_block_shape.field_kinds t).(index)
end

module Block_shape = struct
  type t =
    | Scannable of Scannable_block_shape.t
    | Float_record

  let equal t1 t2 =
    match t1, t2 with
    | Scannable shape1, Scannable shape2 ->
      Scannable_block_shape.equal shape1 shape2
    | Float_record, Float_record -> true
    | (Scannable _ | Float_record), _ -> false

  let compare t1 t2 =
    match t1, t2 with
    | Scannable shape1, Scannable shape2 ->
      Scannable_block_shape.compare shape1 shape2
    | Scannable _, Float_record -> -1
    | Float_record, Scannable _ -> 1
    | Float_record, Float_record -> 0

  let print ppf t =
    match t with
    | Scannable shape ->
      Format.fprintf ppf "(Scannable@ %a)" Scannable_block_shape.print shape
    | Float_record -> Format.fprintf ppf "Float_record"

  let element_kind t index =
    match t with
    | Scannable shape -> Scannable_block_shape.element_kind shape index
    | Float_record -> Naked_number Naked_float
end

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
    | Naked_float32
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let of_standard_int (t : Standard_int.t) : t =
    match t with
    | Tagged_immediate -> Tagged_immediate
    | Naked_immediate -> Naked_immediate
    | Naked_int32 -> Naked_int32
    | Naked_int64 -> Naked_int64
    | Naked_nativeint -> Naked_nativeint

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value
    | Naked_immediate -> Naked_number Naked_immediate
    | Naked_float32 -> Naked_number Naked_float32
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
      | Naked_float32 -> Format.pp_print_string ppf "Naked_float32"
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
    | Naked_float32 -> Format.pp_print_string ppf "naked_float32"
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Boxable_number = struct
  type t =
    | Naked_float32
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
    | Naked_vec128

  let unboxed_kind t : kind =
    match t with
    | Naked_float32 -> Naked_number Naked_float32
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint
    | Naked_vec128 -> Naked_number Naked_vec128

  let primitive_kind t : Primitive.boxed_integer =
    match t with
    | Naked_vec128 | Naked_float | Naked_float32 -> assert false
    | Naked_int32 -> Pint32
    | Naked_int64 -> Pint64
    | Naked_nativeint -> Pnativeint

  include Container_types.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Naked_float32 -> Format.pp_print_string ppf "Naked_float32"
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
      | Naked_vec128 -> Format.pp_print_string ppf "Naked_vec128"

    let compare = Stdlib.compare

    let equal t1 t2 = compare t1 t2 = 0

    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Naked_float32 -> Format.pp_print_string ppf "naked_float32"
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
    | Naked_vec128 -> Format.fprintf ppf "naked_vec128"

  let print_lowercase_short ppf t =
    match t with
    | Naked_float32 -> Format.pp_print_string ppf "float32"
    | Naked_float -> Format.pp_print_string ppf "float"
    | Naked_int32 -> Format.pp_print_string ppf "int32"
    | Naked_int64 -> Format.pp_print_string ppf "int64"
    | Naked_nativeint -> Format.pp_print_string ppf "nativeint"
    | Naked_vec128 -> Format.pp_print_string ppf "vec128"
end

module With_subkind = struct
  module Subkind = struct
    type t =
      | Anything
      | Boxed_float32
      | Boxed_float
      | Boxed_int32
      | Boxed_int64
      | Boxed_nativeint
      | Boxed_vec128
      | Tagged_immediate
      | Variant of
          { consts : Targetint_31_63.Set.t;
            non_consts :
              (Block_shape.t * kind_and_subkind list) Tag.Scannable.Map.t
          }
      | Float_block of { num_fields : int }
      | Float_array
      | Immediate_array
      | Value_array
      | Generic_array
      | Unboxed_float32_array
      | Unboxed_int32_array
      | Unboxed_int64_array
      | Unboxed_nativeint_array
      | Unboxed_vec128_array

    and kind_and_subkind =
      { kind : kind;
        subkind : t
      }

    let rec compatible (t : t) ~(when_used_at : t) =
      match t, when_used_at with
      (* Simple equality cases: *)
      | Anything, Anything
      | Boxed_float, Boxed_float
      | Boxed_float32, Boxed_float32
      | Boxed_int32, Boxed_int32
      | Boxed_int64, Boxed_int64
      | Boxed_nativeint, Boxed_nativeint
      | Boxed_vec128, Boxed_vec128
      | Tagged_immediate, Tagged_immediate
      | Float_array, Float_array
      | Immediate_array, Immediate_array
      | Value_array, Value_array
      | Generic_array, Generic_array
      | Unboxed_int32_array, Unboxed_int32_array
      | Unboxed_int64_array, Unboxed_int64_array
      | Unboxed_nativeint_array, Unboxed_nativeint_array
      | Unboxed_vec128_array, Unboxed_vec128_array ->
        true
      | ( Variant { consts = consts1; non_consts = non_consts1 },
          Variant { consts = consts2; non_consts = non_consts2 } ) ->
        if not (Targetint_31_63.Set.equal consts1 consts2)
        then false
        else
          let tags1 = Tag.Scannable.Map.keys non_consts1 in
          let tags2 = Tag.Scannable.Map.keys non_consts2 in
          if not (Tag.Scannable.Set.equal tags1 tags2)
          then false
          else
            let field_lists1 = Tag.Scannable.Map.data non_consts1 in
            let field_lists2 = Tag.Scannable.Map.data non_consts2 in
            assert (List.compare_lengths field_lists1 field_lists2 = 0);
            List.for_all2
              (fun (shape1, fields1) (shape2, fields2) ->
                if not (Block_shape.equal shape1 shape2)
                then false
                else if List.compare_lengths fields1 fields2 <> 0
                then false
                else
                  List.for_all2
                    (fun { kind = _; subkind = d }
                         { kind = _; subkind = when_used_at } ->
                      compatible d ~when_used_at)
                    fields1 fields2)
              field_lists1 field_lists2
      | ( Float_block { num_fields = num_fields1 },
          Float_block { num_fields = num_fields2 } ) ->
        num_fields1 = num_fields2
      (* Subkinds of [Value] may always be used at [Value] (but not the
         converse): *)
      | ( ( Variant _ | Float_block _ | Float_array | Immediate_array
          | Value_array | Generic_array | Boxed_float | Boxed_float32
          | Boxed_int32 | Boxed_int64 | Boxed_nativeint | Tagged_immediate ),
          Anything ) ->
        true
      (* All specialised (boxed) array kinds may be used at kind
         [Generic_array], and [Immediate_array] may be used at kind
         [Value_array]: *)
      | (Float_array | Immediate_array | Value_array), Generic_array
      | Immediate_array, Value_array ->
        true
      (* All other combinations are incompatible: *)
      | ( ( Anything | Boxed_float | Boxed_float32 | Boxed_int32 | Boxed_int64
          | Boxed_nativeint | Boxed_vec128 | Tagged_immediate | Variant _
          | Float_block _ | Float_array | Immediate_array | Value_array
          | Generic_array | Unboxed_float32_array | Unboxed_int32_array
          | Unboxed_int64_array | Unboxed_nativeint_array | Unboxed_vec128_array
            ),
          _ ) ->
        false

    include Container_types.Make (struct
      type nonrec t = t

      let rec print ppf t =
        let colour = Flambda_colours.subkind in
        match t with
        | Anything -> Format.fprintf ppf "*"
        | Tagged_immediate ->
          Format.fprintf ppf "%t=tagged_@<1>\u{2115}@<1>\u{1d55a}%t" colour
            Flambda_colours.pop
        | Boxed_float32 ->
          Format.fprintf ppf "%t=boxed_@<1>\u{2115}@<1>\u{1d557}32%t" colour
            Flambda_colours.pop
        | Boxed_float ->
          Format.fprintf ppf "%t=boxed_@<1>\u{2115}@<1>\u{1d557}%t" colour
            Flambda_colours.pop
        | Boxed_int32 ->
          Format.fprintf ppf "%t=boxed_@<1>\u{2115}@<1>\u{1d7db}@<1>\u{1d7da}%t"
            colour Flambda_colours.pop
        | Boxed_int64 ->
          Format.fprintf ppf "%t=boxed_@<1>\u{2115}@<1>\u{1d7de}@<1>\u{1d7dc}%t"
            colour Flambda_colours.pop
        | Boxed_nativeint ->
          Format.fprintf ppf "%t=boxed_@<1>\u{2115}@<1>\u{2115}%t" colour
            Flambda_colours.pop
        | Boxed_vec128 ->
          Format.fprintf ppf "%t=boxed_@<1>\u{2115}@<1>\u{1d54d}128%t" colour
            Flambda_colours.pop
        | Variant { consts; non_consts } ->
          let print_field ppf { kind = _; subkind } = print ppf subkind in
          Format.fprintf ppf "%t=Variant((consts (%a))@ (non_consts (%a)))%t"
            colour Targetint_31_63.Set.print consts
            (Tag.Scannable.Map.print (fun ppf (_shape, fields) ->
                 Format.fprintf ppf "[%a]"
                   (Format.pp_print_list ~pp_sep:Format.pp_print_space
                      print_field)
                   fields))
            non_consts Flambda_colours.pop
        | Float_block { num_fields } ->
          Format.fprintf ppf "%t=Float_block(%d)%t" colour num_fields
            Flambda_colours.pop
        | Float_array ->
          Format.fprintf ppf "%t=Float_array%t" colour Flambda_colours.pop
        | Immediate_array ->
          Format.fprintf ppf "%t=Immediate_array%t" colour Flambda_colours.pop
        | Value_array ->
          Format.fprintf ppf "%t=Value_array%t" colour Flambda_colours.pop
        | Generic_array ->
          Format.fprintf ppf "%t=Generic_array%t" colour Flambda_colours.pop
        | Unboxed_float32_array ->
          Format.fprintf ppf "%t=Unboxed_float32_array%t" colour
            Flambda_colours.pop
        | Unboxed_int32_array ->
          Format.fprintf ppf "%t=Unboxed_int32_array%t" colour
            Flambda_colours.pop
        | Unboxed_int64_array ->
          Format.fprintf ppf "%t=Unboxed_int64_array%t" colour
            Flambda_colours.pop
        | Unboxed_nativeint_array ->
          Format.fprintf ppf "%t=Unboxed_nativeint_array%t" colour
            Flambda_colours.pop
        | Unboxed_vec128_array ->
          Format.fprintf ppf "%t=Unboxed_vec128_array%t" colour
            Flambda_colours.pop

      let compare = Stdlib.compare

      let equal t1 t2 = compare t1 t2 = 0

      let hash = Hashtbl.hash
    end)
  end

  type with_subkind = Subkind.kind_and_subkind

  type t = with_subkind

  let create (kind : kind) (subkind : Subkind.t) : t =
    (match kind with
    | Value -> ()
    | Naked_number _ | Region | Rec_info -> (
      match subkind with
      | Anything -> ()
      | Boxed_float | Boxed_float32 | Boxed_int32 | Boxed_int64
      | Boxed_nativeint | Boxed_vec128 | Tagged_immediate | Variant _
      | Float_block _ | Float_array | Immediate_array | Value_array
      | Generic_array | Unboxed_float32_array | Unboxed_int32_array
      | Unboxed_int64_array | Unboxed_nativeint_array | Unboxed_vec128_array ->
        Misc.fatal_errorf "Subkind %a is not valid for kind %a" Subkind.print
          subkind print kind));
    { kind; subkind }

  let anything kind = create kind Anything

  let compatible (t : t) ~(when_used_at : t) =
    equal t.kind when_used_at.kind
    && Subkind.compatible t.subkind ~when_used_at:when_used_at.subkind

  let kind (t : t) = t.kind

  let subkind (t : t) = t.subkind

  let any_value = create value Anything

  let naked_immediate = create naked_immediate Anything

  let naked_float32 = create naked_float32 Anything

  let naked_float = create naked_float Anything

  let naked_int32 = create naked_int32 Anything

  let naked_int64 = create naked_int64 Anything

  let naked_nativeint = create naked_nativeint Anything

  let naked_vec128 = create naked_vec128 Anything

  let region = create region Anything

  let boxed_float32 = create value Boxed_float32

  let boxed_float = create value Boxed_float

  let boxed_int32 = create value Boxed_int32

  let boxed_int64 = create value Boxed_int64

  let boxed_nativeint = create value Boxed_nativeint

  let boxed_vec128 = create value Boxed_vec128

  let tagged_immediate = create value Tagged_immediate

  let rec_info = create rec_info Anything

  let float_array = create value Float_array

  let immediate_array = create value Immediate_array

  let value_array = create value Value_array

  let generic_array = create value Generic_array

  let unboxed_float32_array = create value Unboxed_float32_array

  let unboxed_int32_array = create value Unboxed_int32_array

  let unboxed_int64_array = create value Unboxed_int64_array

  let unboxed_nativeint_array = create value Unboxed_nativeint_array

  let unboxed_vec128_array = create value Unboxed_vec128_array

  let block tag fields =
    if List.exists (fun (t : t) -> not (equal t.kind Value)) fields
    then
      Misc.fatal_error
        "Block with fields of non-Value kind (use \
         [Flambda_kind.With_subkind.float_block] for float records)";
    match Tag.Scannable.of_tag tag with
    | Some tag ->
      create value
        (Variant
           { consts = Targetint_31_63.Set.empty;
             non_consts =
               Tag.Scannable.Map.singleton tag
                 (Block_shape.Scannable Value_only, fields)
           })
    | None -> Misc.fatal_errorf "Tag %a is not scannable" Tag.print tag

  let float_block ~num_fields = create value (Float_block { num_fields })

  let of_naked_number_kind (naked_number_kind : Naked_number_kind.t) =
    match naked_number_kind with
    | Naked_immediate -> naked_immediate
    | Naked_float -> naked_float
    | Naked_float32 -> naked_float32
    | Naked_int32 -> naked_int32
    | Naked_int64 -> naked_int64
    | Naked_nativeint -> naked_nativeint
    | Naked_vec128 -> naked_vec128

  let naked_of_boxable_number (boxable_number : Boxable_number.t) =
    match boxable_number with
    | Naked_float32 -> naked_float32
    | Naked_float -> naked_float
    | Naked_int32 -> naked_int32
    | Naked_int64 -> naked_int64
    | Naked_nativeint -> naked_nativeint
    | Naked_vec128 -> naked_vec128

  let boxed_of_boxable_number (boxable_number : Boxable_number.t) =
    match boxable_number with
    | Naked_float32 -> boxed_float32
    | Naked_float -> boxed_float
    | Naked_int32 -> boxed_int32
    | Naked_int64 -> boxed_int64
    | Naked_nativeint -> boxed_nativeint
    | Naked_vec128 -> boxed_vec128

  let of_flat_suffix_element elt =
    create (Flat_suffix_element0.kind elt) Anything

  let of_lambda_flat_element_kind elt =
    Flat_suffix_element0.from_lambda elt |> of_flat_suffix_element

  let rec from_lambda_value_kind (vk : Lambda.value_kind) =
    match vk with
    | Pgenval -> any_value
    | Pboxedfloatval Pfloat64 -> boxed_float
    | Pboxedfloatval Pfloat32 -> boxed_float32
    | Pboxedintval Pint32 -> boxed_int32
    | Pboxedintval Pint64 -> boxed_int64
    | Pboxedintval Pnativeint -> boxed_nativeint
    | Pboxedvectorval Pvec128 -> boxed_vec128
    | Pintval -> tagged_immediate
    | Pvariant { consts; non_consts } -> (
      match consts, non_consts with
      | [], [] -> Misc.fatal_error "[Pvariant] with no constructors at all"
      | [], [(tag, shape)] when tag = Obj.double_array_tag ->
        (* If we have [Obj.double_array_tag] here, this is always an all-float
           block, not an array. *)
        (* CR vlaviron: change the Lambda type *)
        let num_fields =
          match shape with
          | Constructor_uniform fields -> List.length fields
          | Constructor_mixed _ -> assert false
        in
        float_block ~num_fields
      | [], _ :: _ | _ :: _, [] | _ :: _, _ :: _ ->
        let consts =
          Targetint_31_63.Set.of_list
            (List.map (fun const -> Targetint_31_63.of_int const) consts)
        in
        let non_consts =
          List.fold_left
            (fun non_consts (tag, shape) ->
              match Tag.Scannable.create tag with
              | Some tag ->
                let shape_and_fields : Block_shape.t * _ =
                  (* CR mshinwell/vlaviron: In both of these cases it would be
                     nice to propagate immediacy information. *)
                  match (shape : Lambda.constructor_shape) with
                  | Constructor_uniform fields ->
                    Scannable Value_only, List.map from_lambda_value_kind fields
                  | Constructor_mixed { value_prefix; flat_suffix } ->
                    let mixed_block_shape =
                      Mixed_block_shape.from_lambda
                        { value_prefix_len = List.length value_prefix;
                          flat_suffix = Array.of_list flat_suffix
                        }
                    in
                    let fields =
                      List.map from_lambda_value_kind value_prefix
                      @ List.map of_lambda_flat_element_kind flat_suffix
                    in
                    Scannable (Mixed_record mixed_block_shape), fields
                in
                Tag.Scannable.Map.add tag shape_and_fields non_consts
              | None ->
                Misc.fatal_errorf "Non-scannable tag %d in [Pvariant]" tag)
            Tag.Scannable.Map.empty non_consts
        in
        create value (Variant { consts; non_consts }))
    | Parrayval Pfloatarray -> float_array
    | Parrayval Pintarray -> immediate_array
    | Parrayval Paddrarray -> value_array
    | Parrayval Pgenarray -> generic_array
    | Parrayval (Punboxedfloatarray Pfloat64) -> float_array
    | Parrayval (Punboxedfloatarray Pfloat32) -> unboxed_float32_array
    | Parrayval (Punboxedintarray Pint32) -> unboxed_int32_array
    | Parrayval (Punboxedintarray Pint64) -> unboxed_int64_array
    | Parrayval (Punboxedintarray Pnativeint) -> unboxed_nativeint_array
    | Parrayval (Punboxedvectorarray Pvec128) -> unboxed_vec128_array

  let from_lambda_values_and_unboxed_numbers_only (layout : Lambda.layout) =
    match layout with
    | Pvalue vk -> from_lambda_value_kind vk
    | Punboxed_float Pfloat64 -> naked_float
    | Punboxed_float Pfloat32 -> naked_float32
    | Punboxed_int Pint32 -> naked_int32
    | Punboxed_int Pint64 -> naked_int64
    | Punboxed_int Pnativeint -> naked_nativeint
    | Punboxed_vector Pvec128 -> naked_vec128
    | Punboxed_product _ | Ptop | Pbottom ->
      Misc.fatal_errorf
        "Flambda_kind.from_lambda_values_and_unboxed_numbers_only: cannot \
         convert %a"
        Printlambda.layout layout

  include Container_types.Make (struct
    type nonrec t = t

    let print ppf ({ kind; subkind } : t) =
      match kind, subkind with
      | _, Anything -> print ppf kind
      | Value, subkind ->
        Format.fprintf ppf "@[%a%a@]" print kind Subkind.print subkind
      | ( (Naked_number _ | Region | Rec_info),
          ( Boxed_float | Boxed_float32 | Boxed_int32 | Boxed_int64
          | Boxed_nativeint | Boxed_vec128 | Tagged_immediate | Variant _
          | Float_block _ | Float_array | Immediate_array | Value_array
          | Generic_array | Unboxed_float32_array | Unboxed_int32_array
          | Unboxed_int64_array | Unboxed_nativeint_array | Unboxed_vec128_array
            ) ) ->
        assert false
    (* see [create] *)

    let compare ({ kind = kind1; subkind = subkind1 } : t)
        ({ kind = kind2; subkind = subkind2 } : t) =
      let c = compare kind1 kind2 in
      if c <> 0 then c else Subkind.compare subkind1 subkind2

    let equal t1 t2 = compare t1 t2 = 0

    let hash ({ kind; subkind } : t) =
      Hashtbl.hash (hash kind, Subkind.hash subkind)
  end)

  let has_useful_subkind_info (t : t) =
    match t.subkind with
    | Anything -> false
    | Boxed_float | Boxed_float32 | Boxed_int32 | Boxed_int64 | Boxed_nativeint
    | Boxed_vec128 | Tagged_immediate | Variant _ | Float_block _ | Float_array
    | Immediate_array | Value_array | Generic_array | Unboxed_float32_array
    | Unboxed_int32_array | Unboxed_int64_array | Unboxed_nativeint_array
    | Unboxed_vec128_array ->
      true

  let erase_subkind (t : t) : t = { t with subkind = Anything }

  let equal_ignoring_subkind t1 t2 = equal (erase_subkind t1) (erase_subkind t2)
end

module Flat_suffix_element = struct
  include Flat_suffix_element0

  let to_kind_with_subkind t =
    match t with
    | Tagged_immediate -> With_subkind.tagged_immediate
    | Naked_float -> With_subkind.naked_float
    | Naked_float32 -> With_subkind.naked_float32
    | Naked_int32 -> With_subkind.naked_int32
    | Naked_int64 -> With_subkind.naked_int64
    | Naked_nativeint -> With_subkind.naked_nativeint
    | Naked_vec128 -> With_subkind.naked_vec128
end
