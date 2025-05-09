type taggable_integer =
  | Int8
  | Int16
  | Int

type boxable_integer =
  | Int32
  | Int64
  | Nativeint

type floating =
  | Float
  | Float32

type boxable =
  | Integer of boxable_integer
  | Float of floating
  | Vec128

(** All constructors of this type other than [Int] are technically optional and just give
    more context. *)
type tagged_integer =
  | Constants of int list
  | Known_width of taggable_integer

type gc_ignorable =
  | Tagged of tagged_integer
  | Untagged of tagged_integer
  | Unboxed of boxable_integer

(** an unboxed product *)
type 'a unboxed_product =
  | One of 'a
  | Product of 'a unboxed_product array  (** Invariant: non-empty array *)

type 'addr array_contents =
  | Genarray
  | Floatarray
  | Gc_scanned of 'addr unboxed_product
  | Gc_ignored of gc_ignorable unboxed_product

type 'addr constructor_field =
  | Addr of 'addr
  | Naked of gc_ignorable

type 'addr non_constant_constructor =
  { tag : int;
    fields : 'addr constructor_field unboxed_product
  }

type 'addr block =
  | Top
  | Non_constant_constructors of 'addr non_constant_constructor list
  | Bytes
  | Bigarray
  | Array of 'addr array_contents
  | Boxed of boxable
  | Lazy of 'addr
  | Forward of 'addr
  | Bottom

type 'addr value =
  { consts : tagged_integer;
    blocks : 'addr block
  }

type 'addr gc_scannable =
  | Nullable of 'addr value
  | Non_nullable of 'addr value

let tag = function
  | Non_constant_constructor { tag; _ } ->
    assert (
      Obj.first_non_constant_constructor_tag <= tag
      && tag <= Obj.last_non_constant_constructor_tag);
    Some tag
  | Bytes -> Some Obj.string_tag
  | Array Floatarray -> Some Obj.double_array_tag
  | Array (Gc_scanned _) -> Some Obj.first_non_constant_constructor_tag
  | Boxed Float64 -> Some Obj.double_tag
  | Array (Gc_ignored _) | Boxed _ -> Some Obj.custom_tag
  | Other -> None

type primitive =
  | Array_ref of
      { array : unit array_contents;
        index : integral
      }
