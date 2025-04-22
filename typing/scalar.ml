type any_locality_mode = Any_locality_mode

module Integer_comparison = struct
  type t =
    | Ceq
    | Cne
    | Clt
    | Cgt
    | Cle
    | Cge

  let all = [Ceq; Cne; Clt; Cgt; Cle; Cge]

  let to_string = function
    | Ceq -> "equal"
    | Cne -> "notequal"
    | Cgt -> "greaterthan"
    | Cge -> "greaterequal"
    | Clt -> "lessthan"
    | Cle -> "lessequal"

  let negate = function
    | Ceq -> Cne
    | Cne -> Ceq
    | Clt -> Cge
    | Cle -> Cgt
    | Cgt -> Cle
    | Cge -> Clt

  let swap = function
    | Ceq -> Ceq
    | Cne -> Cne
    | Clt -> Cgt
    | Cle -> Cge
    | Cgt -> Clt
    | Cge -> Cle
end

module Float_comparison = struct
  type t =
    | CFeq
    | CFneq
    | CFlt
    | CFnlt
    | CFgt
    | CFngt
    | CFle
    | CFnle
    | CFge
    | CFnge

  let all = [CFeq; CFneq; CFlt; CFnlt; CFgt; CFngt; CFle; CFnle; CFge; CFnge]

  let to_string = function
    | CFeq -> "ordered_and_equal"
    | CFgt -> "ordered_and_greaterthan"
    | CFge -> "ordered_and_greaterequal"
    | CFlt -> "ordered_and_lessthan"
    | CFle -> "ordered_and_lessequal"
    | CFneq -> "unordered_or_notequal"
    | CFngt -> "unordered_or_lessequal"
    | CFnge -> "unordered_or_lessthan"
    | CFnlt -> "unordered_or_greaterequal"
    | CFnle -> "unordered_or_greaterthan"

  let negate = function
    | CFeq -> CFneq
    | CFneq -> CFeq
    | CFlt -> CFnlt
    | CFnlt -> CFlt
    | CFgt -> CFngt
    | CFngt -> CFgt
    | CFle -> CFnle
    | CFnle -> CFle
    | CFge -> CFnge
    | CFnge -> CFge

  let swap = function
    | CFeq -> CFeq
    | CFneq -> CFneq
    | CFlt -> CFgt
    | CFnlt -> CFngt
    | CFle -> CFge
    | CFnle -> CFnge
    | CFgt -> CFlt
    | CFngt -> CFnlt
    | CFge -> CFle
    | CFnge -> CFnle
end

let ignore_locality _ = Any_locality_mode

module Maybe_naked = struct
  type ('a, 'b) t =
    | Value of 'a
    | Naked of 'b

  module Make1 (M : sig
    type 'a t

    val all : any_locality_mode t list

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val to_string : any_locality_mode t -> string

    val naked_sort : any_locality_mode t -> Jkind_types.Sort.Const.t
  end) =
  struct
    type nonrec 'a t = ('a M.t, any_locality_mode M.t) t

    let map (t : _ t) ~f =
      match t with
      | Naked (_ : any_locality_mode M.t) as t -> t
      | Value t -> Value (M.map t ~f)

    let width (Naked t | Value t : _ t) = t

    let to_string = function
      | Value m -> M.to_string m
      | Naked m -> M.to_string m ^ "#"

    let ignore_locality = map ~f:ignore_locality

    let all = List.concat_map (fun m -> [Value m; Naked m]) M.all

    let sort = function
      | Value (_ : any_locality_mode M.t) -> Jkind_types.Sort.Const.value
      | Naked t -> M.naked_sort t
  end
end

module Integral = struct
  module Taggable = struct
    module Width = struct
      type t =
        | Int8
        | Int16
        | Int

      let all = [Int8; Int16; Int]

      let to_string = function
        | Int8 -> "int8"
        | Int16 -> "int16"
        | Int -> "int"

      let naked_sort = function
        | Int8 -> Jkind_types.Sort.Const.bits8
        | Int16 -> Jkind_types.Sort.Const.bits16
        | Int -> Jkind_types.Sort.Const.word
    end

    include Maybe_naked.Make1 (struct
      include Width

      type nonrec 'a t = t

      let map t ~f:_ = t
    end)
  end

  module Boxable = struct
    module Width = struct
      type 'mode t =
        | Int32 of 'mode
        | Nativeint of 'mode
        | Int64 of 'mode

      let all =
        [ Int32 Any_locality_mode;
          Nativeint Any_locality_mode;
          Int64 Any_locality_mode ]

      let map t ~f =
        match t with
        | Int32 mode -> Int32 (f mode)
        | Nativeint mode -> Nativeint (f mode)
        | Int64 mode -> Int64 (f mode)

      let to_string = function
        | Int32 Any_locality_mode -> "int32"
        | Nativeint Any_locality_mode -> "nativeint"
        | Int64 Any_locality_mode -> "int64"

      let naked_sort = function
        | Int32 Any_locality_mode -> Jkind_types.Sort.Const.bits32
        | Int64 Any_locality_mode -> Jkind_types.Sort.Const.bits64
        | Nativeint Any_locality_mode -> Jkind_types.Sort.Const.word
    end

    include Maybe_naked.Make1 (Width)
  end

  module Width = struct
    type 'mode t =
      | Taggable of Taggable.Width.t
      | Boxable of 'mode Boxable.Width.t

    let all =
      List.concat
        [ List.map (fun t -> Taggable t) Taggable.Width.all;
          List.map (fun t -> Boxable t) Boxable.Width.all ]

    let map t ~f =
      match t with
      | Taggable (Int8 | Int16 | Int) as t -> t
      | Boxable b -> Boxable (Boxable.Width.map b ~f)

    let to_string = function
      | Taggable t -> Taggable.Width.to_string t
      | Boxable b -> Boxable.Width.to_string b

    let naked_sort = function
      | Taggable t -> Taggable.Width.naked_sort t
      | Boxable b -> Boxable.Width.naked_sort b

    let int8 = Taggable Int8

    let int16 = Taggable Int16

    let int32 = Boxable (Int32 Any_locality_mode)

    let int64 = Boxable (Int64 Any_locality_mode)

    let int = Taggable Int

    let nativeint = Boxable (Nativeint Any_locality_mode)
  end

  include Maybe_naked.Make1 (Width)

  let int8 : _ t = Value Width.int8

  let int16 : _ t = Value Width.int16

  let int32 : _ t = Value Width.int32

  let int64 : _ t = Value Width.int64

  let int : _ t = Value Width.int

  let nativeint : _ t = Value Width.nativeint

  let naked_int8 : _ t = Naked Width.int8

  let naked_int16 : _ t = Naked Width.int16

  let naked_int32 : _ t = Naked Width.int32

  let naked_int64 : _ t = Naked Width.int32

  let naked_int : _ t = Naked Width.int

  let naked_nativeint : _ t = Naked Width.nativeint
end

module Floating = struct
  module Width = struct
    type 'mode t =
      | Float32 of 'mode
      | Float64 of 'mode

    let all = [Float32 Any_locality_mode; Float64 Any_locality_mode]

    let map t ~f =
      match t with
      | Float32 mode -> Float32 (f mode)
      | Float64 mode -> Float64 (f mode)

    let to_string = function
      | Float32 Any_locality_mode -> "float32"
      | Float64 Any_locality_mode -> "float"

    let naked_sort = function
      | Float32 Any_locality_mode -> Jkind_types.Sort.Const.float32
      | Float64 Any_locality_mode -> Jkind_types.Sort.Const.float64

    let float32 = Float32 Any_locality_mode

    let float = Float64 Any_locality_mode
  end

  include Maybe_naked.Make1 (Width)

  let float32 : _ t = Value Width.float32

  let float : _ t = Value Width.float

  let naked_float32 : _ t = Naked Width.float32

  let naked_float : _ t = Naked Width.float
end

module Width = struct
  type 'mode t =
    | Floating of 'mode Floating.Width.t
    | Integral of 'mode Integral.Width.t

  let all =
    List.concat
      [ List.map (fun t -> Floating t) Floating.Width.all;
        List.map (fun t -> Integral t) Integral.Width.all ]

  let map t ~f =
    match t with
    | Floating g -> Floating (Floating.Width.map g ~f)
    | Integral i -> Integral (Integral.Width.map i ~f)

  let naked_sort = function
    | Floating f -> Floating.Width.naked_sort f
    | Integral i -> Integral.Width.naked_sort i

  let ignore_locality = map ~f:ignore_locality

  let to_string = function
    | Floating f -> Floating.Width.to_string f
    | Integral i -> Integral.Width.to_string i

  let float32 = Floating Floating.Width.float32

  let float = Floating Floating.Width.float

  let int8 = Integral Integral.Width.int8

  let int16 = Integral Integral.Width.int16

  let int = Integral Integral.Width.int

  let int32 = Integral Integral.Width.int32

  let int64 = Integral Integral.Width.int64

  let nativeint = Integral Integral.Width.nativeint
end

include Maybe_naked.Make1 (Width)

let integral : 'a Integral.t -> 'a t = function
  | Value i -> Value (Integral i)
  | Naked i -> Naked (Integral i)

let floating : 'a Floating.t -> 'a t = function
  | Value f -> Value (Floating f)
  | Naked f -> Naked (Floating f)

let int8 : _ t = Value Width.int8

let int16 : _ t = Value Width.int16

let int : _ t = Value Width.int

let int32 : _ t = Value Width.int32

let nativeint : _ t = Value Width.nativeint

let int64 : _ t = Value Width.int64

let float32 : _ t = Value Width.float32

let float : _ t = Value Width.float

let naked_int8 : _ t = Naked Width.int8

let naked_int16 : _ t = Naked Width.int16

let naked_int : _ t = Naked Width.int

let naked_int32 : _ t = Naked Width.int32

let naked_nativeint : _ t = Naked Width.nativeint

let naked_int64 : _ t = Naked Width.int64

let naked_float32 : _ t = Naked Width.float32

let naked_float : _ t = Naked Width.float

type 'a scalar = 'a t

module Intrinsic = struct
  type 'mode info =
    { can_raise : bool;
      result : 'mode t
    }

  (* CR jvanburen: nullary primitives for 0/1/-1/min/max once we can put
     unboxed values into structures?
  *)

  module Unary = struct
    (* Remember to update [all] right below this if you add a constructor *)
    module Int_op = struct
      type t =
        | Neg
        | Succ  (** add 1 *)
        | Pred  (** subtract 1 *)
        | Bswap

      let all = [Neg; Succ; Pred; Bswap]

      let to_string = function
        | Neg -> "neg"
        | Succ -> "succ"
        | Pred -> "pred"
        | Bswap -> "bswap"
    end

    module Float_op = struct
      type t =
        | Neg
        | Abs

      let all = [Neg; Abs]

      let to_string = function Neg -> "neg" | Abs -> "abs"
    end

    type nonrec 'mode t =
      (* CR jvanburen: logical Not, int Abs, float bitcast *)
      | Integral of 'mode Integral.t * Int_op.t
      | Floating of 'mode Floating.t * Float_op.t
      | Static_cast of
          { src : any_locality_mode t;
            dst : 'mode t
          }

    let all =
      List.concat
        [ ListLabels.concat_map Integral.all ~f:(fun size ->
              ListLabels.map Int_op.all ~f:(fun op -> Integral (size, op)));
          ListLabels.concat_map Floating.all ~f:(fun size ->
              ListLabels.map Float_op.all ~f:(fun op -> Floating (size, op)));
          ListLabels.concat_map all ~f:(fun src ->
              ListLabels.concat_map all ~f:(fun dst ->
                  if src = dst then [] else [Static_cast { src; dst }])) ]

    let map (type a b) (t : a t) ~(f : a -> b) : b t =
      match t with
      | Integral (size, op) -> Integral (Integral.map size ~f, op)
      | Floating (size, op) -> Floating (Floating.map size ~f, op)
      | Static_cast { src; dst } -> Static_cast { src; dst = map dst ~f }

    let info = function
      | Integral (size, (Neg | Bswap | Succ | Pred)) ->
        { result = integral size; can_raise = false }
      | Floating (size, (Neg | Abs)) ->
        { result = floating size; can_raise = false }
      | Static_cast { src = _; dst } -> { result = dst; can_raise = false }

    let to_string t =
      let i = Integral.to_string in
      let f = Floating.to_string in
      match t with
      | Integral (size, op) ->
        Printf.sprintf "%s_%s" (i size) (Int_op.to_string op)
      | Floating (size, op) ->
        Printf.sprintf "%s_%s" (f size) (Float_op.to_string op)
      | Static_cast { src; dst } ->
        Printf.sprintf "%s_of_%s" (to_string dst) (to_string src)

    let sort = function
      | Integral (width, _) ->
        let sort = Integral.sort width in
        sort, sort
      | Floating (width, _) ->
        let sort = Floating.sort width in
        sort, sort
      | Static_cast { src; dst } -> sort src, sort dst
  end

  module Binary = struct
    module Int_op = struct
      type division_is_safe =
        | Safe
        | Unsafe

      type t =
        | Add
        | Sub
        | Mul
        | Div of division_is_safe
        | Mod of division_is_safe
        | And
        | Or
        | Xor

      let all =
        [Add; Sub; Mul; Div Safe; Div Unsafe; Mod Safe; Mod Unsafe; And; Or; Xor]

      let to_string = function
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div Safe -> "div"
        | Div Unsafe -> "unsafe_div"
        | Mod Safe -> "mod"
        | Mod Unsafe -> "unsafe_mod"
        | And -> "and"
        | Or -> "or"
        | Xor -> "xor"
    end

    module Shift_op = struct
      module Rhs = struct
        (* CR jvanburen: expand this to all of [Integral.t] *)
        type t = Int

        let all = [Int]
      end

      type t =
        | Lsl
        | Asr
        | Lsr

      let all = [Lsl; Asr; Lsr]

      let to_string = function Lsl -> "lsl" | Asr -> "asr" | Lsr -> "lsr"
    end

    module Float_op = struct
      type t =
        | Add
        | Sub
        | Mul
        | Div

      let all = [Add; Sub; Mul; Div]

      let to_string = function
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div -> "div"
    end

    (* CR jvanburen: comparisons that return naked values *)

    (** comparisons return a tagged immediate *)
    type nonrec 'mode t =
      (* CR jvanburen: Fmod, Min, Max? *)
      | Integral of 'mode Integral.t * Int_op.t
      | Shift of 'mode Integral.t * Shift_op.t * Shift_op.Rhs.t
      | Floating of 'mode Floating.t * Float_op.t
      | Icmp of any_locality_mode Integral.t * Integer_comparison.t
      | Fcmp of any_locality_mode Floating.t * Float_comparison.t
      | Three_way_compare of any_locality_mode t

    let all =
      List.concat
        [ ListLabels.concat_map Integral.all ~f:(fun size ->
              ListLabels.map Int_op.all ~f:(fun op -> Integral (size, op)));
          ListLabels.concat_map Floating.all ~f:(fun size ->
              ListLabels.map Float_op.all ~f:(fun op -> Floating (size, op)));
          ListLabels.concat_map Integral.all ~f:(fun size ->
              ListLabels.concat_map Shift_op.all ~f:(fun op ->
                  ListLabels.map Shift_op.Rhs.all ~f:(fun rhs ->
                      Shift (size, op, rhs))));
          ListLabels.concat_map Integer_comparison.all ~f:(fun cmp ->
              List.map (fun size -> Icmp (size, cmp)) Integral.all);
          ListLabels.concat_map Float_comparison.all ~f:(fun cmp ->
              List.map (fun size -> Fcmp (size, cmp)) Floating.all);
          List.map (fun size -> Three_way_compare size) all ]

    let sort = function
      | Integral
          ( width,
            ( Add | Sub | Mul
            | Div (Safe | Unsafe)
            | Mod (Safe | Unsafe)
            | And | Or | Xor ) ) ->
        let sort = Integral.sort width in
        sort, sort, sort
      | Shift (width, (Lsl | Lsr | Asr), Int) ->
        let sort = Integral.sort width in
        sort, Jkind_types.Sort.Const.value, sort
      | Floating (width, (Add | Sub | Mul | Div)) ->
        let sort = Floating.sort width in
        sort, sort, sort
      | Icmp (width, (_ : Integer_comparison.t)) ->
        let sort = Integral.sort width in
        sort, sort, Jkind_types.Sort.Const.value
      | Fcmp (width, (_ : Float_comparison.t)) ->
        let sort = Floating.sort width in
        sort, sort, Jkind_types.Sort.Const.value
      | Three_way_compare width ->
        let sort = sort width in
        sort, sort, Jkind_types.Sort.Const.value

    let to_string t =
      let make size name = String.concat "_" [to_string size; name] in
      match t with
      | Integral (size, op) -> make (integral size) (Int_op.to_string op)
      | Floating (size, op) -> make (floating size) (Float_op.to_string op)
      | Shift (size, op, Int) -> make (integral size) (Shift_op.to_string op)
      | Icmp (size, cmp) ->
        make (integral size) (Integer_comparison.to_string cmp)
      | Fcmp (size, cmp) ->
        make (floating size) (Float_comparison.to_string cmp)
      | Three_way_compare size -> make size "compare"

    let map t ~f =
      match t with
      | Integral (size, op) -> Integral (Integral.map size ~f, op)
      | Floating (size, op) -> Floating (Floating.map size ~f, op)
      | Shift (size, op, rhs) -> Shift (Integral.map size ~f, op, rhs)
      | Icmp (size, cmp) -> Icmp (size, cmp)
      | Fcmp (size, cmp) -> Fcmp (size, cmp)
      | Three_way_compare size -> Three_way_compare size

    let info = function
      | Integral
          (size, (Add | Sub | Mul | Div Unsafe | Mod Unsafe | And | Or | Xor))
      | Shift (size, (Lsl | Lsr | Asr), Int) ->
        { result = integral size; can_raise = false }
      | Integral (size, (Div Safe | Mod Safe)) ->
        { result = integral size; can_raise = true }
      | Floating (size, (Add | Sub | Mul | Div)) ->
        { result = floating size; can_raise = false }
      | Icmp ((_ : any_locality_mode Integral.t), (_ : Integer_comparison.t))
      | Fcmp ((_ : any_locality_mode Floating.t), (_ : Float_comparison.t))
      | Three_way_compare (_ : any_locality_mode scalar) ->
        { result = int; can_raise = false }
  end

  type 'mode t =
    | Unary of 'mode Unary.t
    | Binary of 'mode Binary.t

  let all =
    List.concat
      [ List.map (fun u -> Unary u) Unary.all;
        List.map (fun b -> Binary b) Binary.all ]

  let sort = function
    | Unary u ->
      let x, y = Unary.sort u in
      [x; y]
    | Binary b ->
      let x, y, z = Binary.sort b in
      [x; y; z]

  let map t ~f =
    match t with
    | Unary u -> Unary (Unary.map u ~f)
    | Binary b -> Binary (Binary.map b ~f)

  let info = function Unary u -> Unary.info u | Binary b -> Binary.info b

  let to_string = function
    | Unary u -> Unary.to_string u
    | Binary b -> Binary.to_string b

  let arity = function Unary _ -> 1 | Binary _ -> 2

  let of_string =
    let module Tbl = Misc.Stdlib.String.Tbl in
    let tbl =
      lazy
        (let tbl = Tbl.create (2 * List.length all) in
         ListLabels.iter all ~f:(fun t -> Tbl.add tbl (to_string t) t);
         tbl)
    in
    fun s -> Tbl.find (Lazy.force tbl) s

  module With_percent_prefix = struct
    type nonrec t = any_locality_mode t

    let to_string t = "%" ^ to_string t

    let of_string s =
      let len = String.length s in
      if len = 0 || s.[0] <> '%'
      then raise Not_found
      else of_string (StringLabels.sub s ~pos:1 ~len:(len - 1))
  end
end
