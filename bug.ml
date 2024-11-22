let __dummy1__ _ = assert false[@@inline never ]
external __dummy2__ : unit -> 'a = "%opaque"
external __ignore__ : 'a -> unit = "%ignore"

module Flambda_kind = struct
  module Standard_int = struct
    type t =
      | Tagged_immediate
      | Naked_immediate
      | Naked_int32
      | Naked_int64
      | Naked_nativeint

    let compare = Stdlib.compare

    let equal t1 t2 = compare t1 t2 = 0
  end
end

module K = Flambda_kind
module I = K.Standard_int

type array_index_kind =
  | Ptagged_int_index
  | Punboxed_int_index of unboxed_integer

and unboxed_integer =
  | Unboxed_int64
  | Unboxed_nativeint
  | Unboxed_int32

let[@inline never] standard_int_of_unboxed_integer (_ : unboxed_integer) : K.Standard_int.t
  = __dummy2__ ()

let check_bound
    ~index_kind:(index_kind : array_index_kind)
    ~bound_kind:(bound_kind : I.t) =
  let index_kind =
    match index_kind with
    | Ptagged_int_index -> __dummy2__ ()
    | Punboxed_int_index width ->
        standard_int_of_unboxed_integer (__dummy2__ ())
  in
  let (comp_kind : I.t) =
    ( (match (index_kind, bound_kind) with
       | (Tagged_immediate, Tagged_immediate) -> __dummy2__ ()
       | (Naked_int64, _) | (_, Naked_int64) -> __dummy2__ ()
       | ((Naked_nativeint | Tagged_immediate | Naked_immediate | Naked_int32),
          (Naked_nativeint | Tagged_immediate | Naked_immediate | Naked_int32))
         -> Naked_nativeint : I.t))
  in
  let conv x ~src =
    if I.equal src comp_kind then __dummy2__ () else __dummy2__ ()
  [@@inline always][@@local never ]
  in
  let bound = conv (__dummy2__ ()) ~src:(__dummy2__ ()) in
  __dummy2__ ()
