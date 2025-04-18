[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

module T = struct
  type t =
    { src : Label.t;
      dst : Label.t
    }

  let compare { src = left_src; dst = left_dst }
      { src = right_src; dst = right_dst } =
    match Label.compare left_src right_src with
    | 0 -> Label.compare left_dst right_dst
    | c -> c
end

include T

module Map : Map.S with type key = t = Map.Make (T)

module Set : Set.S with type elt = t = Set.Make (T)
