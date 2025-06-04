[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare

module type S = sig
  type e

  type t

  val make : original_capacity:int -> t

  val clear : t -> unit

  val is_empty : t -> bool

  val choose_and_remove : t -> e option

  val add : t -> e -> unit

  val remove : t -> e -> unit

  val iter : t -> f:(e -> unit) -> unit

  val fold : t -> f:('a -> e -> 'a) -> init:'a -> 'a

  val to_list : t -> e list
end

module type OrderedTypeWithDummy = sig
  include Set.OrderedType

  val dummy : t
end

external unsafe_blit :
  src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit
  = "caml_array_blit"

external unsafe_fill : 'a array -> pos:int -> len:int -> 'a -> unit
  = "caml_array_fill"

module Make (T : OrderedTypeWithDummy) : S with type e = T.t = struct
  type e = T.t

  type t =
    { mutable array : e array;
      mutable length : int
    }

  let make ~original_capacity =
    let array = Array.make (max 1 original_capacity) T.dummy in
    let length = 0 in
    { array; length }

  let clear t =
    unsafe_fill t.array ~pos:0 ~len:t.length T.dummy;
    t.length <- 0

  let is_empty t = Int.equal t.length 0

  let index array length e =
    let low = ref 0 in
    let high = ref length in
    while !low < !high do
      let mid = (!low + !high) / 2 in
      if T.compare e (Array.unsafe_get array mid) > 0
      then low := succ mid
      else high := mid
    done;
    !low

  let new_length curr = if curr < 512 then 2 * curr else curr + 128

  let add t e =
    let idx = index t.array t.length e in
    if idx >= Array.length t.array
       || T.compare e (Array.unsafe_get t.array idx) <> 0
    then (
      if t.length = Array.length t.array
      then (
        (* reallocation *)
        let new_array =
          Array.make (new_length (Array.length t.array)) T.dummy
        in
        let len_before = idx in
        if len_before > 0
        then
          unsafe_blit ~src:t.array ~src_pos:0 ~dst:new_array ~dst_pos:0
            ~len:len_before;
        let len_after = t.length - idx in
        if len_after > 0
        then
          unsafe_blit ~src:t.array ~src_pos:idx ~dst:new_array
            ~dst_pos:(succ idx) ~len:len_after;
        Array.unsafe_set new_array idx e;
        t.array <- new_array;
        t.length <- succ t.length)
      else
        (* insertion *)
        let len = t.length - idx in
        if len > 0
        then
          unsafe_blit ~src:t.array ~src_pos:idx ~dst:t.array ~dst_pos:(succ idx)
            ~len;
        Array.unsafe_set t.array idx e;
        t.length <- succ t.length)

  let remove t e =
    let idx = index t.array t.length e in
    if idx < Array.length t.array
       && T.compare e (Array.unsafe_get t.array idx) = 0
    then (
      let len = t.length - idx - 1 in
      if len > 0
      then
        unsafe_blit ~src:t.array ~src_pos:(succ idx) ~dst:t.array ~dst_pos:idx
          ~len;
      t.length <- pred t.length;
      Array.unsafe_set t.array t.length T.dummy)

  let choose_and_remove t =
    if Int.equal t.length 0
    then None
    else
      let idx = pred t.length in
      t.length <- idx;
      let res = Some (Array.unsafe_get t.array idx) in
      Array.unsafe_set t.array idx T.dummy;
      res

  let iter t ~f =
    for i = 0 to pred t.length do
      f (Array.unsafe_get t.array i)
    done

  let fold t ~f ~init =
    let res = ref init in
    for i = 0 to pred t.length do
      res := f !res (Array.unsafe_get t.array i)
    done;
    !res

  let to_list t =
    let rec loop arr idx acc =
      if idx < 0
      then acc
      else loop arr (pred idx) (Array.unsafe_get arr idx :: acc)
    in
    loop t.array (pred t.length) []
end
