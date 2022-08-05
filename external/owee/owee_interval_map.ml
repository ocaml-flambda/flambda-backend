(* Representation of intervals.

   Even though the addresses are 64-bits, we assume that the higher-order bits
   are 0 (true on linux for user-space addresses).
   Therefore we can use tagged integers for better performance.
*)

type 'a interval = {
  lbound: int;
  rbound: int;
  value: 'a;
}

let interval l r value = {
  lbound = Int64.to_int l;
  rbound = Int64.to_int r;
  value
}

(* A specialized implementation of a balanced-binary tree
   that lazily prune out-of-bound intervals during rebalancing.

   Core balancing algorithm is taken from grenier.baltree
   https://github.com/let-def/grenier/blob/master/baltree/bt1.mli
*)
module Tree : sig
  type 'a t = private
    | Leaf
    | Node of int * 'a t * 'a interval * 'a t

  val leaf : 'a t
  val node : 'a t -> 'a interval -> 'a t -> 'a t

  (* [set_bound l] will cause [node] to prune intervals that end before [l]
     during rebalancing. *)
  val set_bound : int -> unit
end = struct
  type 'a t =
    | Leaf
    | Node of int * 'a t * 'a interval * 'a t

  let size = function
    | Node (s, _, _, _) -> s
    | Leaf -> 0

  let smaller_ell smin smax = (smin < smax) && ((smin land smax) lsl 1 < smax)
  let disbalanced smin smax = smaller_ell smin (smax lsr 1)
  let node_ l x r = Node (size l + 1 + size r, l, x, r)

  let rot_left l x r k = match r with
    | Node (_, rl, y, rr) ->
      k (k l x rl) y rr
    | _ -> assert false

  let rot_right l y r k = match l with
    | Node (_, ll, x, lr) ->
      k ll x (k lr y r)
    | _ -> assert false

  let inc_left l x r k =
    let r = match r with
      | Node (_, rl, y, rr) when smaller_ell (size rr) (size rl) ->
        rot_right rl y rr k
      | _ -> r
    in
    rot_left l x r k

  let inc_right l y r k =
    let l = match l with
      | Node (_, ll, x, lr) when smaller_ell (size ll) (size lr) ->
        rot_left ll x lr k
      | _ -> l
    in
    rot_right l y r k

  let rec node_left l x r =
    if disbalanced (size l) (size r) then
      inc_left l x r node_left
    else
      node_ l x r

  let rec node_right l y r =
    if disbalanced (size r) (size l) then
      inc_right l y r node_right
    else
      node_ l y r

  let leaf = Leaf

  let bound = ref min_int

  let set_bound x = bound := x

  let rec node l x r =
    if x.rbound < !bound then
      join l r
    else match l, r with
    | Leaf, Leaf -> node_ leaf x leaf
    | l, r when size l < size r ->
      node_left l x r
    | l, r ->
      node_right l x r

  and join l r = match l, r with
    | Leaf, t | t, Leaf -> t
    | Node (sl, ll, x, lr), Node (sr, rl, y, rr) ->
      if sl <= sr then
        node (join l rl) y rr
      else
        node ll x (join lr r)
end

let int_compare : int -> int -> int = compare

(* Implement a right-bound ordered interval map on top of Tree *)

module RMap = struct
  type 'a t = 'a Tree.t
  let empty = Tree.leaf

  let rec add i = function
    | Tree.Leaf -> Tree.node empty i empty
    | Tree.Node (_, l, j, r) ->
      let c = int_compare i.rbound j.rbound in
      if c < 0
      then Tree.node (add i l) j r
      else Tree.node l j (add i r)

  let add i t =
    Tree.set_bound i.lbound;
    add i t

  let rec build_spine bound acc = function
    | Tree.Leaf -> acc
    | Tree.Node (_, l, i, r) ->
      let c = int_compare i.rbound bound in
      if c >= 0 then
        build_spine bound ((i, r) :: acc) l
      else
        build_spine bound acc r

  let rec expand_spine acc = function
    | Tree.Leaf -> acc
    | Tree.Node (_, l, i, r) ->
      expand_spine ((i, r) :: acc) l

  let list_from rmap bound =
    let rec loop acc = function
      | [] -> acc
      | (i, r) :: spine' -> loop (i :: acc) (expand_spine spine' r)
    in
    loop [] (build_spine bound [] rmap)
end

(*
  Algorithm suggested by Tudor Brindus (@Xyene) and Timothy Li (@FatalEagle)
  Implementation by Frédéric Bour (@let-def).

  See https://github.com/let-def/owee/issues/23,
  and https://github.com/let-def/owee/pull/24.
*)

(*
  The [intervals] array contain each interval definition.
  The [maps.(i)] contains a tree of all intervals that overlap [intervals.(i)].
  Both are ordered by left-bound of intervals.

  [maps] is computed by a left-scan that is done lazily. [last] contains the
  last index of [maps] that was computed.
  Therefore all cells [maps.(i)] for [0 <= i <= last] are valid, while
  [last < i < Array.length.(maps)] still have to be computed.
  This is done by [initialize_until].
*)

type 'a t = {
  intervals: 'a interval array;
  maps: 'a RMap.t array;
  mutable last: int;
}

let create count ~f =
  let intervals = Array.init count f in
  Array.fast_sort (fun i1 i2 -> int_compare i1.lbound i2.lbound) intervals;
  { intervals; maps = Array.make count RMap.empty; last = -1 }

let iter (t : _ t) ~f =
  Array.iter f t.intervals

(* Lazy initialization of [t.maps] *)

let initialize_until t j =
  let last = t.last in
  if j > last then (
    let cumulative = ref (if last < 0 then RMap.empty else t.maps.(last)) in
    for i = last + 1 to j do
      let interval = t.intervals.(i) in
      cumulative := RMap.add interval !cumulative;
      (*Printf.eprintf "size: %d\n" (Tree.size !cumulative);*)
      t.maps.(i) <- !cumulative;
    done;
    t.last <- j;
  )

(* Left-leaning binary search on array of intervals *)
let closest_key intervals (addr : int) =
  let l = ref 0 in
  let r = ref (Array.length intervals - 1) in
  while !l <= !r do
    let m = !l + (!r - !l) / 2 in
    let lb = intervals.(m).lbound in
    if lb < addr then
      l := m + 1
    else
      r := m - 1
  done;
  if (!l = Array.length intervals) || (intervals.(!l).lbound > addr) then
    decr l;
  assert (!l = -1 || intervals.(!l).lbound <= addr);
  !l

(* Query algorithm:
   - find the closest interval that starts before [addr]
   - return the list of all overlapping intervals that end after [addr] *)
let query t (addr : int64) =
  let addr = Int64.to_int addr in
  let l = closest_key t.intervals addr in
  if l = -1 then [] else (
    initialize_until t l;
    RMap.list_from t.maps.(l) addr
  )

(*
  Uncomment: switch to eager creation and print counters on interval
  computation

  let create count ~f =
    let result = create count ~f in
    let t0 = Sys.time () in
    let min0, prom0, maj0 = Gc.counters () in
    initialize_until result (count - 1);
    let min1, prom1, maj1 = Gc.counters () in
    let t1 = Sys.time () in
    Printf.eprintf "owee: minor:%.0f prom:%.0f maj:%.0f t:%.02f\n%!"
      (min1 -. min0) (prom1 -. prom0) (maj1 -. maj0) (t1 -. t0);
    result
*)
