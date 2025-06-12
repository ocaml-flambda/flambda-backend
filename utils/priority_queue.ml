[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module Array = ArrayLabels

let fatal = Misc.fatal_error

(* CR xclerc for xclerc: reuse `{Map,Set}.OrderedType`? *)
module type Order = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type Priority_queue = sig
  type priority

  type 'a t

  type 'a element =
    { priority : priority;
      data : 'a
    }

  val make : initial_capacity:int -> 'a t

  val is_empty : 'a t -> bool

  val size : 'a t -> int

  val add : 'a t -> priority:priority -> data:'a -> unit

  val get : 'a t -> 'a element

  val remove : 'a t -> unit

  val get_and_remove : 'a t -> 'a element

  val iter : 'a t -> f:('a element -> unit) -> unit
end

(* CR-soon gyorsh: check whether the dynamic array module from the stdlib can be
   used. *)
(* CR xclerc for xclerc: some issues we might want to address with the
   implementation below: - it uses `Obj.magic`; - `elements` can only grow. *)
module Make (Priority : Order) :
  Priority_queue with type priority = Priority.t = struct
  type priority = Priority.t

  type 'a element =
    { priority : priority;
      data : 'a
    }

  let dummy = { priority = Obj.magic 0; data = Obj.magic 0 }

  let element_compare : 'a element -> 'a element -> int =
   fun left right ->
    assert (left != dummy);
    assert (right != dummy);
    Priority.compare left.priority right.priority

  type 'a t =
    { mutable size : int;
      mutable elements : 'a element array
    }

  let make : initial_capacity:int -> 'a t =
   fun ~initial_capacity ->
    let size = 0 in
    let elements = Array.make initial_capacity dummy in
    { size; elements }

  let is_empty : 'a t -> bool = fun queue -> queue.size = 0

  let size : 'a t -> int = fun queue -> queue.size

  let resize : 'a t -> unit =
   fun queue ->
    let current_capacity = Array.length queue.elements in
    let new_capacity =
      if current_capacity <= 2048
      then 2 * current_capacity
      else current_capacity + 2048
    in
    let new_elements = Array.make new_capacity dummy in
    Array.blit ~src:queue.elements ~src_pos:0 ~dst:new_elements ~dst_pos:0
      ~len:queue.size;
    queue.elements <- new_elements

  let parent : int -> int = fun i -> (i - 1) / 2

  let left_child : int -> int = fun i -> (2 * i) + 1

  let right_child : int -> int = fun i -> (2 * i) + 2

  let swap : 'a element array -> int -> int -> unit =
   fun arr i j ->
    assert (arr.(i) != dummy);
    assert (arr.(j) != dummy);
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp

  let upify : 'a element array -> start:int -> unit =
   fun arr ~start ->
    let i = ref start in
    while !i > 0 && element_compare arr.(!i) arr.(parent !i) > 0 do
      swap arr !i (parent !i);
      i := parent !i
    done

  let rec downify : 'a element array -> idx:int -> len:int -> unit =
   fun arr ~idx ~len ->
    let left = left_child idx in
    let right = right_child idx in
    let largest = ref idx in
    if left < len && element_compare arr.(left) arr.(!largest) > 0
    then largest := left;
    if right < len && element_compare arr.(right) arr.(!largest) > 0
    then largest := right;
    if !largest <> idx
    then (
      swap arr idx !largest;
      downify arr ~idx:!largest ~len)

  let rec add : 'a t -> priority:priority -> data:'a -> unit =
   fun queue ~priority ~data ->
    if Array.length queue.elements = queue.size
    then (
      resize queue;
      add queue ~priority ~data)
    else
      let elem = { priority; data } in
      let old_size = queue.size in
      Array.unsafe_set queue.elements old_size elem;
      queue.size <- succ old_size;
      upify queue.elements ~start:old_size

  let get : 'a t -> 'a element =
   fun queue ->
    match queue.size with
    | 0 -> fatal "trying to get an element from an empty priority queue"
    | _ ->
      let res = Array.unsafe_get queue.elements 0 in
      assert (res != dummy);
      res

  let remove : 'a t -> unit =
   fun queue ->
    match queue.size with
    | 0 -> fatal "trying to remove an element from an empty priority queue"
    | _ ->
      let old_size = queue.size in
      let index = pred old_size in
      swap queue.elements 0 index;
      queue.elements.(index) <- dummy;
      queue.size <- pred old_size;
      downify queue.elements ~idx:0 ~len:queue.size

  let get_and_remove : 'a t -> 'a element =
   fun queue ->
    match queue.size with
    | 0 ->
      fatal "trying to get and remove an element from an empty priority queue"
    | _ ->
      let res = Array.unsafe_get queue.elements 0 in
      assert (res != dummy);
      remove queue;
      res

  let iter : 'a t -> f:('a element -> unit) -> unit =
   fun queue ~f ->
    for i = 0 to pred queue.size do
      let elem = Array.unsafe_get queue.elements i in
      assert (elem != dummy);
      f elem
    done
end
