[@@@ocaml.warning "+a-30-40-41-42"]

(* CR-soon xclerc: add `open! Int_replace_polymorphic_compare`. *)

(* CR xclerc for xclerc: refactor. *)
let init_random : unit Lazy.t = lazy (Random.self_init ())

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type T = sig
  type elem

  type cell

  val value : cell -> elem

  val next : cell -> cell option

  val cut : cell -> unit

  val delete_curr : cell -> unit

  type t

  val make_empty : max_skip_level:int -> skip_factor:float -> unit -> t

  val length : t -> int

  val clear : t -> unit

  val hd_cell : t -> cell option

  val insert : t -> elem -> unit

  val iter : t -> f:(elem -> unit) -> unit

  val fold_left : t -> f:('a -> elem -> 'a) -> init:'a -> 'a

  val map : t -> f:(elem -> elem) -> t

  val exists : t -> f:(elem -> bool) -> bool

  val to_list : t -> elem list

  val print_for_debug : t -> f:(elem -> string) -> unit

  val invariant : t -> unit

  module Cursor : sig
    type t

    val value : t -> elem

    val next : t -> bool

    val delete_and_next : t -> bool

    val cut : t -> unit
  end

  val create_cursor_hd : t -> Cursor.t option
end

module Make (OT : OrderedType) : T with type elem = OT.t = struct
  type elem = OT.t

  type node =
    | Empty
    | Node of
        { data : elem;
          prev : pillar;
          next : pillar
        }

  and pillar = node array

  let[@inline] is_node : node -> bool =
   fun node -> match node with Empty -> false | Node _ -> true

  let[@inline] make_node_array n (v : node) : node array =
    match n with
    | 0 -> [||]
    | 1 -> [| v |]
    | 2 -> [| v; v |]
    | 3 -> [| v; v; v |]
    | 4 -> [| v; v; v; v |]
    | 5 -> [| v; v; v; v; v |]
    | 6 -> [| v; v; v; v; v; v |]
    | 7 -> [| v; v; v; v; v; v; v |]
    | 8 -> [| v; v; v; v; v; v; v; v |]
    | 9 -> [| v; v; v; v; v; v; v; v; v |]
    | 10 -> [| v; v; v; v; v; v; v; v; v; v |]
    | 11 -> [| v; v; v; v; v; v; v; v; v; v; v |]
    | 12 -> [| v; v; v; v; v; v; v; v; v; v; v; v |]
    | 13 -> [| v; v; v; v; v; v; v; v; v; v; v; v; v |]
    | 14 -> [| v; v; v; v; v; v; v; v; v; v; v; v; v; v |]
    | 15 -> [| v; v; v; v; v; v; v; v; v; v; v; v; v; v; v |]
    | _ -> assert false

  let[@inline] make_pillar_array n (v : pillar) : pillar array =
    match n with
    | 0 -> [||]
    | 1 -> [| v |]
    | 2 -> [| v; v |]
    | 3 -> [| v; v; v |]
    | 4 -> [| v; v; v; v |]
    | 5 -> [| v; v; v; v; v |]
    | 6 -> [| v; v; v; v; v; v |]
    | 7 -> [| v; v; v; v; v; v; v |]
    | 8 -> [| v; v; v; v; v; v; v; v |]
    | 9 -> [| v; v; v; v; v; v; v; v; v |]
    | 10 -> [| v; v; v; v; v; v; v; v; v; v |]
    | 11 -> [| v; v; v; v; v; v; v; v; v; v; v |]
    | 12 -> [| v; v; v; v; v; v; v; v; v; v; v; v |]
    | 13 -> [| v; v; v; v; v; v; v; v; v; v; v; v; v |]
    | 14 -> [| v; v; v; v; v; v; v; v; v; v; v; v; v; v |]
    | 15 -> [| v; v; v; v; v; v; v; v; v; v; v; v; v; v; v |]
    | _ -> assert false

  type t =
    { max_skip_level : int;
      skip_factor : float;
      head : pillar
    }

  type cell =
    { node : node; (* invariant: this node is not empty *)
      t : t
    }

  let value : cell -> 'a =
   fun cell ->
    match cell.node with
    | Empty -> assert false (* invariant: this node is not empty *)
    | Node { data; prev = _; next = _ } -> data

  let next : cell -> cell option =
   fun cell ->
    match cell.node with
    | Empty -> assert false (* invariant: this node is not empty *)
    | Node { data = _; prev = _; next } -> (
      match Array.unsafe_get next 0 with
      | Empty -> None
      | Node _ as node -> Some { cell with node })

  let cut_node : t -> node -> unit =
   fun t node ->
    match node with
    | Empty -> assert false (* invariant: this node is not empty *)
    | Node { data = _; prev = node_prev; next = _ } -> (
      match Array.unsafe_get node_prev 0 with
      | Empty ->
        for level = 0 to pred (Array.length t.head) do
          Array.unsafe_set t.head level Empty
        done
      | Node { data = _; prev = node_prev; next = node_next } as node ->
        let curr_node : node ref = ref node in
        let curr_node_prev : pillar ref = ref node_prev in
        let curr_pillar : pillar ref = ref node_next in
        for level = 0 to t.max_skip_level do
          while level >= Array.length !curr_pillar do
            match Array.unsafe_get !curr_node_prev 0 with
            | Empty -> curr_pillar := t.head
            | Node { data = _; prev = prev_node_prev; next = prev_node_next } as
              prev_node ->
              curr_node := prev_node;
              curr_node_prev := prev_node_prev;
              curr_pillar := prev_node_next
          done;
          Array.unsafe_set !curr_pillar level Empty
        done)

  let cut : cell -> unit = fun cell -> cut_node cell.t cell.node

  let delete_node : t -> node -> unit =
   fun t node ->
    match node with
    | Empty -> assert false (* invariant: this node is not empty *)
    | Node { data = _; prev = node_prev; next = node_next } ->
      for level = 0 to pred (Array.length node_prev) do
        match Array.unsafe_get node_prev level with
        | Empty ->
          Array.unsafe_set t.head level (Array.unsafe_get node_next level)
        | Node { data = _; prev = _; next = prev_next } ->
          Array.unsafe_set prev_next level (Array.unsafe_get node_next level)
      done;
      let level = ref 0 in
      while
        !level < Array.length node_next
        && is_node (Array.unsafe_get node_next !level)
      do
        match Array.unsafe_get node_next !level with
        | Empty -> assert false
        | Node { data = _; prev = next_prev; next = _ } ->
          Array.unsafe_set next_prev !level (Array.unsafe_get node_prev !level);
          incr level
      done

  let delete_curr : cell -> unit = fun cell -> delete_node cell.t cell.node

  let random_level : t -> int =
   fun t ->
    let level = ref 0 in
    while
      !level < t.max_skip_level && Random.bool () && Random.bool ()
      && Random.float 1.0 <= t.skip_factor
    do
      incr level
    done;
    assert (0 <= !level && !level <= t.max_skip_level);
    !level

  let make_empty : max_skip_level:int -> skip_factor:float -> unit -> t =
   fun ~max_skip_level ~skip_factor () ->
    assert (max_skip_level >= 1);
    assert (skip_factor >= 0. && skip_factor <= 1.);
    Lazy.force init_random;
    let max_skip_level = 3 in
    let head = make_node_array (succ max_skip_level) Empty in
    { max_skip_level; skip_factor; head }

  let length : t -> int =
    let rec aux (node : node) acc =
      match node with
      | Empty -> acc
      | Node { data = _; prev = _; next } ->
        aux (Array.unsafe_get next 0) (succ acc)
    in
    fun t -> aux (Array.unsafe_get t.head 0) 0

  let clear : t -> unit =
   fun t ->
    for level = 0 to pred (Array.length t.head) do
      Array.unsafe_set t.head level Empty
    done

  let[@inline] hd_cell : t -> cell option =
   fun t ->
    match Array.unsafe_get t.head 0 with
    | Empty -> None
    | Node _ as node -> Some { node; t }

  let[@inline] should_advance (i : int) (node : node array) (value : elem) :
      bool =
    match Array.unsafe_get node i with
    | Empty -> false
    | Node { data; prev = _; next = _ } -> OT.compare data value < 0

  let insert : t -> elem -> unit =
   fun t elem ->
    let prev_nodes : node array =
      (* previous node for each level *)
      make_node_array (succ t.max_skip_level) Empty
    in
    let update : pillar array =
      (* pillar to update for each level *)
      make_pillar_array (succ t.max_skip_level) t.head
    in
    let curr : pillar ref = ref t.head in
    let prev : node ref = ref Empty in
    for i = t.max_skip_level downto 0 do
      while should_advance i !curr elem do
        match Array.unsafe_get !curr i with
        | Empty -> assert false
        | Node { data = _; prev = _; next } as res ->
          prev := res;
          curr := next
      done;
      Array.unsafe_set update i !curr;
      Array.unsafe_set prev_nodes i !prev
    done;
    let level = random_level t in
    let prev : node array = make_node_array (succ level) Empty in
    let next : node array = make_node_array (succ level) Empty in
    for i = 0 to level do
      Array.unsafe_set prev i (Array.unsafe_get prev_nodes i);
      Array.unsafe_set next i (Array.unsafe_get (Array.unsafe_get update i) i)
    done;
    let node = Node { data = elem; prev; next } in
    for i = 0 to level do
      (match Array.unsafe_get (Array.unsafe_get update i) i with
      | Empty -> ()
      | Node { data = _; prev; next = _ } -> Array.unsafe_set prev i node);
      Array.unsafe_set (Array.unsafe_get update i) i node
    done;
    ()

  let iter : t -> f:(elem -> unit) -> unit =
    let rec aux (node : node) f =
      match node with
      | Empty -> ()
      | Node { data; prev = _; next } ->
        f data;
        aux (Array.unsafe_get next 0) f
    in
    fun t ~f -> aux (Array.unsafe_get t.head 0) f

  let fold_left : t -> f:('a -> elem -> 'a) -> init:'a -> 'a =
    let rec aux (node : node) f acc =
      match node with
      | Empty -> acc
      | Node { data; prev = _; next } ->
        aux (Array.unsafe_get next 0) f (f acc data)
    in
    fun t ~f ~init -> aux (Array.unsafe_get t.head 0) f init

  let map : t -> f:(elem -> elem) -> t =
   fun t ~f ->
    (* CR-someday xclerc for xclerc: this is not a very good implementation, but
       `map` is currently used only for debugging. *)
    let res =
      make_empty ~max_skip_level:t.max_skip_level ~skip_factor:t.skip_factor ()
    in
    iter t ~f:(fun elem -> insert res (f elem));
    res

  let exists : t -> f:(elem -> bool) -> bool =
    let rec aux (node : node) f =
      match node with
      | Empty -> false
      | Node { data; prev = _; next } ->
        f data || aux (Array.unsafe_get next 0) f
    in
    fun t ~f -> aux (Array.unsafe_get t.head 0) f

  let to_list : t -> elem list =
   fun t ->
    (* CR xclerc for xclerc: avoid the `List.rev`. *)
    let rec aux (node : node) acc =
      match node with
      | Empty -> List.rev acc
      | Node { data; prev = _; next } ->
        aux (Array.unsafe_get next 0) (data :: acc)
    in
    aux (Array.unsafe_get t.head 0) []

  (* CR-soon xclerc: the printing function below are for debugging and should
     probably be deleted. *)
  let rec print_aux :
      node -> (elem -> string) -> level:int -> acc_length:int -> unit =
   fun node f ~level ~acc_length ->
    match node with
    | Empty -> Printf.printf "/// length=%d\n%!" acc_length
    | Node { data; prev = _; next } ->
      Printf.printf "%s " (f data);
      print_aux
        (Array.unsafe_get next level)
        f ~level ~acc_length:(succ acc_length)

  let print_for_debug : t -> f:(elem -> string) -> unit =
   fun t ~f ->
    for level = Array.length t.head - 1 downto 0 do
      Printf.printf "[level %d] -> " level;
      print_aux (Array.unsafe_get t.head level) f ~level ~acc_length:0
    done

  let invariant_pillar_height : string -> int -> pillar -> unit =
   fun name level pillar ->
    let expected = succ level in
    let actual = Array.length pillar in
    if expected <> actual
    then
      Misc.fatal_errorf
        "Skip_list: inconsistent `%s` height (expected=%d, actual=%d)" name
        expected actual

  let rec invariant_pillar_some_at_bottom :
      string -> pillar -> int -> bool -> unit =
   fun name pillar level some_part ->
    if level >= Array.length pillar
    then ()
    else
      let some_part =
        match Array.unsafe_get pillar level with
        | Empty -> false
        | Node _ ->
          if not some_part
          then
            Misc.fatal_errorf
              "Skip_list: inconsistent `%s` pillar (`Some _` at level %d, \
               after None value)"
              name level;
          true
      in
      invariant_pillar_some_at_bottom name pillar (succ level) some_part

  let rec invariant_nodes : node -> unit =
   fun node ->
    match node with
    | Empty -> ()
    | Node { data = _; prev; next } ->
      invariant_pillar_height "prev" (Array.length prev - 1) prev;
      invariant_pillar_height "next" (Array.length next - 1) next;
      invariant_pillar_some_at_bottom "prev" prev 0 true;
      invariant_pillar_some_at_bottom "next" next 0 true;
      invariant_nodes next.(0)

  let rec invariant_level : prev:node -> node:node -> level:int -> unit =
   fun ~prev ~node ~level ->
    match node with
    | Empty -> ()
    | Node { data = node_data; prev = node_prev; next = node_next } as new_prev
      ->
      let is_consistent_links, is_consistent_order =
        match node_prev.(level), prev with
        | Node _, Empty | Empty, Node _ -> false, true
        | Empty, Empty -> true, true
        | ( (Node _ as prev_from_link),
            (Node { data = prev_as_passed_data; prev = _; next = _ } as
            prev_as_passed) ) ->
          ( prev_from_link == prev_as_passed,
            OT.compare prev_as_passed_data node_data <= 0 )
      in
      if not (is_consistent_links && is_consistent_order)
      then
        Misc.fatal_errorf
          "Skip_list: inconsistent list at level %d (links=%B, order=%B)" level
          is_consistent_links is_consistent_order;
      invariant_level ~prev:new_prev ~node:node_next.(level) ~level

  let invariant : t -> unit =
   fun t ->
    invariant_nodes t.head.(0);
    for level = 0 to pred (Array.length t.head) do
      invariant_level ~prev:Empty ~node:(Array.unsafe_get t.head level) ~level
    done

  module Cursor = struct
    type nonrec t =
      { t : t;
        mutable node : node (* invariant: this node is not empty *)
      }

    let[@inline] value : t -> elem =
     fun cursor ->
      match cursor.node with
      | Empty -> assert false (* invariant: this node is not empty *)
      | Node { data; prev = _; next = _ } -> data

    let[@inline] next : t -> bool =
     fun cursor ->
      match cursor.node with
      | Empty -> assert false (* invariant: this node is not empty *)
      | Node { data = _; prev = _; next } -> (
        match Array.unsafe_get next 0 with
        | Empty -> false
        | Node _ as node ->
          cursor.node <- node;
          true)

    let delete_and_next : t -> bool =
     fun cursor ->
      match cursor.node with
      | Empty -> assert false (* invariant: this node is not empty *)
      | Node { data = _; prev = _; next } as node -> (
        (* CR-soon xclerc for xclerc: it might be clearer to update the cursor
           first and then perform the deletion. *)
        let next_node =
          match Array.unsafe_get next 0 with
          | Empty -> None
          | Node _ as node -> Some node
        in
        delete_node cursor.t node;
        match next_node with
        | None -> false
        | Some node ->
          cursor.node <- node;
          true)

    let cut : t -> unit = fun cursor -> cut_node cursor.t cursor.node
  end

  let create_cursor_hd : t -> Cursor.t option =
   fun t ->
    match Array.unsafe_get t.head 0 with
    | Empty -> None
    | Node _ as node -> Some { node; t }
end
[@@inline]
