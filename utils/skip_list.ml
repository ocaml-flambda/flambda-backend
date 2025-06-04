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
    { data : elem;
      prev : pillar;
      next : pillar
    }

  and pillar = node option array

  (* CR-soon xclerc for xclerc: consider avoiding the `option` like we do in
     `Doubly_linked_list`. *)
  type t =
    { max_skip_level : int;
      skip_factor : float;
      head : pillar
    }

  type cell =
    { node : node;
      t : t
    }

  let value : cell -> 'a = fun cell -> cell.node.data

  let next : cell -> cell option =
   fun cell ->
    match Array.unsafe_get cell.node.next 0 with
    | None -> None
    | Some node -> Some { cell with node }

  let cut : cell -> unit =
   fun cell ->
    match Array.unsafe_get cell.node.prev 0 with
    | None ->
      for level = 0 to pred (Array.length cell.t.head) do
        Array.unsafe_set cell.t.head level None
      done
    | Some node ->
      let curr_node : node ref = ref node in
      let curr_pillar : pillar ref = ref node.next in
      for level = 0 to cell.t.max_skip_level do
        while level >= Array.length !curr_pillar do
          match Array.unsafe_get !curr_node.prev 0 with
          | None -> curr_pillar := cell.t.head
          | Some prev_node ->
            curr_node := prev_node;
            curr_pillar := prev_node.next
        done;
        Array.unsafe_set !curr_pillar level None
      done

  let delete_curr : cell -> unit =
   fun cell ->
    let node = cell.node in
    for level = 0 to pred (Array.length node.prev) do
      match Array.unsafe_get node.prev level with
      | None ->
        Array.unsafe_set cell.t.head level (Array.unsafe_get node.next level)
      | Some prev ->
        Array.unsafe_set prev.next level (Array.unsafe_get node.next level)
    done;
    let level = ref 0 in
    while
      !level < Array.length node.next
      && Option.is_some (Array.unsafe_get node.next !level)
    do
      match Array.unsafe_get node.next !level with
      | None -> assert false
      | Some next ->
        Array.unsafe_set next.prev !level (Array.unsafe_get node.prev !level);
        incr level
    done

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
    let head = Array.make (succ max_skip_level) None in
    { max_skip_level; skip_factor; head : node option array }

  let length : t -> int =
    let rec aux (node : node option) acc =
      match node with
      | None -> acc
      | Some node -> aux (Array.unsafe_get node.next 0) (succ acc)
    in
    fun t -> aux (Array.unsafe_get t.head 0) 0

  let clear : t -> unit =
   fun t ->
    for level = 0 to pred (Array.length t.head) do
      Array.unsafe_set t.head level None
    done

  let[@inline] hd_cell : t -> cell option =
   fun t ->
    match Array.unsafe_get t.head 0 with
    | None -> None
    | Some node -> Some { node; t }

  let[@inline] should_advance (i : int) (node : node option array)
      (value : elem) : bool =
    match Array.unsafe_get node i with
    | None -> false
    | Some (node : node) -> OT.compare node.data value < 0

  let insert : t -> elem -> unit =
   fun t elem ->
    let prev_nodes : node option array =
      (* previous node for each level *)
      Array.make (succ t.max_skip_level) None
    in
    let update : pillar array =
      (* pillar to update for each level *)
      Array.make (succ t.max_skip_level) t.head
    in
    let curr : pillar ref = ref t.head in
    let prev : node option ref = ref None in
    for i = t.max_skip_level downto 0 do
      while should_advance i !curr elem do
        match Array.unsafe_get !curr i with
        | None -> assert false
        | Some node as res ->
          prev := res;
          curr := node.next
      done;
      Array.unsafe_set update i !curr;
      Array.unsafe_set prev_nodes i !prev
    done;
    let level = random_level t in
    let prev : node option array = Array.make (succ level) None in
    let next : node option array = Array.make (succ level) None in
    for i = 0 to level do
      Array.unsafe_set prev i (Array.unsafe_get prev_nodes i);
      Array.unsafe_set next i (Array.unsafe_get (Array.unsafe_get update i) i)
    done;
    let node = { data = elem; prev; next } in
    for i = 0 to level do
      (match Array.unsafe_get (Array.unsafe_get update i) i with
      | None -> ()
      | Some n -> Array.unsafe_set n.prev i (Some node));
      Array.unsafe_set (Array.unsafe_get update i) i (Some node)
    done;
    ()

  let iter : t -> f:(elem -> unit) -> unit =
    let rec aux (node : node option) f =
      match node with
      | None -> ()
      | Some node ->
        f node.data;
        aux (Array.unsafe_get node.next 0) f
    in
    fun t ~f -> aux (Array.unsafe_get t.head 0) f

  let fold_left : t -> f:('a -> elem -> 'a) -> init:'a -> 'a =
    let rec aux (node : node option) f acc =
      match node with
      | None -> acc
      | Some node -> aux (Array.unsafe_get node.next 0) f (f acc node.data)
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
    let rec aux (node : node option) f =
      match node with
      | None -> false
      | Some node -> f node.data || aux (Array.unsafe_get node.next 0) f
    in
    fun t ~f -> aux (Array.unsafe_get t.head 0) f

  let to_list : t -> elem list =
   fun t ->
    (* CR xclerc for xclerc: avoid the `List.rev`. *)
    let res = ref [] in
    let curr = ref (Array.unsafe_get t.head 0) in
    while !curr <> None do
      res := (Option.get !curr).data :: !res;
      curr := Array.unsafe_get (Option.get !curr).next 0
    done;
    List.rev !res

  (* CR-soon xclerc: the printing function below are for debugging and should
     probably be deleted. *)
  let rec print_aux :
      node option -> (elem -> string) -> level:int -> acc_length:int -> unit =
   fun node f ~level ~acc_length ->
    match node with
    | None -> Printf.printf "/// length=%d\n%!" acc_length
    | Some node ->
      Printf.printf "%s " (f node.data);
      print_aux
        (Array.unsafe_get node.next level)
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
        | None -> false
        | Some _ ->
          if not some_part
          then
            Misc.fatal_errorf
              "Skip_list: inconsistent `%s` pillar (`Some _` at level %d, \
               after None value)"
              name level;
          true
      in
      invariant_pillar_some_at_bottom name pillar (succ level) some_part

  let rec invariant_nodes : node option -> unit =
   fun node ->
    match node with
    | None -> ()
    | Some node ->
      invariant_pillar_height "prev" (Array.length node.prev - 1) node.prev;
      invariant_pillar_height "next" (Array.length node.next - 1) node.next;
      invariant_pillar_some_at_bottom "prev" node.prev 0 true;
      invariant_pillar_some_at_bottom "next" node.next 0 true;
      invariant_nodes node.next.(0)

  let rec invariant_level :
      prev:node option -> node:node option -> level:int -> unit =
   fun ~prev ~node ~level ->
    match node with
    | None -> ()
    | Some node as new_prev ->
      let is_consistent_links, is_consistent_order =
        match node.prev.(level), prev with
        | Some _, None | None, Some _ -> false, true
        | None, None -> true, true
        | Some prev_from_link, Some prev_as_passed ->
          ( prev_from_link == prev_as_passed,
            OT.compare prev_as_passed.data node.data <= 0 )
      in
      if not (is_consistent_links && is_consistent_order)
      then
        Misc.fatal_errorf
          "Skip_list: inconsistent list at level %d (links=%B, order=%B)" level
          is_consistent_links is_consistent_order;
      invariant_level ~prev:new_prev ~node:node.next.(level) ~level

  let invariant : t -> unit =
   fun t ->
    invariant_nodes t.head.(0);
    for level = 0 to pred (Array.length t.head) do
      invariant_level ~prev:None ~node:(Array.unsafe_get t.head level) ~level
    done

  module Cursor = struct
    type nonrec t =
      { t : t;
        mutable node : node
      }

    let[@inline] value : t -> elem = fun cursor -> cursor.node.data

    let[@inline] next : t -> bool =
     fun cursor ->
      match Array.unsafe_get cursor.node.next 0 with
      | None -> false
      | Some node ->
        cursor.node <- node;
        true

    let delete_and_next : t -> bool =
     (* CR-soon xclerc for xclerc: factor out with the version above. *)
     fun cursor ->
      let next_node =
        match Array.unsafe_get cursor.node.next 0 with
        | None -> None
        | Some node -> Some node
      in
      let node = cursor.node in
      for level = 0 to pred (Array.length node.prev) do
        match Array.unsafe_get node.prev level with
        | None ->
          Array.unsafe_set cursor.t.head level
            (Array.unsafe_get node.next level)
        | Some prev ->
          Array.unsafe_set prev.next level (Array.unsafe_get node.next level)
      done;
      let level = ref 0 in
      while
        !level < Array.length node.next
        && Option.is_some (Array.unsafe_get node.next !level)
      do
        match Array.unsafe_get node.next !level with
        | None -> assert false
        | Some next ->
          Array.unsafe_set next.prev !level (Array.unsafe_get node.prev !level);
          incr level
      done;
      match next_node with
      | None -> false
      | Some next_node ->
        cursor.node <- next_node;
        true

    let cut : t -> unit =
     fun cursor ->
      match Array.unsafe_get cursor.node.prev 0 with
      | None ->
        for level = 0 to pred (Array.length cursor.t.head) do
          Array.unsafe_set cursor.t.head level None
        done
      | Some node ->
        let curr_node : node ref = ref node in
        let curr_pillar : pillar ref = ref node.next in
        for level = 0 to cursor.t.max_skip_level do
          while level >= Array.length !curr_pillar do
            match Array.unsafe_get !curr_node.prev 0 with
            | None -> curr_pillar := cursor.t.head
            | Some prev_node ->
              curr_node := prev_node;
              curr_pillar := prev_node.next
          done;
          Array.unsafe_set !curr_pillar level None
        done
  end

  let create_cursor_hd : t -> Cursor.t option =
   fun t ->
    match Array.unsafe_get t.head 0 with
    | None -> None
    | Some node -> Some { node; t }
end
[@@inline]
