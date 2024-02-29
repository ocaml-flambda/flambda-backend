[@@@ocaml.warning "+a-30-40-41-42"]

module List = ListLabels

module type T = sig
  type t

  val all : t list

  val equal : t -> t -> bool

  val hash : t -> int

  val tag : t -> string

  val print : Format.formatter -> t -> unit

  val size_in_bytes : t -> int

  val of_machtype : Cmm.machtype_component -> t
end

module type Tbl = sig
  type stack_class

  type 'a t

  val make : 'a -> 'a t

  val init : f:(stack_class -> 'a) -> 'a t

  val copy : 'a t -> 'a t

  val copy_values : from:'a t -> to_:'a t -> unit

  val find : 'a t -> stack_class -> 'a

  val replace : 'a t -> stack_class -> 'a -> unit

  val update : 'a t -> stack_class -> f:('a -> 'a) -> unit

  val iter : 'a t -> f:(stack_class -> 'a -> unit) -> unit

  val fold : 'a t -> f:(stack_class -> 'a -> 'b -> 'b) -> init:'b -> 'b

  val exists : 'a t -> f:(stack_class -> 'a -> bool) -> bool

  val total_size_in_bytes_for_class : int t -> stack_class:stack_class -> int

  val total_size_in_bytes : int t -> int

  val total_size_in_slots : int t -> int

  val equal_machtype : Reg.t -> Reg.t -> bool
end

module Make_tbl (SC : T) : Tbl with type stack_class = SC.t = struct
  module Tbl = Hashtbl.Make (SC)

  type stack_class = SC.t

  type 'a t = 'a Tbl.t

  let init : f:(stack_class -> 'a) -> 'a t =
   fun ~f ->
    let res = Tbl.create (List.length SC.all) in
    List.iter SC.all ~f:(fun stack_class ->
        Tbl.replace res stack_class (f stack_class));
    res

  let make : 'a -> 'a t = fun x -> init ~f:(fun _stack_class -> x)

  let copy : 'a t -> 'a t = fun tbl -> Tbl.copy tbl

  let copy_values : from:'a t -> to_:'a t -> unit =
   fun ~from ~to_ ->
    assert (Tbl.length from = Tbl.length to_);
    List.iter SC.all ~f:(fun stack_class ->
        Tbl.replace to_ stack_class (Tbl.find from stack_class))

  let find : 'a t -> stack_class -> 'a =
   fun tbl stack_class ->
    match Tbl.find_opt tbl stack_class with
    | None ->
      Misc.fatal_errorf "stack class %a missing from table" SC.print stack_class
    | Some x -> x

  let replace : 'a t -> stack_class -> 'a -> unit =
   fun tbl stack_class x -> Tbl.replace tbl stack_class x

  let update : 'a t -> stack_class -> f:('a -> 'a) -> unit =
   fun tbl stack_class ~f ->
    let curr = find tbl stack_class in
    replace tbl stack_class (f curr)

  let iter : 'a t -> f:(stack_class -> 'a -> unit) -> unit =
   fun tbl ~f -> Tbl.iter f tbl

  let fold : 'a t -> f:(stack_class -> 'a -> 'b -> 'b) -> init:'b -> 'b =
   fun tbl ~f ~init -> Tbl.fold f tbl init

  exception Found

  let exists : 'a t -> f:(stack_class -> 'a -> bool) -> bool =
   fun tbl ~f ->
    try
      Tbl.iter (fun stack_class v -> if f stack_class v then raise Found) tbl;
      false
    with Found -> true

  let total_size_in_bytes_for_class :
      int Tbl.t -> stack_class:stack_class -> int =
   fun tbl ~stack_class ->
    let num = Tbl.find tbl stack_class in
    num * SC.size_in_bytes stack_class

  let total_size_in_bytes : int Tbl.t -> int =
   fun tbl ->
    List.fold_left SC.all ~init:0 ~f:(fun acc stack_class ->
        acc + total_size_in_bytes_for_class tbl ~stack_class)

  let total_size_in_slots : int Tbl.t -> int =
    fun tbl -> fold tbl ~init:0 ~f:(fun _stack_class num acc -> num + acc)

  let equal_machtype : Reg.t -> Reg.t -> bool = fun left right ->
    SC.equal (SC.of_machtype left.typ) (SC.of_machtype right.typ)
end
