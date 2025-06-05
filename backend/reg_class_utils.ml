[@@@ocaml.warning "+a-40-41-42"]

module List = ListLabels

module type T = sig
  type t

  val all : t list

  val first_available_register : t -> int

  val num_available_registers : t -> int

  val num_registers : t -> int

  val dwarf_register_numbers : t -> int array

  val register_name : Cmm.machtype_component -> int -> string

  val equal : t -> t -> bool

  val hash : t -> int

  val print : Format.formatter -> t -> unit

  val of_machtype : Cmm.machtype_component -> t
end

module type Tbl = sig
  type reg_class

  type 'a t

  val init : f:(reg_class -> 'a) -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val iter : 'a t -> f:(reg_class -> 'a -> unit) -> unit

  val find : 'a t -> reg_class -> 'a
end

module Make_tbl (RC : T) : Tbl with type reg_class = RC.t = struct
  module Tbl = Hashtbl.Make (RC)

  type reg_class = RC.t

  type 'a t = 'a Tbl.t

  let init : f:(reg_class -> 'a) -> 'a t =
   fun ~f ->
    let res = Tbl.create (List.length RC.all) in
    List.iter RC.all ~f:(fun reg_class ->
        Tbl.replace res reg_class (f reg_class));
    res

  let map : 'a t -> f:('a -> 'b) -> 'b t =
   fun tbl ~f ->
    let res = Tbl.create (List.length RC.all) in
    List.iter RC.all ~f:(fun reg_class ->
        Tbl.replace res reg_class (f (Tbl.find tbl reg_class)));
    res

  let iter : 'a t -> f:(reg_class -> 'a -> unit) -> unit =
   fun tbl ~f -> Tbl.iter f tbl

  let find : 'a t -> reg_class -> 'a =
   fun tbl reg_class -> Tbl.find tbl reg_class
end
