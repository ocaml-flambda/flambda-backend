[@@@ocaml.warning "+a-30-40-41-42"]

(** Definition of stack classes for a given backend.

    The number of stack slot classes may differ from the number of register classes.
    On x86, we use the same register class for floating point and SIMD vector registers,
    but they take up different amounts of space on the stack. *)
module type T = sig
  (** The "enum" representing the different classes. *)
  type t

  (** The list of all classes. *)
  val all : t list

  val equal : t -> t -> bool

  val hash : t -> int

  val tag : t -> string

  val print : Format.formatter -> t -> unit

  val size_in_bytes : t -> int

  val of_machtype : Cmm.machtype_component -> t
end

(** Definition of tables with register classes as keys. All registers classes
    are always bound. *)
module type Tbl = sig
  type stack_class

  type 'a t

  (** Creates a table with all register classes set to the passed value. *)
  val make : 'a -> 'a t

  (** Creates a table by calling [f] on each and every register class to
      get the initial value for that class. *)
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

module Make_tbl (SC : T) : Tbl with type stack_class = SC.t
