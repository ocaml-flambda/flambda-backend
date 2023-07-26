[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Cell = sig
  type t
  val to_string : t -> string
  val empty : unit -> t
end

module type T = sig
  type row
  type t
  type cell

  val column_names : t -> string list
  val rows : t -> row list
  val set_rows : t -> row list -> unit

  val empty_row : string -> string list -> row
  val add_row : t -> row -> unit
  val add_empty_row : t -> string -> unit
  val update_row : row -> string -> (cell -> cell) -> row
  val create : string list -> t

  val print : t -> string -> unit
end

module Make(C : Cell) : T with type cell = C.t
