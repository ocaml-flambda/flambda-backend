module type Cell = sig
  type t
  val to_string : t -> string
  val empty : unit -> t
end

module Make(C : Cell) : sig
  type row
  type t

  val column_names : t -> string list
  val rows : t -> row list

  val empty_row : string -> string list -> row
  val add_row : t -> row -> unit
  val update_row : row -> string -> (C.t -> C.t) -> row
  val create : string list -> t

  val row_to_string : row -> string
  val to_string : t -> string
end