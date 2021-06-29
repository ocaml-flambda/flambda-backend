module Location : sig
  type t = {
    filename : string;
    line : int;
    start_char : int;
    end_char : int;
    defname : string;
  }

  val to_string : t -> string
  val unknown : t
end

module Writer : sig
  type t
  val create : unit -> t
  val max_length : int
  (* put_location never writes more than max_length bytes *)
  val put_location : t -> Buf.Write.t -> int * Location.t list -> unit
end

module Reader : sig
  type t
  val create : unit -> t
  val get_location : t -> Buf.Read.t -> int * Location.t list
end
