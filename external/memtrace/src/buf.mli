module Write : sig
  (** A [t] is a subsequence of a Bytes.t, to be written sequentially.

      None of the operations below allocate or resize the underlying byte buffer -
      the underlying Bytes.t is managed by the caller, and may be shared between
      mutiple [t]s *)
  type t = private {
    buf : Bytes.t;
    mutable pos : int;
    pos_end : int;
  }

  val of_bytes : Bytes.t -> t
  val of_bytes_sub : Bytes.t -> pos:int -> pos_end:int -> t
  val remaining : t -> int

  (** [write_fd fd b] writes the bytes written to b to the fd.

      No bufs are invalidated *)
  val write_fd : Unix.file_descr -> t -> unit

  (** Writing to a buf. All types are written little-endian.
      All functions raise Overflow if there is insufficient space remaining *)

  exception Overflow of int
  val put_8  : t -> int -> unit
  val put_16 : t -> int -> unit
  val put_32 : t -> int32 -> unit
  val put_64 : t -> int64 -> unit
  val put_float : t -> float -> unit
  val put_string : t -> string -> unit
  val put_vint : t -> int -> unit

  (** The skip_t functions reserve space to be filled by a later update_t.
      (for e.g. length fields that are only known when writing is finished) *)

  type position_8 = private int
  val skip_8  : t -> position_8
  val update_8  : t -> position_8 -> int -> unit

  type position_16 = private int
  val skip_16 : t -> position_16
  val update_16 : t -> position_16 -> int -> unit

  type position_32 = private int
  val skip_32 : t -> position_32
  val update_32 : t -> position_32 -> int32 -> unit

  type position_64 = private int
  val skip_64 : t -> position_64
  val update_64 : t -> position_64 -> int64 -> unit

  type position_float = private int
  val skip_float : t -> position_float
  val update_float : t -> position_float -> float -> unit
end

module Read : sig
  (** A [t] is a subsequence of a Bytes.t, to be read sequentially.

      None of the operations below allocate or resize the underlying byte buffer -
      the underlying Bytes.t is managed by the caller, and may be shared between
      mutiple [t]s *)
  type t = private {
    buf : Bytes.t;
    mutable pos : int;
    pos_end : int;
  }

  val of_bytes : Bytes.t -> t
  val of_bytes_sub : Bytes.t -> pos:int -> pos_end:int -> t
  val remaining : t -> int

  (** [split_buf b len] splits b into (a, b), where a contains at most len bytes and b
      contains the rest, if any.

      The two returned parts share the same underlying Bytes.t *)
  val split : t -> int -> (t * t)

  (** [read_fd fd byt] returns a buf containing bytes read from fd,
      whose underlying buffer is byt.

      All [t]s sharing this underlying buffer are invalidated *)
  val read_fd : Unix.file_descr -> Bytes.t -> t

  (** [refill_fd fd b] returns a buf containing the contents of b followed
      by bytes read from fd, whose underlying buffer is that of b.

      All bufs sharing this underlying buffer (including b) are invalidated *)
  val refill_fd : Unix.file_descr -> t -> t

  val empty : t

  (** Reading from a buf. All types are read little-endian.
      All functions raise Underflow if there are insufficient bytes remaining. *)

  exception Underflow of int
  val get_8  : t -> int
  val get_16 : t -> int
  val get_32 : t -> int32
  val get_64 : t -> int64
  val get_float : t -> float
  val get_string : t -> string
  val get_vint : t -> int (* NB: may overflow if read on 32-bit machines *)
end
