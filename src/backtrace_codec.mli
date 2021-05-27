
module Writer : sig
  type t
  val create : unit -> t
  (* Maximum number of bytes written by put_backtrace *)
  val max_length : int
  val put_backtrace :
    t ->
    Buf.Write.t ->
    alloc_id:int ->
    callstack:int array ->
    callstack_len:int ->
    log_new_location:(index:int -> unit) ->
    int (* number of encoded slots *)
  val put_cache_verifier : t -> Buf.Write.t -> unit
  val put_dummy_verifier : Buf.Write.t -> unit
end

module Reader : sig
  type t
  val create : unit -> t
  val get_backtrace :
    t ->
    Buf.Read.t ->
    nencoded:int ->
    common_pfx_len:int ->
    (int array * int)
  val skip_backtrace :
    t ->
    Buf.Read.t ->
    nencoded:int ->
    common_pfx_len:int ->
    unit
  val check_cache_verifier : t -> Buf.Read.t -> bool
  val skip_cache_verifier : Buf.Read.t -> unit
end
