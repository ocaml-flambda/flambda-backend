module Int64_u : sig
  type t = int64#

  val to_int_exn : t -> int

end = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) = "%box_int64"

  let[@inline] to_int_exn t = (Conv.int64_to_int_exn [@inlined hint]) (to_int64 t)
end

module Offset : sig
  type t : bits64

  val to_int : local_ t -> int [@@zero_alloc]
end = struct
  type t = Int64_u.t

  let[@inline] to_int t = Int64_u.to_int_exn t
end

let[@inline] [@zero_alloc] bounds_check ~offset
  =
  let pos = Offset.to_int offset in
  if pos < 0
  then
    raise (Failure "Boo")


let[@inline] [@zero_alloc] read_exn offset chunk_length  = exclave_
  bounds_check ~offset;
  Offset.to_int offset





