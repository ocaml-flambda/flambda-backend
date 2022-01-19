include Targetint

let size_in_bytes_as_targetint =
  match size with 32 -> of_int32 4l | 64 -> of_int64 8L | _ -> assert false

let to_uint64_exn t =
  match repr t with
  | Int32 t -> Numbers_extra.Uint64.of_nonnegative_int32_exn t
  | Int64 t -> Numbers_extra.Uint64.of_nonnegative_int64_exn t
