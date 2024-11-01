module Conv : sig
  val int64_to_int_exn : local_ int64 -> int
end = struct
  (* code extracted from [Base.Int_conversions] *)

  external globalize_int64 : local_ int64 -> int64 = "%obj_dup"

  let int64_to_string = Stdlib.Int64.to_string
  let int_to_int64 = Stdlib.Int64.of_int
  let max_value = Stdlib.max_int
  let min_value = Stdlib.min_int
  type 'a compare__local = local_ 'a -> local_ 'a -> int

  external poly_compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int = "%compare"
  let compare_int64__local : int64 compare__local = poly_compare

  let convert_failure x a b to_string =
    failwith (Printf.sprintf
                "conversion from %s to %s failed: %s is out of range"
                a
                b
                (to_string x)
             )
  [@@cold] [@@inline never] [@@local never] [@@specialise never]
  ;;

  let[@cold] [@inline never] [@local never] [@specialise never] int64_to_int_failure x =
    convert_failure
      (Stdlib.Int64.add
         (globalize_int64 x)
         0L (* force int64 boxing to be here under flambda2 *))
      "int64"
      "int"
      int64_to_string
  ;;

  let int64_to_int_trunc = Stdlib.Int64.to_int

  let int64_is_representable_as_int =
    let min = int_to_int64 min_value in
    let max = int_to_int64 max_value in
    fun x -> compare_int64__local min x <= 0 && compare_int64__local x max <= 0
  ;;

  let int64_to_int_exn x =
    if int64_is_representable_as_int x then
      int64_to_int_trunc x
    else
      let x = Int64.add (globalize_int64 x) (Sys.opaque_identity 0L) in
      int64_to_int_failure x
  ;;

end
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





