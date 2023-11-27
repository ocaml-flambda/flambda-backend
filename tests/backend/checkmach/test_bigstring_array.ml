module Bigstring = struct
  open Bigarray
  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  external unsafe_set_64 : t -> int -> (int64[@local]) -> unit = "%caml_bigstring_set64u"

  external unsafe_get_64 : t -> int -> (int64[@local]) = "%caml_bigstring_get64u"

  let length = Array1.dim

  let[@inline always] check_args ~loc ~pos ~len (bstr : t) =
    let bstr_len = length bstr in
    if pos < 0 || len < 0 || bstr_len - pos < len then raise (Failure "boo")
  ;;

  let[@inline always] set_int64 (t : t) (pos : int) ((v : int64) [@local]) : unit =
    check_args ~loc:"set_64" ~pos ~len:8 t;
    unsafe_set_64 t pos v

  let[@inline always] get_int64 (t : t) (pos : int) : (int64[@local]) =
    check_args ~loc:"set_64" ~pos ~len:8 t;
    exclave_ (unsafe_get_64 t pos)
end

type t = Bigstring.t array

(* set and get should be zero_alloc but they are not. at least print a user-friendly
   error message that explains why. *)
let[@zero_alloc] set (t:t) ~table pos word =
  Bigstring.set_int64 t.(table) pos word

let[@zero_alloc] get t ~table pos = exclave_
    Bigstring.get_int64 t.(table) pos
