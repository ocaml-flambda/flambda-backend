let __dummy1__ _ = assert false [@@inline never]

external __dummy2__ : unit -> 'a = "%opaque"

external __ignore__ : 'a -> unit = "%ignore"

module Sexp = struct
  type t
end

let return x = x [@@inline never] [@@local never]

type w = A of int

let of_prev = return (fun _ -> __dummy1__ ()) [@@inline never] [@@local never]

let of_prev (A z) = A ((__dummy2__ ()) z)
  [@@ocaml.warning "-20"] [@@inline never]

type t = unit

let t_of_sexp _ =
  __ignore__
    (of_prev
       (if __dummy2__ ()
       then A (__dummy2__ ())
       else (return (fun _ -> __dummy1__ ())) (__dummy2__ ())))
