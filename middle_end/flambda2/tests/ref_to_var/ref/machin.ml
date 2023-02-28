type 'a ref = { mutable contents : 'a }

external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"

external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"

external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"
(* external incr : (int ref[@local_opt]) -> unit = "%incr"
 * external decr : (int ref[@local_opt]) -> unit = "%decr" *)

external ( +. ) :
  (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
  = "%addfloat"

external ( + ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%addint"

let incr x = x := !x + 1

(* let f x =
 *   let a = ref x in
 *   incr a;
 *   !a *)

let f b x =
  let a = ref x in
  let y, z =
    if b
    then (
      incr a;
      a, a)
    else a, a
  in
  incr y;
  incr z;
  z := !z + !y;
  !a

(* let f b x =
 *   let a = ref x in
 *   while b do
 *     incr a;
 *   done;
 *   !a *)
