type t =
  { rand : Random.State.t;
    one_log1m_lambda : float }

let default_rand () =
  Random.State.make [| 0x52b87efb; 0x332235ea; 0x5f813723; 0x057b9dff |]

let make ?(rand = default_rand ()) ~sampling_rate () =
  let one_log1m_lambda =
    if sampling_rate >= 1. then 0. else 1./.log1p(-. sampling_rate) in
  { rand; one_log1m_lambda }

(* port of log_approx in runtime/memprof.c to OCaml
   see https://github.com/ocaml/ocaml/pull/9466

   slightly different results because of double precision,
   however, this difference is ~100x smaller than the
   error introduced by the original approximation *)

let log_approx n =
  let f = Int64.bits_of_float (Int32.(to_float (add one (shift_left n 1)))) in
  let exp = Int64.(to_int (shift_right f 52)) in
  let exp = float_of_int (exp + (127 - 1023 + 1)) in
  let x = Int64.(float_of_bits (logor (logand f 0xfffffffffffffL) 0x3ff0000000000000L)) in
  x *. (2.104659476859 +. x *. (-0.720478916626 +. x *. 0.107132064797))
    +. (-111.701724334061 +. 0.6931471805 *. exp)

(* Draw from the geometric distribution by:
   (1) Draw a uniform random number (via Random.State.bits)
   (2) Construct an exponentially-distributed random variable as the log of (1)
   (3) Scale the distribution of (2) to have the correct mean by multiplying
   (4) Convert to a geometric distribution by taking the floor + 1 *)
let draw s =
  let uniform_rand = Int32.of_int (Random.State.bits s.rand) in
  let exp_rand = log_approx uniform_rand in
  1 + int_of_float (exp_rand *. s.one_log1m_lambda)
