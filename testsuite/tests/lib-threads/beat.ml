(* TEST

* hassysthreads
include systhreads
** bytecode
** native

*)

(* Test Thread.delay and its scheduling *)

open Printf

let tick (delay, count) =
  while true do
    Thread.delay delay;
    incr count
  done

let _ =
  let c1 = ref 0 and c2 = ref 0 in
  ignore (Thread.create tick (0.0333333333, c1));
  ignore (Thread.create tick (0.05, c2));
  Thread.delay 0.3;
  let n1 = !c1 and n2 = !c2 in
  if n1 >= 8 && n1 <= 10 && n2 >= 5 && n2 <= 7
  then printf "passed\n"
  else printf "FAILED (n1 = %d, n2 = %d)\n" n1 n2
