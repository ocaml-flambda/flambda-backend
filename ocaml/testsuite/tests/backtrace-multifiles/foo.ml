
let[@inline never] print s = (print_endline[@inlined never]) s

let[@inline] print_stack () =
  let st = Printexc.get_callstack 100 in
  (Printexc.print_raw_backtrace[@inlined never]) stdout st

let[@inline] f s =
  print_stack ();
  print s

let[@inline] g s =
  let s = (String.cat[@inlined never]) s " !" in
  f s;
  print_stack ();
  print "end of g"

let[@inline] h s =
  g s;
  print_stack ();
  print "end of h"

