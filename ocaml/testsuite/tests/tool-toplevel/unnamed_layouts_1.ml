(* TEST
 {
   toplevel;
 }{
   toplevel.opt;
 }
*)

(* The layout of the expression in a [Pstr_eval] node must be value, when using
   the toplevel, to accomodate a transformation done in
   [Opttoploop.execute_phrase].  It would be fine to allow these to be non-value
   in the bytecode toplevel but for now we don't. *)

#3.14;;
