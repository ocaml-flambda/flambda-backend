(* val at_exit : (unit -> unit) -> unit *)

val exit : int -> 'a
val do_at_exit : unit -> unit
(*

(let
  (id/7 = (function param/9 0a)
   exit_function/11 = (makemutable 0 id/7)
   do_at_exit/12 = (function param/14 (apply (field_mut 0 exit_function/11) 0a))
   _at_exit/15 = (function _f/17 0a)
   exit/18 = (function retcode/20[int] (seq (apply do_at_exit/12 0a) (caml_sys_exit retcode/20))))
  (makeblock 0 exit/18 do_at_exit/12))

*)


(*

val do_at_exit : unit -> unit
val exit : int -> 'a

(let
  (id/7 = (function param/9 0a)
   exit_function/11 = (makemutable 0 id/7)
   do_at_exit/12 = (function param/14 (apply (field_mut 0 exit_function/11) 0a))
   _at_exit/15 = (function _f/17 0a)
   exit/18 = (function retcode/20[int] (seq (apply do_at_exit/12 0a) (caml_sys_exit retcode/20))))
  (makeblock 0 do_at_exit/12 exit/18))

*)
