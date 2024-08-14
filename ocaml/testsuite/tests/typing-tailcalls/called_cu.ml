(* This file is part of the caller_cu test. *)

let[@inline never] tailcalls_arg ~fn = fn 1

let[@inline never] doesn't_tailcall_arg ~fn = fn 2; ()

