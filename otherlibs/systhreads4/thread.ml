# 2 "thread.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* User-level threads *)

[@@@ocaml.flambda_o3]

type t : value mod contended portable

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_cleanup : unit -> unit @@ portable = "caml_thread_cleanup"
external thread_new : (unit -> unit) @ once -> t @@ portable = "caml_thread_new"
external thread_uncaught_exception : exn -> unit @@ portable =
            "caml_thread_uncaught_exception"

external yield : unit -> unit @@ portable = "caml_thread_yield"
external self : unit -> t @@ portable = "caml_thread_self" [@@noalloc]
external id : t -> int @@ portable = "caml_thread_id" [@@noalloc]
external join : t -> unit @@ portable = "caml_thread_join"
external exit_stub : unit -> unit @@ portable = "caml_thread_exit"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let[@inline never] check_memprof_cb () = ref ()

let default_uncaught_exception_handler = thread_uncaught_exception

let uncaught_exception_handler = Atomic.make { Modes.Portable.portable = default_uncaught_exception_handler }

let set_uncaught_exception_handler (fn @ portable) = Atomic.Contended.set uncaught_exception_handler { Modes.Portable.portable = fn }

exception Exit

let create (fn @ once) arg =
  thread_new
    (fun () ->
      try
        fn arg;
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      with
      | Exit ->
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      | exn ->
        let raw_backtrace = Printexc.get_raw_backtrace () in
        flush stdout; flush stderr;
        try
          (Atomic.Contended.get uncaught_exception_handler).portable exn
        with
        | Exit -> ()
        | exn' ->
          Printf.eprintf
            "Thread %d killed on uncaught exception %s\n"
            (id (self ())) (Printexc.to_string exn);
          Printexc.print_raw_backtrace stderr raw_backtrace;
          Printf.eprintf
            "Thread %d uncaught exception handler raised %s\n"
            (id (self ())) (Printexc.to_string exn');
          Printexc.print_backtrace stdout;
          flush stderr)

module Portable = struct
  let create (fn @ once portable) arg = create fn arg
end

let create (fn @ many) arg = create fn arg

let exit () =
  ignore (Sys.opaque_identity (check_memprof_cb ()));
  exit_stub ()

(* Thread.kill is currently not implemented due to problems with
   cleanup handlers on several platforms *)

let kill th = invalid_arg "Thread.kill: not implemented"

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let preempt_signal =
  match Sys.os_type with
  | "Win32" -> Sys.sigterm
  | _       -> Sys.sigvtalrm

let () =
  Sys.set_signal preempt_signal (Sys.Signal_handle preempt);
  thread_initialize ();
  Callback.Safe.register "Thread.at_shutdown" (fun () ->
    thread_cleanup();
    (* In case of DLL-embedded OCaml the preempt_signal handler
       will point to nowhere after DLL unloading and an accidental
       preempt_signal will crash the main program. So restore the
       default handler. *)
    Sys.Safe.set_signal preempt_signal Sys.Signal_default
  )

(* Wait functions *)

let delay = Unix.sleepf

let wait_read fd = ()
let wait_write fd = ()

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

external sigmask : Unix.sigprocmask_command -> int list -> int list @@ portable
   = "caml_thread_sigmask"
external wait_signal : int list -> int @@ portable = "caml_wait_signal"
