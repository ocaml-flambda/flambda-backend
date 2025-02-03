# 2 "callback.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(** Registering OCaml values with the C runtime.

   This module allows OCaml values to be registered with the C runtime
   under a symbolic name, so that C code can later call back registered
   OCaml functions, or raise registered OCaml exceptions.
*)

val register : string -> 'a -> unit
[@@alert unsafe_multidomain "Use [Callback.Safe.register]."]
(** [Callback.register n v] registers the value [v] under
   the name [n]. C code can later retrieve a handle to [v]
   by calling [caml_named_value(n)]. *)

val register_exception : string -> exn -> unit
[@@alert unsafe_multidomain "Use [Callback.Safe.register_exception]."]
(** [Callback.register_exception n exn] registers the
   exception contained in the exception value [exn]
   under the name [n]. C code can later retrieve a handle to
   the exception by calling [caml_named_value(n)]. The exception
   value thus obtained is suitable for passing as first argument
   to [raise_constant] or [raise_with_arg]. *)

(** Submodule containing non-backwards-compatible functions which enforce thread safety
    via modes. *)
module Safe : sig
  val register : string -> 'a @ portable -> unit @@ portable
  (** Like {!register}, but is safe to use in the presence of multiple domains.

      The provided value must be [portable] as registered values may be looked up from
      any domain. *)

  val register_exception : string -> exn @ portable -> unit @@ portable
  (** Like {!register_exception}, but is safe to use in the presence of multiple domains.

      The provided exception must be [portable] as registered exceptions may be looked up
      from any domain. *)
end
