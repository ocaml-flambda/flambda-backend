[@@@ocaml.warning "+a-30-40-41-42"]

open! Regalloc_utils

type t
(* Holds a Cfg_with_layout.t value, and a "cache" of the liveness information
   that can be invalidated. *)

val make : Cfg_with_layout.t -> t

val cfg_with_layout : t -> Cfg_with_layout.t

val cfg : t -> Cfg.t

val liveness : t -> liveness

val liveness_find : t -> int -> Cfg_liveness.Liveness.domain

val liveness_find_opt : t -> int -> Cfg_liveness.Liveness.domain option

val invalidate_liveness : t -> unit
