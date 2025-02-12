(* Parameters: P, Q *)

(* This is in the [fancy] library, so it's compiled with
[-open Fancy__ -open No_direct_access_to_fancy] *)

type t

val create : string -> t
val to_string : t -> string
