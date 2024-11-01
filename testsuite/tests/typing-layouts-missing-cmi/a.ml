(* CR layouts v2.5: The commented out code in this file uses void, but could
   use any non-value layout. *)
type a_imm = A

module Value : sig
  type t
end = struct
  type t = unit
end
type a_value = Value.t
(* type a_void : void *)
