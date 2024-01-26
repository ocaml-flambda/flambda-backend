(* TEST
 * arch64
 ** expect *)

(* Mode crossing works on immediate64 types *)
module F (M : sig type t [@@immediate64] end) = struct
  let f : local_ M.t -> _ = fun t -> t
end

[%%expect{|
module F :
  functor (M : sig type t : immediate64 end) ->
    sig val f : M.t@local -> M.t end
|}]
