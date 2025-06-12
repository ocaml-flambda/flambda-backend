(* Parameters: (none) *)

(* If this compiles, then we correctly handle the fact that [Util[Q:Q_impl]] is
   secretly just [Util]. In particular, we understand that projecting [t] out of
   either one gets us the same type. *)

let fancy : Util.t = Export_fancy_q_impl.fancy
