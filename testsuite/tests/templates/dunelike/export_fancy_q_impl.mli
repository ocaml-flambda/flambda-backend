(* Parameters: P *)

(* Access the alias [Util] as exported from [Fancy[Q:Q_impl]]. What's
   interesting here is that [Fancy] has to assume that [Util] takes both [P] and
   [Q], but it actually only takes [P]. So [Fancy_q_impl.Util] evaluates (after
   elaboration) to [Util[Q:Q_impl]{P:P}] and we have to tolerate a excess
   _visible_ argument. *)

val fancy : Fancy_q_impl.Util.t
