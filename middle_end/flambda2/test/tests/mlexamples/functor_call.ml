(* This should inline [Functor_with_rec.F], and we should check that this
   indeeds create a specialised version of [foo]. Bonus points if [test] is also
   inlined in this version of [foo]. *)

module X = struct
  let[@inline always] test x = x > 0
end

include Functor_with_rec.F (X)
