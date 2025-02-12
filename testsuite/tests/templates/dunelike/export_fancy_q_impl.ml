(* Parameters: P *)

module Fancy_q_impl = Fancy(Q)(Q_impl) [@jane.non_erasable.instances]

let fancy = Util.create ()
