module Util_int = Util(P)(P_int) [@jane.non_erasable.instances]
module Just_util = Util

type t1 = Util_int.t
type t2 = Just_util.t
