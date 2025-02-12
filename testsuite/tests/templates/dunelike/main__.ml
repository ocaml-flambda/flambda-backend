(* Parameters: (none) *)

module Fancy_int = Fancy(P)(P_int)(Q)(Q_impl) [@jane.non_erasable.instances]
module Fancy_string = Fancy(P)(P_string)(Q)(Q_impl) [@jane.non_erasable.instances]
module Use_fancy_q_impl_p_int = Use_fancy_q_impl(P)(P_int) [@jane.non_erasable.instances]
module Util_p_int = Util(P)(P_int) [@jane.non_erasable.instances]

module No_direct_access_to_main = struct
  module Main = No_such_module
end

module Main = Main
