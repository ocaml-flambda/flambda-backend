(* Parameters: P *)

module Fancy_q_impl = Fancy(Q)(Q_impl) [@jane.non_erasable.instances]

module No_direct_access_to_export_fancy_q_impl = struct
  module Export_fancy_q_impl = No_such_module
end

module Export_fancy_q_impl = Export_fancy_q_impl
