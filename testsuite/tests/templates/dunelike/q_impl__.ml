(* Parameters: (none) *)

module No_direct_access_to_q_impl = struct
  module Q_impl = No_such_module
end

module Q_impl = Q_impl
