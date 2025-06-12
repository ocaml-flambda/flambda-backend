module No_direct_access_to_q = struct
  module Q = No_such_module
end

module Q = Q
