module No_direct_access_to_p_int = struct
  module P_int = No_such_module
end

module P_int = P_int
