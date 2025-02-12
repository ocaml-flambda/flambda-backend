(* Parameters: (none) *)

module No_direct_access_to_p_string = struct
  module P_string = No_such_module
end

module P_string = P_string
