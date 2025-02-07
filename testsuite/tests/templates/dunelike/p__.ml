(* Parameters: (none) *)

module No_direct_access_to_p = struct
  module P = No_such_module
end

module P = P
