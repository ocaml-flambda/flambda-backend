(* Parameters: P *)

module No_direct_access_to_util = struct
  module Util = No_such_module
end

module Util = Util
