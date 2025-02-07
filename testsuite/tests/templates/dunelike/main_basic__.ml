(* Parameters: (none) *)

module Basic_int = Basic(P)(P_int) [@jane.non_erasable.instances]
module Basic_string = Basic(P)(P_string) [@jane.non_erasable.instances]

module No_direct_access_to_main_basic = struct
  module Main_basic = No_such_module
end

module Main_basic = Main_basic
