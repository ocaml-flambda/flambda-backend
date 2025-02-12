(* Parameters: P, Q *)

module No_direct_access_to_fancy = struct
  module Fancy = No_such_module
  module Fancy__Flourish = No_such_module
  module Fancy__Ornament = No_such_module
end

module Fancy = Fancy
module Flourish = Fancy__Flourish
module Ornament = Fancy__Ornament

(* Not parameterised - this checks that we're not too strict *)

module PI = P_int
