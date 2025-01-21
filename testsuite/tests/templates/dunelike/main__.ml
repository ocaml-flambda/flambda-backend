module Fancy_int = Fancy(P)(P_int) [@jane.non_erasable.instances]
module Fancy_string = Fancy(P)(P_string) [@jane.non_erasable.instances]

module No_direct_access_to_main = struct
  module Main = No_such_module
end

module Main = Main
