module type S = sig
  module Asm_directives : Asm_directives.S

  val get_file_num : string -> int
end
