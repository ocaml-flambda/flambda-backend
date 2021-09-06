module type S = sig
  module Asm_directives : Asm_directives.S

  val make_symbol : string -> string

  val get_file_num : string -> int
end