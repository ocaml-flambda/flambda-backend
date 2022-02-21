module type S = Asm_directives_intf.S

module type Arg = Asm_directives_intf.Arg

module Make (_ : Arg) : S
