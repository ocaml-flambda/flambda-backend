val conv :
  symbol_for_global:(?comp_unit:Compilation_unit.t -> Ident.t -> Symbol.t) ->
  module_ident:Ident.t ->
  Fexpr.flambda_unit ->
  Flambda_unit.t
