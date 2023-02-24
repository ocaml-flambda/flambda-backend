open Import

exception Failure

val dump_error : Parse_flambda.error -> unit

val parse_flambda : Misc.filepath -> Flambda_unit.t
