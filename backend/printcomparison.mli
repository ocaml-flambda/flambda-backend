val integer_comparison : Comparison.Integer.t -> string 
val float_comparison : Comparison.Float.t -> string 

val intcomp : Comparison.Integer.With_signedness.t -> string
val floatcomp : Comparison.Float.t -> string

val test
    : (Format.formatter -> 'a -> unit)
    -> Comparison.Test.t
    -> Format.formatter
    -> 'a array
    -> unit
