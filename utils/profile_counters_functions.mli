type typing_output_for_counters =
  | Typedtree_implementation_output of Typedtree.implementation
  | Typedtree_signature_output of Typedtree.signature

val count_language_extensions : typing_output_for_counters -> Profile.Counters.t
