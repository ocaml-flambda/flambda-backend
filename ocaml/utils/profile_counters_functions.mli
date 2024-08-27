type typing_input =
  | Typedtree_implementation_input of Typedtree.implementation
  | Typedtree_signature_input of Typedtree.signature

val count_language_extensions : typing_input -> Profile.Counters.t
