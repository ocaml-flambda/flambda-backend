type ('a, 'witness) t : immutable_data with 'witness = { compare : 'a -> 'a -> int }
[@@unsafe_allow_any_mode_crossing]
