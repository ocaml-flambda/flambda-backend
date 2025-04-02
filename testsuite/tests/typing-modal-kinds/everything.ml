(* TEST
   expect;
*)

(* The kinds [value] and [float64] don't mode cross, but the kinds [value mod
   everything] and [float64 mod everything] cross everything (ignoring
   nullability) *)
type t_value : value
type t_value_mod_e : value mod everything
type t_float64 : float64
type t_float64_mod_e : float64 mod everything
[%%expect{|
type t_value
type t_value_mod_e : immediate
type t_float64 : float64
type t_float64_mod_e : float64 mod everything
|}]

(*** locality ***)
type bad : value mod global = t_value
[%%expect{|
Line 1, characters 0-37:
1 | type bad : value mod global = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of value mod global
         because of the definition of bad at line 1, characters 0-37.
|}]

type good : value mod global = t_value_mod_e
[%%expect{|
type good = t_value_mod_e
|}]

type bad : float64 mod global = t_float64
[%%expect{|
Line 1, characters 0-41:
1 | type bad : float64 mod global = t_float64
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_float64" is float64
         because of the definition of t_float64 at line 3, characters 0-24.
       But the kind of type "t_float64" must be a subkind of float64 mod global
         because of the definition of bad at line 1, characters 0-41.
|}]

type good : float64 mod global = t_float64_mod_e
[%%expect{|
type good = t_float64_mod_e
|}]

(*** linearity ***)
type bad : value mod many = t_value
[%%expect{|
Line 1, characters 0-35:
1 | type bad : value mod many = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of value mod many
         because of the definition of bad at line 1, characters 0-35.
|}]

type good : value mod many = t_value_mod_e
[%%expect{|
type good = t_value_mod_e
|}]

type bad : float64 mod many = t_float64
[%%expect{|
Line 1, characters 0-39:
1 | type bad : float64 mod many = t_float64
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_float64" is float64
         because of the definition of t_float64 at line 3, characters 0-24.
       But the kind of type "t_float64" must be a subkind of float64 mod many
         because of the definition of bad at line 1, characters 0-39.
|}]

type good : float64 mod many = t_float64_mod_e
[%%expect{|
type good = t_float64_mod_e
|}]

(*** uniqueness ***)
type bad : value mod aliased = t_value
[%%expect{|
Line 1, characters 0-38:
1 | type bad : value mod aliased = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of value mod aliased
         because of the definition of bad at line 1, characters 0-38.
|}]

type good : value mod aliased = t_value_mod_e
[%%expect{|
type good = t_value_mod_e
|}]

type bad : float64 mod aliased = t_float64
[%%expect{|
Line 1, characters 0-42:
1 | type bad : float64 mod aliased = t_float64
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_float64" is float64
         because of the definition of t_float64 at line 3, characters 0-24.
       But the kind of type "t_float64" must be a subkind of
         float64 mod aliased
         because of the definition of bad at line 1, characters 0-42.
|}]

type good : float64 mod aliased = t_float64_mod_e
[%%expect{|
type good = t_float64_mod_e
|}]

(*** portability ***)
type bad : value mod portable = t_value
[%%expect{|
Line 1, characters 0-39:
1 | type bad : value mod portable = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of value mod portable
         because of the definition of bad at line 1, characters 0-39.
|}]

type good : value mod portable = t_value_mod_e
[%%expect{|
type good = t_value_mod_e
|}]

type bad : float64 mod portable = t_float64
[%%expect{|
Line 1, characters 0-43:
1 | type bad : float64 mod portable = t_float64
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_float64" is float64
         because of the definition of t_float64 at line 3, characters 0-24.
       But the kind of type "t_float64" must be a subkind of
         float64 mod portable
         because of the definition of bad at line 1, characters 0-43.
|}]

type good : float64 mod portable = t_float64_mod_e
[%%expect{|
type good = t_float64_mod_e
|}]

(*** contention ***)
type bad : value mod contended = t_value
[%%expect{|
Line 1, characters 0-40:
1 | type bad : value mod contended = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of value mod contended
         because of the definition of bad at line 1, characters 0-40.
|}]

type good : value mod contended = t_value_mod_e
[%%expect{|
type good = t_value_mod_e
|}]

type bad : float64 mod contended = t_float64
[%%expect{|
Line 1, characters 0-44:
1 | type bad : float64 mod contended = t_float64
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_float64" is float64
         because of the definition of t_float64 at line 3, characters 0-24.
       But the kind of type "t_float64" must be a subkind of
         float64 mod contended
         because of the definition of bad at line 1, characters 0-44.
|}]

type good : float64 mod contended = t_float64_mod_e
[%%expect{|
type good = t_float64_mod_e
|}]

(*** yielding ***)
type bad : value mod unyielding = t_value
[%%expect{|
Line 1, characters 0-41:
1 | type bad : value mod unyielding = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of value mod unyielding
         because of the definition of bad at line 1, characters 0-41.
|}]

type good : value mod unyielding = t_value_mod_e
[%%expect{|
type good = t_value_mod_e
|}]

type bad : float64 mod unyielding = t_float64
[%%expect{|
Line 1, characters 0-45:
1 | type bad : float64 mod unyielding = t_float64
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_float64" is float64
         because of the definition of t_float64 at line 3, characters 0-24.
       But the kind of type "t_float64" must be a subkind of
         float64 mod unyielding
         because of the definition of bad at line 1, characters 0-45.
|}]

type good : float64 mod unyielding = t_float64_mod_e
[%%expect{|
type good = t_float64_mod_e
|}]

(*** external ***)
type bad : value mod external_ = t_value
[%%expect{|
Line 1, characters 0-40:
1 | type bad : value mod external_ = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of value mod external_
         because of the definition of bad at line 1, characters 0-40.
|}]

type good : value mod external_ = t_value_mod_e
[%%expect{|
type good = t_value_mod_e
|}]

type bad : float64 mod external_ = t_float64
[%%expect{|
Line 1, characters 0-44:
1 | type bad : float64 mod external_ = t_float64
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_float64" is float64
         because of the definition of t_float64 at line 3, characters 0-24.
       But the kind of type "t_float64" must be a subkind of
         float64 mod external_
         because of the definition of bad at line 1, characters 0-44.
|}]

type good : float64 mod external_ = t_float64_mod_e
[%%expect{|
type good = t_float64_mod_e
|}]

(* mod everything does not affect nullability *)
type t : value_or_null mod everything
type bad : value = t

[%%expect{|
type t : immediate_or_null
Line 2, characters 0-20:
2 | type bad : value = t
    ^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immediate_or_null
         because of the definition of t at line 1, characters 0-37.
       But the kind of type "t" must be a subkind of value
         because of the definition of bad at line 2, characters 0-20.
|}]

(* everything can not be written as a mode or modality *)
type t = int @ everything -> int

[%%expect{|
Line 1, characters 15-25:
1 | type t = int @ everything -> int
                   ^^^^^^^^^^
Error: Unrecognized mode everything.
|}]

type t = { x : int @@ everything }

[%%expect{|
Line 1, characters 22-32:
1 | type t = { x : int @@ everything }
                          ^^^^^^^^^^
Error: Unrecognized modality everything.
|}]

(* Printing *)
type t : immediate & immediate
[%%expect{|
type t : immediate & immediate
|}]

type t : value & float64 mod everything
[%%expect{|
type t : immediate & float64 mod everything
|}]

type t : value & (immediate & bits64) & float32 mod everything
[%%expect{|
type t
  : immediate & (immediate & bits64 mod everything) & float32 mod everything
|}]
