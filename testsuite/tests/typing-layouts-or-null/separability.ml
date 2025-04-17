(* TEST
 expect;
*)

(* Tests for the separability jkind axis. *)

(* Basic subkinding relation. *)

type t_nonsep : any mod non_separable
type t_sep : any mod separable
type t_nonfloat : any mod non_float

[%%expect{|
type t_nonsep : any
type t_sep : any mod separable
type t_nonfloat : any mod non_float
|}]

type ('a :  any mod non_separable) accepts_nonsep
type ('a : any mod separable) accepts_sep
type ('a : any mod non_float) accepts_nonfloat

[%%expect{|
type ('a : any) accepts_nonsep
type ('a : any mod separable) accepts_sep
type ('a : any mod non_float) accepts_nonfloat
|}]

type succeeds = t_nonsep accepts_nonsep
type succeeds = t_sep accepts_nonsep
type succeeds = t_nonfloat accepts_nonsep

[%%expect{|
type succeeds = t_nonsep accepts_nonsep
type succeeds = t_sep accepts_nonsep
type succeeds = t_nonfloat accepts_nonsep
|}]

type fails = t_nonsep accepts_sep
[%%expect{|
Line 1, characters 13-21:
1 | type fails = t_nonsep accepts_sep
                 ^^^^^^^^
Error: This type "t_nonsep" should be an instance of type
         "('a : any mod separable)"
       The kind of t_nonsep is any
         because of the definition of t_nonsep at line 1, characters 0-37.
       But the kind of t_nonsep must be a subkind of any mod separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type succeeds = t_sep accepts_sep
type succeeds = t_nonfloat accepts_sep


[%%expect{|
type succeeds = t_sep accepts_sep
type succeeds = t_nonfloat accepts_sep
|}]

type fails = t_nonsep accepts_nonfloat
[%%expect{|
Line 1, characters 13-21:
1 | type fails = t_nonsep accepts_nonfloat
                 ^^^^^^^^
Error: This type "t_nonsep" should be an instance of type
         "('a : any mod non_float)"
       The kind of t_nonsep is any
         because of the definition of t_nonsep at line 1, characters 0-37.
       But the kind of t_nonsep must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type fails = t_sep accepts_nonfloat
[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_sep accepts_nonfloat
                 ^^^^^
Error: This type "t_sep" should be an instance of type "('a : any mod non_float)"
       The kind of t_sep is any mod separable
         because of the definition of t_sep at line 2, characters 0-30.
       But the kind of t_sep must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type succeeds = t_nonfloat accepts_nonfloat

[%%expect{|
type succeeds = t_nonfloat accepts_nonfloat
|}]

(* Testing separability for various base jkinds. *)

(* [value_or_null] is non-separable: *)
type t_von : value_or_null

type succeeds = t_von accepts_nonsep
[%%expect{|
type t_von : value_or_null
type succeeds = t_von accepts_nonsep
|}]

type fails = t_von accepts_sep

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_von accepts_sep
                 ^^^^^
Error: This type "t_von" should be an instance of type "('a : any mod separable)"
       The kind of t_von is value_or_null
         because of the definition of t_von at line 1, characters 0-26.
       But the kind of t_von must be a subkind of any mod separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type fails = t_von accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_von accepts_nonfloat
                 ^^^^^
Error: This type "t_von" should be an instance of type "('a : any mod non_float)"
       The kind of t_von is value_or_null
         because of the definition of t_von at line 1, characters 0-26.
       But the kind of t_von must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* [value] is separable *)
type t_val : value

type succeeds = t_val accepts_nonsep
type succeeds = t_val accepts_sep

[%%expect{|
type t_val
type succeeds = t_val accepts_nonsep
type succeeds = t_val accepts_sep
|}]

type fails = t_val accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_val accepts_nonfloat
                 ^^^^^
Error: This type "t_val" should be an instance of type "('a : any mod non_float)"
       The kind of t_val is value
         because of the definition of t_val at line 1, characters 0-18.
       But the kind of t_val must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* [any] is non-separable *)
type t_any : any

type succeeds = t_any accepts_nonsep

[%%expect{|
type t_any : any
type succeeds = t_any accepts_nonsep
|}]

type fails = t_any accepts_sep

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_any accepts_sep
                 ^^^^^
Error: This type "t_any" should be an instance of type "('a : any mod separable)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any mod separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type fails = t_any accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_any accepts_nonfloat
                 ^^^^^
Error: This type "t_any" should be an instance of type "('a : any mod non_float)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* [any_non_null] is separable *)
type t_ann : any_non_null
type succeeds = t_ann accepts_nonsep
type succeeds = t_ann accepts_sep
[%%expect{|
type t_ann : any_non_null
type succeeds = t_ann accepts_nonsep
type succeeds = t_ann accepts_sep
|}]

type fails = t_ann accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_ann accepts_nonfloat
                 ^^^^^
Error: This type "t_ann" should be an instance of type "('a : any mod non_float)"
       The kind of t_ann is any_non_null
         because of the definition of t_ann at line 1, characters 0-25.
       But the kind of t_ann must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* Testing additional non-float jkinds *)

(* [non_float_value] is non-float *)
type t_nfv : non_float_value
type succeeds = t_nfv accepts_nonsep
type succeeds = t_nfv accepts_sep
type succeeds = t_nfv accepts_nonfloat

[%%expect{|
type t_nfv : non_float_value
type succeeds = t_nfv accepts_nonsep
type succeeds = t_nfv accepts_sep
type succeeds = t_nfv accepts_nonfloat
|}]

(* [immutable_data] is non-float *)
type t_imm : immutable_data
type succeeds = t_imm accepts_nonsep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat

[%%expect{|
type t_imm : immutable_data
type succeeds = t_imm accepts_nonsep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat
|}]

(* [mutable_data] is non-float *)
type t_mut : mutable_data
type succeeds = t_mut accepts_nonsep
type succeeds = t_mut accepts_sep
type succeeds = t_mut accepts_nonfloat

[%%expect{|
type t_mut : mutable_data
type succeeds = t_mut accepts_nonsep
type succeeds = t_mut accepts_sep
type succeeds = t_mut accepts_nonfloat
|}]

(* [sync_data] is non-float *)
type t_sync : sync_data
type succeeds = t_sync accepts_nonsep
type succeeds = t_sync accepts_sep
type succeeds = t_sync accepts_nonfloat

[%%expect{|
type t_sync : sync_data
type succeeds = t_sync accepts_nonsep
type succeeds = t_sync accepts_sep
type succeeds = t_sync accepts_nonfloat
|}]

(* [immediate] is non-float *)
type t_imm : immediate
type succeeds = t_imm accepts_nonsep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat

[%%expect{|
type t_imm : immediate
type succeeds = t_imm accepts_nonsep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat
|}]

(* [immediate64] is non-float *)
type t_imm64 : immediate64
type succeeds = t_imm64 accepts_nonsep
type succeeds = t_imm64 accepts_sep
type succeeds = t_imm64 accepts_nonfloat

[%%expect{|
type t_imm64 : immediate64
type succeeds = t_imm64 accepts_nonsep
type succeeds = t_imm64 accepts_sep
type succeeds = t_imm64 accepts_nonfloat
|}]

(* Testing non-value layouts. *)

(* [bits32] is non-float *)
type t_b32 : bits32
type succeeds = t_b32 accepts_nonsep
type succeeds = t_b32 accepts_sep
type succeeds = t_b32 accepts_nonfloat

[%%expect{|
type t_b32 : bits32
type succeeds = t_b32 accepts_nonsep
type succeeds = t_b32 accepts_sep
type succeeds = t_b32 accepts_nonfloat
|}]

(* [bits64] is non-float *)
type t_b64 : bits64
type succeeds = t_b64 accepts_nonsep
type succeeds = t_b64 accepts_sep
type succeeds = t_b64 accepts_nonfloat

[%%expect{|
type t_b64 : bits64
type succeeds = t_b64 accepts_nonsep
type succeeds = t_b64 accepts_sep
type succeeds = t_b64 accepts_nonfloat
|}]

   (* [word] is non-float *)
type t_word : word
type succeeds = t_word accepts_nonsep
type succeeds = t_word accepts_sep
type succeeds = t_word accepts_nonfloat

[%%expect{|
type t_word : word
type succeeds = t_word accepts_nonsep
type succeeds = t_word accepts_sep
type succeeds = t_word accepts_nonfloat
|}]

(* [vec128] is non-float *)
type t_vec : vec128
type succeeds = t_vec accepts_nonsep
type succeeds = t_vec accepts_sep
type succeeds = t_vec accepts_nonfloat

[%%expect{|
type t_vec : vec128
type succeeds = t_vec accepts_nonsep
type succeeds = t_vec accepts_sep
type succeeds = t_vec accepts_nonfloat
|}]

(* Testing non-value float layouts. *)
(* "non_float" in the separability axis refers to "legacy" OCaml float heap blocks,
    so float32/float64 layouts are still non_float. *)

(* [float32] is non-float *)
type t_f32 : float32
type succeeds = t_f32 accepts_nonsep
type succeeds = t_f32 accepts_sep
type succeeds = t_f32 accepts_nonfloat

[%%expect{|
type t_f32 : float32
type succeeds = t_f32 accepts_nonsep
type succeeds = t_f32 accepts_sep
type succeeds = t_f32 accepts_nonfloat
|}]

(* [float64] is non-float *)
type t_f64 : float64
type succeeds = t_f64 accepts_nonsep
type succeeds = t_f64 accepts_sep
type succeeds = t_f64 accepts_nonfloat

[%%expect{|
type t_f64 : float64
type succeeds = t_f64 accepts_nonsep
type succeeds = t_f64 accepts_sep
type succeeds = t_f64 accepts_nonfloat
|}]

(* Test basic types. *)

type succeeds = int accepts_nonfloat
type succeeds = string accepts_nonfloat
type succeeds = unit option accepts_nonfloat

[%%expect{|
type succeeds = int accepts_nonfloat
type succeeds = string accepts_nonfloat
type succeeds = unit option accepts_nonfloat
|}]

(* Floats are separable: *)

type fails = float accepts_nonfloat
[%%expect{|
Line 1, characters 13-18:
1 | type fails = float accepts_nonfloat
                 ^^^^^
Error: This type "float" should be an instance of type "('a : any mod non_float)"
       The kind of float is value mod many unyielding stateless immutable
         because it is the primitive type float.
       But the kind of float must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type succeeds = float accepts_sep
[%%expect{|
type succeeds = float accepts_sep
|}]

(* Separability is a shallow property: lists or arrays of floats are non-float: *)

type succeeds = float list accepts_nonfloat
type succeeds = float array accepts_nonfloat

[%%expect{|
type succeeds = float list accepts_nonfloat
type succeeds = float array accepts_nonfloat
|}]

(* Records and variants are all non-float: *)

type t1 = { f1 : string; f2: int }
type t2 = { f1 : float; f2 : float }
type t3 = { f1 : float }
type t4 = { f1 : string; f2 : string; f3 : int }
type t5 = { f1 : string; f2 : float; f3: int64#; f4: int32# }
[%%expect{|
type t1 = { f1 : string; f2 : int; }
type t2 = { f1 : float; f2 : float; }
type t3 = { f1 : float; }
type t4 = { f1 : string; f2 : string; f3 : int; }
type t5 = { f1 : string; f2 : float; f3 : int64#; f4 : int32#; }
|}]

type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat
type succeeds = t4 accepts_nonfloat
type succeeds = t5 accepts_nonfloat

[%%expect{|
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat
type succeeds = t4 accepts_nonfloat
type succeeds = t5 accepts_nonfloat
|}]

type t1 = | A | B | C
type t2 = | A | B of string | C of { f1 : float; f2 : int }
type t3 = | A of { f1: int64#; f2: float# } | B of int32#

[%%expect{|
type t1 = A | B | C
type t2 = A | B of string | C of { f1 : float; f2 : int; }
type t3 = A of { f1 : int64#; f2 : float#; } | B of int32#
|}]

type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat

[%%expect{|
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat
|}]

type t1 = [ `A | `B | `C ]
type t2 = [ `A of string | `B ]
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat

[%%expect{|
type t1 = [ `A | `B | `C ]
type t2 = [ `A of string | `B ]
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
|}]

(* [or_null] and separability. *)

(* ['a or_null] is not separable: *)

type 'a fails = 'a or_null accepts_nonfloat

[%%expect{|
Line 1, characters 16-26:
1 | type 'a fails = 'a or_null accepts_nonfloat
                    ^^^^^^^^^^
Error: This type "'a or_null" should be an instance of type
         "('b : any mod non_float)"
       The kind of 'a or_null is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a or_null must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type 'a fails = 'a or_null accepts_sep

[%%expect{|
Line 1, characters 16-26:
1 | type 'a fails = 'a or_null accepts_sep
                    ^^^^^^^^^^
Error: This type "'a or_null" should be an instance of type
         "('b : any mod separable)"
       The kind of 'a or_null is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a or_null must be a subkind of any mod separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type 'a succeds = 'a or_null accepts_nonsep

[%%expect{|
type 'a succeds = 'a or_null accepts_nonsep
|}]

(* CR layouts v3: [or_null] should be able accept non-separable values? *)

type t_nonsep_val : value_or_null mod non_null
type fails = t_nonsep_val or_null

[%%expect{|
type t_nonsep_val : value_or_null mod non_null
Line 2, characters 13-25:
2 | type fails = t_nonsep_val or_null
                 ^^^^^^^^^^^^
Error: This type "t_nonsep_val" should be an instance of type "('a : value)"
       The kind of t_nonsep_val is value_or_null mod non_null
         because of the definition of t_nonsep_val at line 1, characters 0-46.
       But the kind of t_nonsep_val must be a subkind of value
         because the type argument of or_null has kind value.
|}]

(* CR layouts: ['a or_null] where 'a is non-float should be non-float. *)

type ('a : non_float_value) should_succeed = 'a or_null accepts_nonfloat

[%%expect{|
Line 1, characters 45-55:
1 | type ('a : non_float_value) should_succeed = 'a or_null accepts_nonfloat
                                                 ^^^^^^^^^^
Error: This type "'a or_null" should be an instance of type
         "('b : any mod non_float)"
       The kind of 'a or_null is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a or_null must be a subkind of any mod non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type ('a : non_float_value) should_succeed = 'a or_null accepts_sep
[%%expect{|
Line 1, characters 45-55:
1 | type ('a : non_float_value) should_succeed = 'a or_null accepts_sep
                                                 ^^^^^^^^^^
Error: This type "'a or_null" should be an instance of type
         "('b : any mod separable)"
       The kind of 'a or_null is immediate_or_null with 'a
         because it is the primitive immediate_or_null type or_null.
       But the kind of 'a or_null must be a subkind of any mod separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type ('a : non_float_value) succeeds = 'a or_null accepts_nonsep

[%%expect{|
type ('a : non_float_value) succeeds = 'a or_null accepts_nonsep
|}]

(* CR with-kinds: fix error reporting difference. *)
(* [float or_null] is not separable: *)

type fails = float or_null accepts_sep

[%%expect{|
Line 1, characters 13-26:
1 | type fails = float or_null accepts_sep
                 ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of float or_null is
         value_or_null mod many unyielding stateless immutable
         because it is the primitive immediate_or_null type or_null.
       But the kind of float or_null must be a subkind of any mod separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}, Principal{|
Line 1, characters 13-26:
1 | type fails = float or_null accepts_sep
                 ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any mod separable)"
       The kind of float or_null is immediate_or_null with float
         because it is the primitive immediate_or_null type or_null.
       But the kind of float or_null must be a subkind of any mod separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

(* Separability and arrays: *)

(* Arrays accept separable values: *)

type fails = t_nonsep_val array

[%%expect{|
Line 1, characters 13-25:
1 | type fails = t_nonsep_val array
                 ^^^^^^^^^^^^
Error: This type "t_nonsep_val" should be an instance of type
         "('a : any_non_null)"
       The kind of t_nonsep_val is value_or_null mod non_null
         because of the definition of t_nonsep_val at line 1, characters 0-46.
       But the kind of t_nonsep_val must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type ('a : value mod separable) succeeds = 'a array

[%%expect{|
type 'a succeeds = 'a array
|}]

(* CR layouts v3: Arrays should accept [value_or_null mod separable] elements. *)

type ('a : value_or_null mod separable) should_succeed = 'a array

[%%expect{|
type 'a should_succeed = 'a array
|}]

(* Arrays should not accept [float or_null]s: *)

type fails = float or_null array

[%%expect{|
Line 1, characters 13-26:
1 | type fails = float or_null array
                 ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of float or_null is
         value_or_null mod many unyielding stateless immutable
         because it is the primitive immediate_or_null type or_null.
       But the kind of float or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}, Principal{|
Line 1, characters 13-26:
1 | type fails = float or_null array
                 ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of float or_null is immediate_or_null with float
         because it is the primitive immediate_or_null type or_null.
       But the kind of float or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

(* CR layouts v3: arrays should accepts non-float [or_null] values. *)

type should_succeed = string or_null array

[%%expect{|
Line 1, characters 22-36:
1 | type should_succeed = string or_null array
                          ^^^^^^^^^^^^^^
Error: This type "string or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of string or_null is
         value_or_null mod many unyielding stateless immutable
         because it is the primitive immediate_or_null type or_null.
       But the kind of string or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}, Principal{|
Line 1, characters 22-36:
1 | type should_succeed = string or_null array
                          ^^^^^^^^^^^^^^
Error: This type "string or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of string or_null is immediate_or_null with string
         because it is the primitive immediate_or_null type or_null.
       But the kind of string or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type should_succeed = int or_null array

[%%expect{|
Line 1, characters 22-33:
1 | type should_succeed = int or_null array
                          ^^^^^^^^^^^
Error: This type "int or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of int or_null is immediate_or_null
         because it is the primitive immediate_or_null type or_null.
       But the kind of int or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}, Principal{|
Line 1, characters 22-33:
1 | type should_succeed = int or_null array
                          ^^^^^^^^^^^
Error: This type "int or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of int or_null is immediate_or_null with int
         because it is the primitive immediate_or_null type or_null.
       But the kind of int or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]
