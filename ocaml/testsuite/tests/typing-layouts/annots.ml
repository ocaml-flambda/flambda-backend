(* TEST
 include stdlib_upstream_compatible;
 {
   expect;
 }{
   flags = "-extension layouts_beta";
   expect;
 }
*)

type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_any : any;;

[%%expect{|
type t_value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_any : any
|}]

type t_void : void;;

[%%expect{|
Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

type t_any_non_null : any_non_null;;

[%%expect{|
type t_any_non_null : any_non_null
|}]

type t_value_or_null : value_or_null;;

[%%expect{|
Line 1, characters 23-36:
1 | type t_value_or_null : value_or_null;;
                           ^^^^^^^^^^^^^
Error: Layout value_or_null is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

type t_value_mod_global : value mod global
type t_value_mod_local : value mod local
type t_value_mod_many : value mod many
type t_value_mod_once : value mod once
type t_value_mod_unique : value mod unique
type t_value_mod_shared : value mod shared
type t_value_mod_internal : value mod internal
type t_value_mod_contended : value mod contended
type t_value_mod_uncontended : value mod uncontended
type t_value_mod_portable : value mod portable
type t_value_mod_nonportable : value mod nonportable
type t_value_mod_external : value mod external_
type t_value_mod_external64 : value mod external64

[%%expect{|
type t_value_mod_global : value mod global
type t_value_mod_local
type t_value_mod_many : value mod many
type t_value_mod_once
type t_value_mod_unique : value mod unique
type t_value_mod_shared
type t_value_mod_internal
type t_value_mod_contended
type t_value_mod_uncontended : value mod uncontended
type t_value_mod_portable : value mod portable
type t_value_mod_nonportable
type t_value_mod_external : value mod external_
type t_value_mod_external64 : value mod external64
|}]

type t1 : float32 mod internal shared many local
type t2 : bits64 mod once external64 unique
type t3 : immediate mod local unique

[%%expect {|
type t1 : float32 mod internal shared many local
type t2 : bits64 mod once external64 unique
type t3 : immediate mod local unique
|}]

type t4 : value mod local local
[%%expect {|
Line 1, characters 26-31:
1 | type t4 : value mod local local
                              ^^^^^
Error: The locality axis has already been specified.
|}]

type t5 : float64 mod global global
[%%expect {|
Line 1, characters 29-35:
1 | type t5 : float64 mod global global
                                 ^^^^^^
Error: The locality axis has already been specified.
|}]

type t6 : bits32 mod local global
[%%expect {|
Line 1, characters 27-33:
1 | type t6 : bits32 mod local global
                               ^^^^^^
Error: The locality axis has already been specified.
|}]

type t7 : bits64 mod global local
[%%expect {|
Line 1, characters 28-33:
1 | type t7 : bits64 mod global local
                                ^^^^^
Error: The locality axis has already been specified.
|}]

type t8 : value mod local many unique uncontended nonportable external64 local
[%%expect {|
Line 1, characters 73-78:
1 | type t8 : value mod local many unique uncontended nonportable external64 local
                                                                             ^^^^^
Error: The locality axis has already been specified.
|}]

type t9 : value mod once many
[%%expect {|
Line 1, characters 25-29:
1 | type t9 : value mod once many
                             ^^^^
Error: The linearity axis has already been specified.
|}]

type t10 : value mod contended uncontended
[%%expect {|
Line 1, characters 31-42:
1 | type t10 : value mod contended uncontended
                                   ^^^^^^^^^^^
Error: The contention axis has already been specified.
|}]

type t11 : value mod portable nonportable
[%%expect {|
Line 1, characters 30-41:
1 | type t11 : value mod portable nonportable
                                  ^^^^^^^^^^^
Error: The portability axis has already been specified.
|}]

type t12 : value mod external64 external_
[%%expect {|
Line 1, characters 32-41:
1 | type t12 : value mod external64 external_
                                    ^^^^^^^^^
Error: The externality axis has already been specified.
|}]

type t13 : value mod maybe_null non_null
[%%expect {|
Line 1, characters 32-40:
1 | type t13 : value mod maybe_null non_null
                                    ^^^^^^^^
Error: The nullability axis has already been specified.
|}]

type t14 : value mod unique shared
[%%expect {|
Line 1, characters 28-34:
1 | type t14 : value mod unique shared
                                ^^^^^^
Error: The uniqueness axis has already been specified.
|}]

type t : value mod foo
[%%expect {|
Line 1, characters 19-22:
1 | type t : value mod foo
                       ^^^
Error: Unrecognized modifier foo.
|}]

type t : value mod global shared bar
[%%expect {|
Line 1, characters 33-36:
1 | type t : value mod global shared bar
                                     ^^^
Error: Unrecognized modifier bar.
|}]

type t : value mod foobar unique many
[%%expect {|
Line 1, characters 19-25:
1 | type t : value mod foobar unique many
                       ^^^^^^
Error: Unrecognized modifier foobar.
|}]

type t : value mod non_null external_ fizzbuzz global
[%%expect {|
Line 1, characters 38-46:
1 | type t : value mod non_null external_ fizzbuzz global
                                          ^^^^^^^^
Error: Unrecognized modifier fizzbuzz.
|}]

(***************************************)
(* Test 1: annotation on type variable *)

let x : int as ('a: value) = 5
let x : int as ('a : immediate) = 5
let x : int as ('a : any) = 5;;
let x : int as ('a: value mod global shared many uncontended portable external_) = 5

[%%expect{|
val x : int = 5
val x : int = 5
val x : int = 5
val x : int = 5
|}]

let x : int as ('a : float64) = 5;;
[%%expect {|
Line 1, characters 8-29:
1 | let x : int as ('a : float64) = 5;;
            ^^^^^^^^^^^^^^^^^^^^^
Error: This alias is bound to type int but is used as an instance of type
         ('a : float64)
       The layout of int is value
         because it is the primitive immediate type int.
       But the layout of int must be a sublayout of float64
         because of the annotation on the type variable 'a.
|}]

let x : (int as ('a : immediate)) list as ('b : value) = [3;4;5]
;;
[%%expect {|
val x : int list = [3; 4; 5]
|}]

let x : int list as ('a : immediate) = [3;4;5]
;;
[%%expect {|
Line 1, characters 8-36:
1 | let x : int list as ('a : immediate) = [3;4;5]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This alias is bound to type int list
       but is used as an instance of type ('a : immediate)
       The kind of int list is value
         because it's a boxed variant type.
       But the kind of int list must be a subkind of immediate
         because of the annotation on the type variable 'a.
|}]
(* CR layouts: error message could be phrased better *)

let x : int list as ('a : value mod global) = [3;4;5]
[%%expect {|
Line 1, characters 8-43:
1 | let x : int list as ('a : value mod global) = [3;4;5]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This alias is bound to type int list
       but is used as an instance of type ('a : value mod global)
       The kind of int list is value
         because it's a boxed variant type.
       But the kind of int list must be a subkind of value mod global
         because of the annotation on the type variable 'a.
|}]

(****************************************)
(* Test 2: Annotation on type parameter *)

type ('a : value) t2
type (_ : value) t2'
type t3 = int t2
type t4 = bool t2

[%%expect {|
type 'a t2
type _ t2'
type t3 = int t2
type t4 = bool t2
|}]

type t = string t2
;;
[%%expect {|
type t = string t2
|}]

type ('a : immediate) t2_imm
type (_ : immediate) t2_imm'
type t1 = int t2_imm
type t2' = bool t2_imm'
type ('a : float64) t2_float64
type (_ : float64) t2_float64'
type t3 = float# t2_float64
type ('a : value mod global) t2_global
type (_ : value mod global) t2_global'
type ('a : word mod external_ many shared) t2_complex
type (_ : word mod external_ many shared) t2_complex'


[%%expect {|
type ('a : immediate) t2_imm
type (_ : immediate) t2_imm'
type t1 = int t2_imm
type t2' = bool t2_imm'
type ('a : float64) t2_float64
type (_ : float64) t2_float64'
type t3 = float# t2_float64
type ('a : value mod global) t2_global
type (_ : value mod global) t2_global'
type ('a : word mod many external_) t2_complex
type (_ : word mod many external_) t2_complex'
|}]

module M1 : sig
  type ('a : value) t
end = struct
  type (_ : value) t
end

module M2 : sig
  type (_ : value) t
end = struct
  type ('a : value) t
end

[%%expect {|
module M1 : sig type 'a t end
module M2 : sig type _ t end
|}]

module M1 : sig
  type ('a : immediate) t
end = struct
  type (_ : immediate) t
end

module M2 : sig
  type (_ : immediate) t
end = struct
  type ('a : immediate) t
end

[%%expect {|
module M1 : sig type ('a : immediate) t end
module M2 : sig type (_ : immediate) t end
|}]

module M1 : sig
  type ('a : value mod global) t
end = struct
  type (_ : value mod global) t
end

module M2 : sig
  type (_ : value mod global) t
end = struct
  type ('a : value mod global) t
end

[%%expect {|
module M1 : sig type ('a : value mod global) t end
module M2 : sig type (_ : value mod global) t end
|}]

module M1 : sig
  type ('a : word mod external_ many shared) t
end = struct
  type (_ : word mod external_ many shared) t
end

module M2 : sig
  type (_ : word mod external_ many shared) t
end = struct
  type ('a : word mod external_ many shared) t
end

[%%expect {|
module M1 : sig type ('a : word mod many external_) t end
module M2 : sig type (_ : word mod many external_) t end
|}]

type t = string t2_imm
;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = string t2_imm
             ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of t2_imm at line 1, characters 0-28.
|}]

type t = string t2_global
;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = string t2_global
             ^^^^^^
Error: This type string should be an instance of type ('a : value mod global)
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of value mod global
         because of the definition of t2_global at line 8, characters 0-38.
|}]

type u : word
type t = u t2_complex
;;
[%%expect {|
type u : word
Line 2, characters 9-10:
2 | type t = u t2_complex
             ^
Error: This type u should be an instance of type
         ('a : word mod many external_)
       The kind of u is word
         because of the definition of u at line 1, characters 0-13.
       But the kind of u must be a subkind of word mod many external_
         because of the definition of t2_complex at line 10, characters 0-53.
|}]

let f : 'a t2_imm -> 'a t2_imm = fun x -> x
let f : 'a t2_global -> 'a t2_global = fun x -> x
let f : 'a t2_complex -> 'a t2_complex = fun x -> x
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
val f : ('a : value mod global). 'a t2_global -> 'a t2_global = <fun>
val f : ('a : word mod many external_). 'a t2_complex -> 'a t2_complex =
  <fun>
|}]

let f : ('a : immediate) t2_imm -> ('a : value) t2_imm = fun x -> x
let f : ('a : value mod global) t2_global -> ('a : value) t2_global = fun x -> x
let f : ('a : word mod external_ many shared) t2_complex -> ('a : word) t2_complex = fun x -> x
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
val f : ('a : value mod global). 'a t2_global -> 'a t2_global = <fun>
val f : ('a : word mod many external_). 'a t2_complex -> 'a t2_complex =
  <fun>
|}]

let f : ('a : value) t2_imm -> ('a : value) t2_imm = fun x -> x
let f : ('a : value) t2_global -> ('a : value) t2_global = fun x -> x
let f : ('a : word) t2_complex -> ('a : word) t2_complex = fun x -> x
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
val f : ('a : value mod global). 'a t2_global -> 'a t2_global = <fun>
val f : ('a : word mod many external_). 'a t2_complex -> 'a t2_complex =
  <fun>
|}]

let f : ('a : immediate). 'a t2_imm -> 'a t2_imm = fun x -> x
let f : ('a : value mod global). 'a t2_global -> 'a t2_global = fun x -> x
let f : ('a : word mod external_ many shared). 'a t2_complex -> 'a t2_complex = fun x -> x
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
val f : ('a : value mod global). 'a t2_global -> 'a t2_global = <fun>
val f : ('a : word mod many external_). 'a t2_complex -> 'a t2_complex =
  <fun>
|}]

let f : ('a : value). 'a t2_imm -> 'a t2_imm = fun x -> x
;;
[%%expect {|
Line 1, characters 8-44:
1 | let f : ('a : value). 'a t2_imm -> 'a t2_imm = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind immediate
         because of the definition of t2_imm at line 1, characters 0-28.
|}]

let f : ('a : value). 'a t2_global -> 'a t2_global = fun x -> x
;;
[%%expect {|
Line 1, characters 8-50:
1 | let f : ('a : value). 'a t2_global -> 'a t2_global = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind value mod global
         because of the definition of t2_global at line 8, characters 0-38.
|}]

let f : ('a : word). 'a t2_complex -> 'a t2_complex = fun x -> x
;;
[%%expect {|
Line 1, characters 8-51:
1 | let f : ('a : word). 'a t2_complex -> 'a t2_complex = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind word.
       But it was inferred to have kind word mod many external_
         because of the definition of t2_complex at line 10, characters 0-53.
|}]

type 'a t = 'a t2_imm
type 'a t = 'a t2_global
type 'a t = 'a t2_complex
[%%expect {|
type ('a : immediate) t = 'a t2_imm
type ('a : value mod global) t = 'a t2_global
type ('a : word mod many external_) t = 'a t2_complex
|}]

type ('a : value) t = 'a t2_imm
type ('a : value) t = 'a t2_global
type ('a : word) t = 'a t2_complex
[%%expect {|
type ('a : immediate) t = 'a t2_imm
type ('a : value mod global) t = 'a t2_global
type ('a : word mod many external_) t = 'a t2_complex
|}]

type ('a : immediate) t = 'a t2_imm
type ('a : value mod global) t = 'a t2_global
type ('a : word mod external_ many shared) t = 'a t2_complex
[%%expect {|
type ('a : immediate) t = 'a t2_imm
type ('a : value mod global) t = 'a t2_global
type ('a : word mod many external_) t = 'a t2_complex
|}]

let f : (_ : value) t2_imm -> unit = fun _ -> ()
let g : (_ : immediate) t2_imm -> unit = fun _ -> ()

let f : (_ : value) t2_global -> unit = fun _ -> ()
let g : (_ : value mod global) t2_global -> unit = fun _ -> ()

let f : (_ : word) t2_complex -> unit = fun _ -> ()
let g : (_ : word mod external_ many shared) t2_complex -> unit = fun _ -> ()

[%%expect {|
val f : ('a : immediate). 'a t2_imm -> unit = <fun>
val g : ('a : immediate). 'a t2_imm -> unit = <fun>
val f : ('a : value mod global). 'a t2_global -> unit = <fun>
val g : ('a : value mod global). 'a t2_global -> unit = <fun>
val f : ('a : word mod many external_). 'a t2_complex -> unit = <fun>
val g : ('a : word mod many external_). 'a t2_complex -> unit = <fun>
|}]

let f : (_ : immediate) -> unit = fun _ -> ()
let f : (_ : value mod global) -> unit = fun _ -> ()
let f : (_ : word mod external_ many shared) -> unit = fun _ -> ()
let g : (_ : value) -> unit = fun _ -> ()

[%%expect {|
val f : ('a : immediate). 'a -> unit = <fun>
val f : ('a : value mod global). 'a -> unit = <fun>
val f : ('a : word mod many external_). 'a -> unit = <fun>
val g : 'a -> unit = <fun>
|}]

let f : (_ : immediate) -> (_ : value) = fun _ -> assert false
let g : (_ : value) -> (_ : immediate) = fun _ -> assert false

let f : (_ : value mod global) -> (_ : value) = fun _ -> assert false
let g : (_ : value) -> (_ : value mod global) = fun _ -> assert false

let f : (_ : word mod external_ many shared) -> (_ : value) = fun _ -> assert false
let g : (_ : value) -> (_ : word mod external_ many shared) = fun _ -> assert false

[%%expect {|
val f : ('a : immediate) 'b. 'a -> 'b = <fun>
val g : 'a ('b : immediate). 'a -> 'b = <fun>
val f : ('a : value mod global) 'b. 'a -> 'b = <fun>
val g : 'a ('b : value mod global). 'a -> 'b = <fun>
val f : ('a : word mod many external_) 'b. 'a -> 'b = <fun>
val g : 'a ('b : word mod many external_). 'a -> 'b = <fun>
|}]

(********************************************)
(* Test 3: Annotation on types in functions *)

let f : ('a : any) -> 'a = fun x -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f : ('a : any). 'a -> 'a = fun x -> x
;;
[%%expect {|
Line 1, characters 31-41:
1 | let f : ('a : any). 'a -> 'a = fun x -> x
                                   ^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         ('a : any). 'a -> 'a
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because we must know concretely how to pass a function argument.
|}]
(* CR layouts v2.9: This error message is not great. Check later if layout history
   is able to improve it. *)

let f : ('a : float64). 'a -> 'a = fun x -> x
;;
[%%expect {|
val f : ('a : float64). 'a -> 'a = <fun>
|}]

(********************************************)
(* Test 4: Annotation on record field types *)

type r = { field : ('a : immediate). 'a -> 'a }
let f { field } = field 5
;;
[%%expect {|
type r = { field : ('a : immediate). 'a -> 'a; }
val f : r -> int = <fun>
|}]

type rf = { fieldf : ('a : float64). 'a -> 'a }
let f { fieldf } = fieldf (Stdlib_upstream_compatible.Float_u.of_float 3.14);;
[%%expect {|
type rf = { fieldf : ('a : float64). 'a -> 'a; }
val f : rf -> Stdlib_upstream_compatible.Float_u.t = <fun>
|}]

type rg = { fieldg : ('a : value mod global). 'a -> 'a }
let f { fieldg } = fieldg 5;;
[%%expect {|
type rg = { fieldg : ('a : value mod global). 'a -> 'a; }
val f : rg -> int = <fun>
|}]

type rc = { fieldc : ('a : word mod external_ many shared). 'a -> 'a }
let f { fieldc } =
  let x : _ as (_ : word mod external_ many shared) = assert false in
  fieldc x;;
[%%expect {|
type rc = { fieldc : ('a : word mod many external_). 'a -> 'a; }
val f : ('a : word mod many external_). rc -> 'a = <fun>
|}]

let f { field } = field "hello"
;;
[%%expect {|
Line 1, characters 24-31:
1 | let f { field } = field "hello"
                            ^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of r at line 1, characters 0-47.
|}]

let f { fieldg } = fieldg "hello"
;;
[%%expect {|
Line 1, characters 26-33:
1 | let f { fieldg } = fieldg "hello"
                              ^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : value mod global)
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of value mod global
         because of the definition of rg at line 1, characters 0-56.
|}]

let f { fieldc } = fieldc "hello"
;;
[%%expect {|
Line 1, characters 26-33:
1 | let f { fieldc } = fieldc "hello"
                              ^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : word mod many external_)
       The layout of string is value
         because it is the primitive type string.
       But the layout of string must be a sublayout of word
         because of the definition of rc at line 1, characters 0-70.
|}]

let r = { field = fun x -> x }
let r = { field = Fun.id }
;;
[%%expect {|
val r : r = {field = <fun>}
val r : r = {field = <fun>}
|}]

let r = { field = fun (type (a : immediate)) (x : a) -> x }
let r = { field = fun (type (a : value mod global)) (x : a) -> x }
;;
[%%expect {|
val r : r = {field = <fun>}
val r : r = {field = <fun>}
|}]

let r = { field = fun (type (a : value)) (x : a) -> x }
;;
[%%expect {|
val r : r = {field = <fun>}
|}]

type r_value = { field : 'a. 'a -> 'a }
let r = { field = fun (type a : immediate) (x : a) -> x }

[%%expect{|
type r_value = { field : 'a. 'a -> 'a; }
Line 2, characters 18-55:
2 | let r = { field = fun (type a : immediate) (x : a) -> x }
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This field value has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       The kind of 'a is value
         because of the definition of r_value at line 1, characters 0-39.
       But the kind of 'a must be a subkind of immediate
         because of the annotation on the abstract type declaration for a.
|}]
(* CR layouts v1.5: that's a pretty awful error message *)

let r = { field = fun (type a : value mod global) (x : a) -> x }
[%%expect{|
Line 1, characters 18-62:
1 | let r = { field = fun (type a : value mod global) (x : a) -> x }
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This field value has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       The kind of 'a is value
         because of the definition of r_value at line 1, characters 0-39.
       But the kind of 'a must be a subkind of value mod global
         because of the annotation on the abstract type declaration for a.
|}]
(* CR layouts v1.5: that's a pretty awful error message *)

type ('a : immediate) t_imm

type s = { f : ('a : value). 'a -> 'a u }
and 'a u = 'a t_imm

[%%expect{|
type ('a : immediate) t_imm
Line 3, characters 15-39:
3 | type s = { f : ('a : value). 'a -> 'a u }
                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The kind of 'a is value
           because of the annotation on the universal variable 'a.
         But the kind of 'a must be a subkind of immediate
           because of the definition of t_imm at line 1, characters 0-27.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]
(* CR layouts v1.5: the location on that message is wrong. But it's hard
   to improve, because it comes from re-checking typedtree, where we don't
   have locations any more. I conjecture the same location problem exists
   when constraints aren't satisfied. *)

type ('a : value mod global) t_global

type s = { f : ('a : value). 'a -> 'a u }
and 'a u = 'a t_global
[%%expect {|
type ('a : value mod global) t_global
Line 3, characters 15-39:
3 | type s = { f : ('a : value). 'a -> 'a u }
                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The kind of 'a is value
           because of the annotation on the universal variable 'a.
         But the kind of 'a must be a subkind of value mod global
           because of the definition of t_global at line 1, characters 0-37.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

type ('a : word mod external_ many shared) t_complex

type s = { f : ('a : word). 'a -> 'a u }
and 'a u = 'a t_complex
[%%expect {|
type ('a : word mod many external_) t_complex
Line 3, characters 15-38:
3 | type s = { f : ('a : word). 'a -> 'a u }
                   ^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The kind of 'a is word
           because of the annotation on the universal variable 'a.
         But the kind of 'a must be a subkind of word mod many external_
           because of the definition of t_complex at line 1, characters 0-52.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

(********************)
(* Test 5: newtypes *)

let f = fun (type (a : value)) (x : a) -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f = fun (type (a : immediate)) (x : a) -> x
;;
[%%expect {|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun (type (a : float64)) (x : a) -> x
;;
[%%expect {|
val f : ('a : float64). 'a -> 'a = <fun>
|}]

let f = fun (type (a : value mod global)) (x : a) -> x
;;
[%%expect {|
val f : ('a : value mod global). 'a -> 'a = <fun>
|}]

let f = fun (type (a : word mod external_ many shared)) (x : a) -> x
;;
[%%expect {|
val f : ('a : word mod many external_). 'a -> 'a = <fun>
|}]

let f = fun (type (a : any)) (x : a) -> x
;;
[%%expect {|
Line 1, characters 29-36:
1 | let f = fun (type (a : any)) (x : a) -> x
                                 ^^^^^^^
Error: This pattern matches values of type a
       but a pattern was expected which matches values of type
         ('a : '_representable_layout_1)
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(****************************************)
(* Test 6: abstract universal variables *)

let f : type (a : value). a -> a = fun x -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f : type (a : immediate). a -> a = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f : type (a : float64). a -> a = fun x -> x
;;
[%%expect {|
val f : ('a : float64). 'a -> 'a = <fun>
|}]

let f : type (a : value mod global). a -> a = fun x -> x
;;
[%%expect {|
val f : ('a : value mod global). 'a -> 'a = <fun>
|}]

let f : type (a : word mod external_ many shared). a -> a = fun x -> x
;;
[%%expect {|
val f : ('a : word mod many external_). 'a -> 'a = <fun>
|}]

let f : type (a : any). a -> a = fun x -> x
;;
[%%expect {|
Line 1, characters 33-43:
1 | let f : type (a : any). a -> a = fun x -> x
                                     ^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(**************************************************)
(* Test 7: Defaulting universal variable to value *)

module type S = sig
  val f : 'a. 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
Line 2, characters 10-36:
2 |   val f : 'a. 'a t2_imm -> 'a t2_imm
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have kind value.
       But it was inferred to have kind immediate
         because of the definition of t2_imm at line 1, characters 0-28.
|}]

let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x

[%%expect {|
Line 1, characters 8-34:
1 | let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have kind value.
       But it was inferred to have kind immediate
         because of the definition of t2_imm at line 1, characters 0-28.
|}]

(********************************************)
(* Test 8: Annotation on universal variable *)

module type S = sig
  val f : ('a : value). 'a t2 -> 'a t2
end
;;
[%%expect {|
module type S = sig val f : 'a t2 -> 'a t2 end
|}]

module type S = sig
  val f : ('a : value). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
Line 2, characters 10-46:
2 |   val f : ('a : value). 'a t2_imm -> 'a t2_imm
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind immediate
         because of the definition of t2_imm at line 1, characters 0-28.
|}]

module type S = sig
  val f : ('a : value). 'a t2_global -> 'a t2_global
end
;;
[%%expect {|
Line 2, characters 10-52:
2 |   val f : ('a : value). 'a t2_global -> 'a t2_global
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind value mod global
         because of the definition of t2_global at line 8, characters 0-38.
|}]

module type S = sig
  val f : ('a : value). 'a t2_complex -> 'a t2_complex
end
;;
[%%expect {|
Line 2, characters 24-26:
2 |   val f : ('a : value). 'a t2_complex -> 'a t2_complex
                            ^^
Error: This type ('a : value) should be an instance of type
         ('b : word mod many external_)
       The layout of 'a is value
         because of the annotation on the universal variable 'a.
       But the layout of 'a must overlap with word
         because of the definition of t2_complex at line 10, characters 0-53.
|}]

module type S = sig
  val f : ('a : word). 'a t2_complex -> 'a t2_complex
end
;;
[%%expect {|
Line 2, characters 10-53:
2 |   val f : ('a : word). 'a t2_complex -> 'a t2_complex
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind word.
       But it was inferred to have kind word mod many external_
         because of the definition of t2_complex at line 10, characters 0-53.
|}]

module type S = sig
  val f : 'a t2_imm -> 'a t2_imm
  val g : ('a : immediate). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
module type S =
  sig
    val f : ('a : immediate). 'a t2_imm -> 'a t2_imm
    val g : ('a : immediate). 'a t2_imm -> 'a t2_imm
  end
|}]

module type S = sig
  val f : 'a t2_float64 -> 'a t2_float64
  val g : ('a : float64). 'a t2_float64 -> 'a t2_float64
end
;;
[%%expect {|
module type S =
  sig
    val f : ('a : float64). 'a t2_float64 -> 'a t2_float64
    val g : ('a : float64). 'a t2_float64 -> 'a t2_float64
  end
|}]

module type S = sig
  val f : 'a t2_global -> 'a t2_global
  val g : ('a : value mod global). 'a t2_global -> 'a t2_global
end
;;
[%%expect {|
module type S =
  sig
    val f : ('a : value mod global). 'a t2_global -> 'a t2_global
    val g : ('a : value mod global). 'a t2_global -> 'a t2_global
  end
|}]

module type S = sig
  val f : 'a t2_complex -> 'a t2_complex
  val g : ('a : word mod external_ many shared). 'a t2_complex -> 'a t2_complex
  val h : ('a : word mod external_ many). 'a t2_complex -> 'a t2_complex
end
;;
[%%expect {|
module type S =
  sig
    val f : ('a : word mod many external_). 'a t2_complex -> 'a t2_complex
    val g : ('a : word mod many external_). 'a t2_complex -> 'a t2_complex
    val h : ('a : word mod many external_). 'a t2_complex -> 'a t2_complex
  end
|}]

(************************************************************)
(* Test 9: Annotation on universal in polymorphic parameter *)

let f (x : ('a : value). 'a -> 'a) = x "string", x 5

[%%expect {|
val f : ('a. 'a -> 'a) -> string * int = <fun>
|}]

let f (x : ('a : word mod external_ many shared). 'a -> 'a) =
  let native_int : nativeint# = assert false in
  x native_int

[%%expect {|
val f : (('a : word mod many external_). 'a -> 'a) -> nativeint# = <fun>
|}]

let f (x : ('a : immediate). 'a -> 'a) = x "string"

[%%expect {|
Line 1, characters 43-51:
1 | let f (x : ('a : immediate). 'a -> 'a) = x "string"
                                               ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the annotation on the universal variable 'a.
|}]

let f (x : ('a : value mod global). 'a -> 'a) = x "string"

[%%expect {|
Line 1, characters 50-58:
1 | let f (x : ('a : value mod global). 'a -> 'a) = x "string"
                                                      ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : value mod global)
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of value mod global
         because of the annotation on the universal variable 'a.
|}]

(**************************************)
(* Test 10: Annotation of record type *)

type t_value : value
[%%expect {|
type t_value
|}]

type t = { x : int }
let f (x : t) : _ as (_ : value) = x
[%%expect {|
type t = { x : int; }
val f : t -> t = <fun>
|}]

type t : value = { x : t_value }
[%%expect {|
type t = { x : t_value; }
|}]

type t : value mod global = { x : int}
[%%expect {|
Line 1, characters 0-38:
1 | type t : value mod global = { x : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: This should be accepted, because t should be inferred to be
   immediate *)

type t : any mod portable = { x : float }
[%%expect {|
Line 1, characters 0-41:
1 | type t : any mod portable = { x : float }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of any mod portable
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: This should be accepted, because t should be inferred to be
   immediate *)

type t = { x : int } [@@unboxed]
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]

type t : value = { x : int } [@@unboxed]
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t : value = { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : immediate = { x : int } [@@unboxed]
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t : immediate = { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]

type t : value mod global = { x : t_value }
[%%expect {|
Line 1, characters 0-43:
1 | type t : value mod global = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type t : value mod unique = { x : t_value }
[%%expect {|
Line 1, characters 0-43:
1 | type t : value mod unique = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod unique
         because of the annotation on the declaration of the type t.
|}]

type t : value mod many = { x : t_value }
[%%expect {|
Line 1, characters 0-41:
1 | type t : value mod many = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod many
         because of the annotation on the declaration of the type t.
|}]

type t : value mod portable = { x : t_value }
[%%expect {|
Line 1, characters 0-45:
1 | type t : value mod portable = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
|}]

type t : value mod uncontended = { x : t_value }
[%%expect {|
Line 1, characters 0-48:
1 | type t : value mod uncontended = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod uncontended
         because of the annotation on the declaration of the type t.
|}]

type t : value mod external_ = { x : t_value }
[%%expect {|
Line 1, characters 0-46:
1 | type t : value mod external_ = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod external_
         because of the annotation on the declaration of the type t.
|}]

(***************************************)
(* Test 11: Annotation of variant type *)

type t = Foo of int
let f (x : t) : _ as (_ : value) = x
[%%expect {|
type t = Foo of int
val f : t -> t = <fun>
|}]

type t : value = Foo of t_value
[%%expect {|
type t = Foo of t_value
|}]

type t : value mod global = Foo of int
[%%expect {|
Line 1, characters 0-38:
1 | type t : value mod global = Foo of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type t : any mod portable = Foo of float
[%%expect {|
Line 1, characters 0-40:
1 | type t : any mod portable = Foo of float
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of any mod portable
         because of the annotation on the declaration of the type t.
|}]

type t = Foo | Bar
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = Foo | Bar
val f : t -> t = <fun>
|}]

type t : value = Foo | Bar
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = Foo | Bar
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : immediate = Foo | Bar
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = Foo | Bar
val f : t -> t = <fun>
|}]

type t = Foo of int [@@unboxed]
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = Foo of int [@@unboxed]
val f : t -> t = <fun>
|}]

type t : value = Foo of int [@@unboxed]
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t : value = Foo of int [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : immediate = Foo of int [@@unboxed]
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t : immediate = Foo of int [@@unboxed]
val f : t -> t = <fun>
|}]

type t : value mod global = Foo of t_value
[%%expect {|
Line 1, characters 0-42:
1 | type t : value mod global = Foo of t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type t : value mod unique = Foo of t_value
[%%expect {|
Line 1, characters 0-42:
1 | type t : value mod unique = Foo of t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod unique
         because of the annotation on the declaration of the type t.
|}]

type t : value mod many = Foo of t_value
[%%expect {|
Line 1, characters 0-40:
1 | type t : value mod many = Foo of t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod many
         because of the annotation on the declaration of the type t.
|}]

type t : value mod portable = Foo of t_value
[%%expect {|
Line 1, characters 0-44:
1 | type t : value mod portable = Foo of t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
|}]

type t : value mod uncontended = Foo of t_value
[%%expect {|
Line 1, characters 0-47:
1 | type t : value mod uncontended = Foo of t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod uncontended
         because of the annotation on the declaration of the type t.
|}]

type t : value mod external_ = Foo of t_value
[%%expect {|
Line 1, characters 0-45:
1 | type t : value mod external_ = Foo of t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed variant type.
       But the kind of type t must be a subkind of value mod external_
         because of the annotation on the declaration of the type t.
|}]

(***************************************)
(* Test 12: Annotation on private type *)

type t = private int
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]

type t : value = private int
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : immediate = private int
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]

type t : bits64 mod portable unique
type u = private t
let f (x : t) : _ as (_ : bits64 mod portable unique) = x
[%%expect {|
type t : bits64 mod portable unique
type u = private t
val f : t -> t = <fun>
|}]

type t : bits64 mod portable unique
type u : bits64 = private t
let f (x : t) : _ as (_ : bits64 mod portable unique) = x
[%%expect {|
type t : bits64 mod portable unique
type u = private t
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since u is nominative *)

type t : bits64 mod portable unique
type u : bits64 mod portable unique = private t
let f (x : t) : _ as (_ : bits64 mod portable unique) = x
[%%expect {|
type t : bits64 mod portable unique
type u = private t
val f : t -> t = <fun>
|}]

type t : float64 mod global portable
type u : bits64 mod global portable = private t
[%%expect {|
type t : float64 mod global portable
Line 2, characters 0-47:
2 | type u : bits64 mod global portable = private t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is float64
         because of the definition of t at line 1, characters 0-36.
       But the layout of type t must be a sublayout of bits64
         because of the definition of u at line 2, characters 0-47.
|}]

type t : word
type u : word mod global = private t
[%%expect {|
type t : word
Line 2, characters 0-36:
2 | type u : word mod global = private t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is word
         because of the definition of t at line 1, characters 0-13.
       But the kind of type t must be a subkind of word mod global
         because of the definition of u at line 2, characters 0-36.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
  mismatch, not a layout mismatch. *)

type t : word
type u : word mod unique = private t
[%%expect {|
type t : word
Line 2, characters 0-36:
2 | type u : word mod unique = private t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is word
         because of the definition of t at line 1, characters 0-13.
       But the kind of type t must be a subkind of word mod unique
         because of the definition of u at line 2, characters 0-36.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
  mismatch, not a layout mismatch. *)

type t : word
type u : word mod many = private t
[%%expect {|
type t : word
Line 2, characters 0-34:
2 | type u : word mod many = private t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is word
         because of the definition of t at line 1, characters 0-13.
       But the kind of type t must be a subkind of word mod many
         because of the definition of u at line 2, characters 0-34.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
  mismatch, not a layout mismatch. *)

type t : word
type u : word mod portable = private t
[%%expect {|
type t : word
Line 2, characters 0-38:
2 | type u : word mod portable = private t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is word
         because of the definition of t at line 1, characters 0-13.
       But the kind of type t must be a subkind of word mod portable
         because of the definition of u at line 2, characters 0-38.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
  mismatch, not a layout mismatch. *)

type t : word
type u : word mod uncontended = private t
[%%expect {|
type t : word
Line 2, characters 0-41:
2 | type u : word mod uncontended = private t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is word
         because of the definition of t at line 1, characters 0-13.
       But the kind of type t must be a subkind of word mod uncontended
         because of the definition of u at line 2, characters 0-41.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
  mismatch, not a layout mismatch. *)

type t : word
type u : word mod external_ = private t
[%%expect {|
type t : word
Line 2, characters 0-39:
2 | type u : word mod external_ = private t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is word
         because of the definition of t at line 1, characters 0-13.
       But the kind of type t must be a subkind of word mod external_
         because of the definition of u at line 2, characters 0-39.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
  mismatch, not a layout mismatch. *)

(**************************************)
(* Test 13: Parsing & pretty-printing *)

let f (type a : value) (x : a) = x
let f (type a : immediate) (x : a) = x
let f (type a : value mod global) (x : a) = x
let f (type a : immediate mod global) (x : a) = x
let f (type a : word mod external_ many shared) (x : a) = x

[%%expect{|
val f : 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : value mod global). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : word mod many external_). 'a -> 'a = <fun>
|}]

let f = fun (type a : value) (x : a) -> x
let f = fun (type a : immediate) (x : a) -> x
let f = fun (type a : value mod global) (x : a) -> x
let f = fun (type a : immediate mod global) (x : a) -> x
let f = fun (type a : word mod external_ many shared) (x : a) -> x

[%%expect{|
val f : 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : value mod global). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : word mod many external_). 'a -> 'a = <fun>
|}]

let o = object
  method m : type (a : value). a -> a = fun x -> x
end
let o = object
  method m : type (a : immediate). a -> a = fun x -> x
end
let o = object
  method m : type (a : value mod global). a -> a = fun x -> x
end
let o = object
  method m : type (a : immediate mod global). a -> a = fun x -> x
end
let o = object
  method m : type (a : word mod external_ many shared). a -> a = fun x -> x
end

[%%expect{|
val o : < m : 'a. 'a -> 'a > = <obj>
val o : < m : ('a : immediate). 'a -> 'a > = <obj>
val o : < m : ('a : value mod global). 'a -> 'a > = <obj>
val o : < m : ('a : immediate). 'a -> 'a > = <obj>
val o : < m : ('a : word mod many external_). 'a -> 'a > = <obj>
|}]

let f : type (a : value). a -> a = fun x -> x
let f : type (a : immediate). a -> a = fun x -> x
let f : type (a : value mod global). a -> a = fun x -> x
let f : type (a : immediate mod global). a -> a = fun x -> x
let f : type (a : word mod external_ many shared). a -> a = fun x -> x

[%%expect{|
val f : 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : value mod global). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : word mod many external_). 'a -> 'a = <fun>
|}]

let f x =
  let local_ g (type a : value) (x : a) = x in
  g x [@nontail]

let f x =
  let local_ g (type a : immediate) (x : a) = x in
  g x [@nontail]

let f x =
  let local_ g (type a : value mod global) (x : a) = x in
  g x [@nontail]

let f x =
  let local_ g (type a : immediate mod global) (x : a) = x in
  g x [@nontail]

let f x =
  let local_ g (type a : word mod external_ many shared) (x : a) = x in
  g x [@nontail]

[%%expect{|
val f : 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : value mod global). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : word mod many external_). 'a -> 'a = <fun>
|}]

let f = fun x y (type (a : value)) (z : a) -> z
let f = fun x y (type (a : immediate)) (z : a) -> z
let f = fun x y (type (a : value mod global)) (z : a) -> z
let f = fun x y (type (a : immediate mod global)) (z : a) -> z
let f = fun x y (type (a : word mod external_ many shared)) (z : a) -> z

[%%expect{|
val f : 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : immediate). 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : value mod global). 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : immediate). 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : word mod many external_). 'b -> 'c -> 'a -> 'a = <fun>
|}]

let f = fun x y (type a : value) (z : a) -> z
let f = fun x y (type a : immediate) (z : a) -> z
let f = fun x y (type a : value mod global) (z : a) -> z
let f = fun x y (type a : immediate mod global) (z : a) -> z
let f = fun x y (type a : word mod external_ many shared) (z : a) -> z

[%%expect{|
val f : 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : immediate). 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : value mod global). 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : immediate). 'b -> 'c -> 'a -> 'a = <fun>
val f : 'b 'c ('a : word mod many external_). 'b -> 'c -> 'a -> 'a = <fun>
|}]
(* CR layouts: canonicalizing the order of quantification here
   would reduce wibbles in error messages *)

external f : ('a : value). 'a -> 'a = "%identity"
external f : ('a : immediate). 'a -> 'a = "%identity"
external f : ('a : value mod global). 'a -> 'a = "%identity"
external f : ('a : immediate mod global). 'a -> 'a = "%identity"
external f : ('a : word mod external_ many shared). 'a -> 'a = "%identity"

[%%expect{|
external f : 'a -> 'a = "%identity"
external f : ('a : immediate). 'a -> 'a = "%identity"
external f : ('a : value mod global). 'a -> 'a = "%identity"
external f : ('a : immediate). 'a -> 'a = "%identity"
external f : ('a : word mod many external_). 'a -> 'a = "%identity"
|}]


type (_ : any) t2_any
exception E : ('a : value) ('b : any). 'b t2_any * 'a list -> exn
exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn
exception E : ('a : value mod global) ('b : any). 'b t2_any * 'a list -> exn
exception E : ('a : immediate mod global) ('b : any). 'b t2_any * 'a list -> exn

[%%expect{|
type (_ : any) t2_any
exception E : ('b : any) 'a. 'b t2_any * 'a list -> exn
exception E : ('b : any) ('a : immediate). 'b t2_any * 'a list -> exn
exception E : ('b : any) ('a : value mod global). 'b t2_any * 'a list -> exn
exception E : ('b : any) ('a : immediate). 'b t2_any * 'a list -> exn
|}]

let f (x : ('a : value). 'a -> 'a) = x 3, x true
let f (x : ('a : immediate). 'a -> 'a) = x 3, x true
let f (x : ('a : value mod global). 'a -> 'a) = x 3, x true
let f (x : ('a : immediate mod global). 'a -> 'a) = x 3, x true

[%%expect{|
val f : ('a. 'a -> 'a) -> int * bool = <fun>
val f : (('a : immediate). 'a -> 'a) -> int * bool = <fun>
val f : (('a : value mod global). 'a -> 'a) -> int * bool = <fun>
val f : (('a : immediate). 'a -> 'a) -> int * bool = <fun>
|}]

type _ a = Mk : [> ] * ('a : value) -> int a
type _ a = Mk : [> ] * ('a : immediate) -> int a
type _ a = Mk : [> ] * ('a : value mod global) -> int a
type _ a = Mk : [> ] * ('a : immediate mod global) -> int a

[%%expect {|
type _ a = Mk : [>  ] * 'a -> int a
type _ a = Mk : ('a : immediate). [>  ] * 'a -> int a
type _ a = Mk : ('a : value mod global). [>  ] * 'a -> int a
type _ a = Mk : ('a : immediate). [>  ] * 'a -> int a
|}]

let f_imm : ('a : immediate). 'a -> 'a = fun x -> x

[%%expect {|
val f_imm : ('a : immediate). 'a -> 'a = <fun>
|}]

let f_val : ('a : value). 'a -> 'a = fun x -> f_imm x

[%%expect {|
Line 1, characters 37-53:
1 | let f_val : ('a : value). 'a -> 'a = fun x -> f_imm x
                                         ^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       The kind of 'a is value
         because of the annotation on the universal variable 'a.
       But the kind of 'a must be a subkind of immediate
         because of the definition of f_imm at line 1, characters 4-9.
|}]

type (_ : value) g =
  | A : ('a : value). 'a g
  | B : ('a : immediate). 'a g
  | C : ('a : value mod global). 'a g
  | D : ('a : immediate mod global). 'a g

[%%expect {|
type _ g =
    A : 'a g
  | B : ('a : immediate). 'a g
  | C : ('a : value mod global). 'a g
  | D : ('a : immediate). 'a g
|}]

type t = int as (_ : value)
type t = int as (_ : immediate)
type t = int as (_ : value mod global)
type t = int as (_ : immediate mod global)
type t = nativeint# as (_ : word mod external_ many shared)

[%%expect {|
type t = int
type t = int
type t = int
type t = int
type t = nativeint#
|}]
