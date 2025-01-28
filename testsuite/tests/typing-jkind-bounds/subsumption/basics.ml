(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module _ : sig
  type t
end = struct
  type t : value with int
end
[%%expect {|
|}]

module _ : sig
  type _ t
end = struct
  type 'a t : value with 'a
end
[%%expect {|
|}]

module _ : sig
  type t : immediate
end = struct
  type t : immediate with int
end
[%%expect {|
|}]

module _ : sig
  type t : float64
end = struct
  type t : float64 with int
end
[%%expect {|
|}]

module _ = struct
  type u : immutable_data
  type t : immutable_data with int = u
end
[%%expect {|
|}]

module _ : sig
  
end = struct
  type ('a, 'b) t : immutable_data with 'b with 'a

  module type S = sig
    type ('a, 'b) t : immutable_data with 'a with 'b
  end

  module type T = S with type ('a, 'b) t = ('a, 'b) t
end
[%%expect {|
|}]

module _ : sig
  type ('a, 'b) t : immutable_data with 'a with 'b
end = struct
  type ('a, 'b) t : immutable_data with 'b
end
[%%expect {|
|}]

module _ : sig
  type ('a, 'b) t : immutable_data with 'a
end = struct
  type ('a, 'b) t : immutable_data with 'b with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t : immutable_data with 'b with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t : immutable_data with 'a with 'b end
       is not included in
         sig type ('a, 'b) t : immutable_data with 'a end
       Type declarations do not match:
         type ('a, 'b) t : immutable_data with 'a with 'b
       is not included in
         type ('a, 'b) t : immutable_data with 'a
       The kind of the first is immutable_data with 'a with 'b
         because of the definition of t at line 4, characters 2-50.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-42.
|}]

module _ = struct
  type ('a, 'b) u : immutable_data with 'b with 'a
  type ('a, 'b) t : immutable_data with 'a = ('a, 'b) u
end
[%%expect {|
Line 3, characters 2-55:
3 |   type ('a, 'b) t : immutable_data with 'a = ('a, 'b) u
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "('a, 'b) u" is immutable_data with 'a with 'b
         because of the definition of u at line 2, characters 2-50.
       But the kind of type "('a, 'b) u" must be a subkind of immutable_data
         with 'a
         because of the definition of t at line 3, characters 2-55.
|}]

module _ = struct
  type ('a, 'b) t : immutable_data with 'a with 'b

  module type S = sig
    type ('a, 'b) t : immutable_data with 'a
  end

  module type T = S with type ('a, 'b) t = ('a, 'b) t
end
[%%expect {|
Line 8, characters 25-53:
8 |   module type T = S with type ('a, 'b) t = ('a, 'b) t
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "('a, 'b) t" is immutable_data with 'a with 'b
         because of the definition of t at line 2, characters 2-50.
       But the kind of type "('a, 'b) t" must be a subkind of immutable_data
         with 'a
         because of the definition of t at line 5, characters 4-44.
|}]

module _ : sig
  type 'a t : mutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a ref
end
[%%expect {|
|}]

module _ = struct
  type ('a : immutable_data) u : immutable_data with 'a
  type 'a t : immutable_data = 'a u
end
[%%expect {|
|}]

module _ : sig
  type t : mutable_data
end = struct
  type a : immediate
  type b : immutable_data with a with int with string with a ref
  type c : mutable_data with b with a with b with a
  type d : immediate with a
  type t : immutable_data mod aliased with d with d with b with d with c with a
end
[%%expect {|
|}]

module _ = struct
  type u
  type t : value mod portable with u
  type q : value mod portable with t = { x : t }
end
[%%expect {|
|}]

module _ = struct
  type u
  type t : value mod portable with u
  type v : value mod portable with t
  type q : value mod portable with t = { x : v }
end
[%%expect {|
|}]

type u
type t = private u
type v : immutable_data with u = { value : t }
(* CR layouts v2.8: this should be accepted *)
[%%expect {|
type u
type t = private u
Line 3, characters 0-46:
3 | type v : immutable_data with u = { value : t }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "v" is immutable_data with t
         because it's a boxed record type.
       But the kind of type "v" must be a subkind of immutable_data with u
         because of the annotation on the declaration of the type v.
|}]

type t : immediate with t = [`foo | `bar]
[%%expect {|
type t = [ `bar | `foo ]
|}]

type t
type u : immutable_data with t = [`foo of t]
(* CR layouts v2.8: we can do better for polymorphic variants *)
[%%expect {|
type t
Line 2, characters 0-44:
2 | type u : immutable_data with t = [`foo of t]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[ `foo of t ]" is value
         because it's a polymorphic variant type.
       But the kind of type "[ `foo of t ]" must be a subkind of immutable_data
         with t
         because of the definition of u at line 2, characters 0-44.
|}]
