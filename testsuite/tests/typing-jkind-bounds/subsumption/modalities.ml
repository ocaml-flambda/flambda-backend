(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module _ : sig
  type ('a, 'b) t : immutable_data with 'a
end = struct
  type ('a, 'b) t : immutable_data with 'a @@ portable
end
[%%expect {|
|}]

module _ : sig
  type ('a, 'b) t : immutable_data with 'a @@ portable
end = struct
  type ('a, 'b) t : immutable_data with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t : immutable_data with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t : immutable_data with 'a end
       is not included in
         sig type ('a, 'b) t : immutable_data with 'a end
       Type declarations do not match:
         type ('a, 'b) t : immutable_data with 'a
       is not included in
         type ('a, 'b) t : immutable_data with 'a
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-42.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-54.
|}]

module _ : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a @@ portable
end
[%%expect {|
|}]

module _ : sig
  type 'a t : value mod portable
end = struct
  type 'a t : immutable_data with 'a @@ portable
end
[%%expect {|
|}]

module _ : sig
  type 'a t : mutable_data with 'a @@ portable
end = struct
  type 'a t : mutable_data with 'a @@ portable
end
[%%expect {|
|}]

module _ = struct
  type 'a u : immutable_data with 'a @@ contended
  type 'a t : value mod portable = 'a u
end
[%%expect {|
Line 3, characters 2-39:
3 |   type 'a t : value mod portable = 'a u
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a u" is immutable_data with 'a
         because of the definition of u at line 2, characters 2-49.
       But the kind of type "'a u" must be a subkind of value mod portable
         because of the definition of t at line 3, characters 2-39.
|}]

module _ : sig
  type 'a t : value mod global
end = struct
  type 'a t : immediate with 'a @@ global
end
[%%expect {|
|}]

module _ : sig
  type 'a t : value mod uncontended
end = struct
  type 'a t : immutable_data with 'a @@ contended
end
[%%expect {|
|}]

module _ = struct
  type 'a u : immutable_data with 'a @@ many
  type 'a t : value mod many = 'a u
end
[%%expect {|
|}]

module _ : sig
  type 'a t : value mod unique
end = struct
  type 'a t : immediate with 'a @@ aliased
end
[%%expect {|
|}]

module _ : sig
  type 'a t : value mod global unique many portable uncontended
end = struct
  type 'a t : immediate with 'a @@ aliased many contended global portable
end
[%%expect {|
|}]

module _ : sig
  type ('a, 'b) t : value mod portable
end = struct
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ portable
end
[%%expect {|
|}]

module _ = struct
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ contended

  module type S = sig
    type ('a, 'b) t : value mod portable
  end

  module type T = S with type ('a, 'b) t = ('a, 'b) t
end
[%%expect {|
Line 8, characters 25-53:
8 |   module type T = S with type ('a, 'b) t = ('a, 'b) t
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "('a, 'b) t" is value mod portable with 'a with 'b
         because of the definition of t at line 2, characters 2-79.
       But the kind of type "('a, 'b) t" must be a subkind of
         value mod portable
         because of the definition of t at line 5, characters 4-40.
|}]

module _ : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ portable with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : immutable_data with 'a @@ portable with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : immutable_data with 'a end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t : immutable_data with 'a
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-56.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-48.
|}]

module _ : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a with 'a @@ portable
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : immutable_data with 'a with 'a @@ portable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : immutable_data with 'a end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t : immutable_data with 'a
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-56.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-48.
|}]

module _ : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a @@ portable with 'a
end
[%%expect {|
|}]

module _ = struct
  type 'a u : value mod uncontended with 'a @@ global
  type 'a t : value mod global = 'a u
end
[%%expect {|
Line 3, characters 2-37:
3 |   type 'a t : value mod global = 'a u
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a u" is value mod uncontended with 'a
         because of the definition of u at line 2, characters 2-53.
       But the kind of type "'a u" must be a subkind of value mod global
         because of the definition of t at line 3, characters 2-37.
|}]

module _ : sig
  type 'a t : immutable_data with 'a @@ contended portable
end = struct
  type 'a t : immutable_data with 'a @@ contended with 'a @@ portable
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : immutable_data with 'a @@ contended with 'a @@ portable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : immutable_data with 'a end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t : immutable_data with 'a
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-69.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-58.
|}]

module _ : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ contended portable with 'a @@ portable many
end
[%%expect {|
|}]

module _ = struct
  type ('a, 'b) u : immutable_data with 'a @@ portable with 'b @@ contended
  type ('a, 'b) t : immutable_data with 'a @@ portable with 'b @@ contended = ('a, 'b) u
end
[%%expect {|
|}]

module _ : sig
  type ('a, 'b) t : immutable_data with 'a @@ portable with 'b @@ contended
end = struct
  type ('a, 'b) t : immutable_data with 'a @@ contended with 'b @@ portable
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t : immutable_data with 'a @@ contended with 'b @@ portable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t : immutable_data with 'a with 'b end
       is not included in
         sig type ('a, 'b) t : immutable_data with 'a with 'b end
       Type declarations do not match:
         type ('a, 'b) t : immutable_data with 'a with 'b
       is not included in
         type ('a, 'b) t : immutable_data with 'a with 'b
       The kind of the first is immutable_data with 'a with 'b
         because of the definition of t at line 4, characters 2-75.
       But the kind of the first must be a subkind of immutable_data
         with 'a with 'b
         because of the definition of t at line 2, characters 2-75.
|}]

module _ : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ portable contended portable
end
[%%expect {|
|}]

module _ = struct
  type t : immutable_data with int ref @@ contended

  module type S = sig
    type t : immutable_data
  end

  module type T = S with type t = t
end
[%%expect {|
|}]
