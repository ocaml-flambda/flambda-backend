(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

(* Test that not-best kinds are respected *)

module _ : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is value
         because of the definition of t at line 4, characters 2-11.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-36.
|}]

(* This appears sound to accept. But it isn't. See the following test. *)
module _ : sig
  type a
  type t : value mod portable with a
end = struct
  type a
  type t
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a type t end
       is not included in
         sig type a type t : value mod portable with a end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod portable with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod portable
         with a
         because of the definition of t at line 3, characters 2-36.
|}]

(* This test demonstrates why the above shouldn't be accepted. We can learn more about
   a via gadt refinement *)
module _ = struct
  type ('a, 'b) eq = Eq : ('a, 'a) eq

  module M : sig
    type a
    type t : value mod portable with a
    val a_is_int : (a, int) eq
  end = struct
    type a = int
    type t : value mod portable with a
    let a_is_int : (a, int) eq = Eq
  end

  let f (x : M.t @@ nonportable) : M.t @@ portable =
    match M.a_is_int with
    | Eq -> x
end
[%%expect {|
|}]

module _ = struct
  type a
  type b = a
  type u
  type t : value mod global with a = u
end
[%%expect {|
Line 5, characters 2-38:
5 |   type t : value mod global with a = u
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "u" is value
         because of the definition of u at line 4, characters 2-8.
       But the kind of type "u" must be a subkind of value mod global with a
         because of the definition of t at line 5, characters 2-38.
|}]

module _ (M : sig type t end) = struct
  module type S = sig
    type t : value mod global with M.t
  end
  type t
  module type T = S with type t = t
end
[%%expect {|
Line 6, characters 25-35:
6 |   module type T = S with type t = t
                             ^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of t at line 5, characters 2-8.
       But the kind of type "t" must be a subkind of value mod global with M.t
         because of the definition of t at line 3, characters 4-38.
|}]

module _ = struct
  module type S = sig
    type t
  end

  type a
  module M : S with type t = a = struct
    type t = a
  end

  module _ : sig
    type t : value mod portable uncontended with M.t
  end = struct
    type t : value mod portable
  end
end
[%%expect {|
Lines 13-15, characters 8-5:
13 | ........struct
14 |     type t : value mod portable
15 |   end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod portable end
       is not included in
         sig type t : value mod uncontended portable with M.t end
       Type declarations do not match:
         type t : value mod portable
       is not included in
         type t : value mod uncontended portable with M.t
       The kind of the first is value mod portable
         because of the definition of t at line 14, characters 4-31.
       But the kind of the first must be a subkind of
         value mod uncontended portable with M.t
         because of the definition of t at line 12, characters 4-52.
|}]

module _ = struct
  module type S = sig
    type t
    type u = t
  end

  type a
  module M : S with type t := a = struct
    type t = a
    type u = a
  end

  module _ : sig
    type t : value mod portable uncontended with M.u
  end = struct
    type t : value mod portable
  end
end
[%%expect {|
Lines 15-17, characters 8-5:
15 | ........struct
16 |     type t : value mod portable
17 |   end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod portable end
       is not included in
         sig type t : value mod uncontended portable with M.u end
       Type declarations do not match:
         type t : value mod portable
       is not included in
         type t : value mod uncontended portable with M.u
       The kind of the first is value mod portable
         because of the definition of t at line 16, characters 4-31.
       But the kind of the first must be a subkind of
         value mod uncontended portable with M.u
         because of the definition of t at line 14, characters 4-52.
|}]

module _ : sig
  type a = [`a of string | `b]
  type t : value mod global with a
end = struct
  type a = [`a of string | `b]
  type t
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a = [`a of string | `b]
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = [ `a of string | `b ] type t end
       is not included in
         sig
           type a = [ `a of string | `b ]
           type t : value mod global with a
         end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod global with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod global with a
         because of the definition of t at line 3, characters 2-34.
|}]

module _ : sig
  type a = < value : string >
  type t : value mod global with a
end = struct
  type a = < value : string >
  type t
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a = < value : string >
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = < value : string > type t end
       is not included in
         sig type a = < value : string > type t : value mod global with a end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod global with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod global with a
         because of the definition of t at line 3, characters 2-34.
|}]
