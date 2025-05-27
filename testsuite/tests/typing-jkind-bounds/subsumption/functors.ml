(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module F (M : sig type t end) = struct
  type t : immutable_data with M.t
end

module Int = struct
  type t = int
end
type t : immutable_data = F(Int).t
[%%expect {|
module F :
  functor (M : sig type t end) -> sig type t : immutable_data with M.t end
module Int : sig type t = int end
type t = F(Int).t
|}]

module T = struct
  type t
end
type t : immutable_data = F(T).t
[%%expect {|
module T : sig type t end
Line 4, characters 0-32:
4 | type t : immutable_data = F(T).t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "F(T).t" is immutable_data with T.t
         because of the definition of t at line 2, characters 2-34.
       But the kind of type "F(T).t" must be a subkind of immutable_data
         because of the definition of t at line 4, characters 0-32.
|}]

module F (M : sig type 'a t end) = struct
  type 'a t : immutable_data with 'a M.t
end

module Ref = struct
  type 'a t = 'a ref
end
type 'a t : mutable_data with 'a = 'a F(Ref).t
[%%expect {|
module F :
  functor (M : sig type 'a t end) ->
    sig type 'a t : immutable_data with 'a M.t end
module Ref : sig type 'a t = 'a ref end
type 'a t = 'a F(Ref).t
|}]

module Ref = struct
  type 'a t = 'a ref
end
type 'a t : immutable_data with 'a = 'a F(Ref).t
[%%expect {|
module Ref : sig type 'a t = 'a ref end
Line 4, characters 0-48:
4 | type 'a t : immutable_data with 'a = 'a F(Ref).t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a F(Ref).t" is mutable_data
         with 'a @@ unyielding many
         because of the definition of t at line 2, characters 2-40.
       But the kind of type "'a F(Ref).t" must be a subkind of immutable_data
         with 'a
         because of the definition of t at line 4, characters 0-48.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended with 'a
         visibility: mod read_write ≰ mod immutable with 'a
|}]

module Ref = struct
  type 'a t = 'a ref
end
type 'a t : mutable_data = 'a F(Ref).t
[%%expect {|
module Ref : sig type 'a t = 'a ref end
Line 4, characters 0-38:
4 | type 'a t : mutable_data = 'a F(Ref).t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a F(Ref).t" is mutable_data
         with 'a @@ unyielding many
         because of the definition of t at line 2, characters 2-40.
       But the kind of type "'a F(Ref).t" must be a subkind of mutable_data
         because of the definition of t at line 4, characters 0-38.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
         statefulness: mod stateless with 'a ≰ mod stateless
|}]

module F (M : sig
  type t
  type u
end) = struct
  type t : immediate with M.u with M.t
end

module Int_int = struct
  type t = int
  type u = int
end
type t : immutable_data = F(Int_int).t
[%%expect {|
module F :
  functor (M : sig type t type u end) ->
    sig type t : immediate with M.t with M.u end
module Int_int : sig type t = int type u = int end
type t = F(Int_int).t
|}]

module Int_abstract = struct
  type t = int
  type u : value mod global
end
type t : value mod global = F(Int_abstract).t
type t : value mod portable with Int_abstract.u = F(Int_abstract).t
[%%expect {|
module Int_abstract : sig type t = int type u : value mod global end
type t = F(Int_abstract).t
type t = F(Int_abstract).t
|}]

type t : value mod portable = F(Int_abstract).t
[%%expect {|
Line 1, characters 0-47:
1 | type t : value mod portable = F(Int_abstract).t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "F(Int_abstract).t" is immediate with Int_abstract.u
         because of the definition of t at line 5, characters 2-38.
       But the kind of type "F(Int_abstract).t" must be a subkind of
         value mod portable
         because of the definition of t at line 1, characters 0-47.
|}]
