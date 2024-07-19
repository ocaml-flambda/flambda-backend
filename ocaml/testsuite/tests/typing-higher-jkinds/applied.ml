(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value

[%%expect {|
type t : (value) => value
|}]


type p = int t

[%%expect {|

|}]


module M : sig
  type p = int t
end = struct
  type p = int t
end

[%%expect {|

|}]


module M : sig
  type 'a p = 'a t
end = struct
  type 'a p = 'a t
end

[%%expect {|

|}]


module M : sig
  type p
end = struct
  type p = int t
end

[%%expect {|
module M : sig type p end
|}]


type p = t t

[%%expect {|

|}]


type r : (value => value) => value

[%%expect {|
type r' : ((value) => value) => value
|}]


type ('a : value => value) r

[%%expect {|
type ('a : ((higher))) r'
|}]


module type M = sig
  val g : ('a : value => value). 'a r -> 'a r
end

[%%expect{|

|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : s r -> s r
end

[%%expect{|

|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : int s r -> int s r
end

[%%expect{|

|}]
