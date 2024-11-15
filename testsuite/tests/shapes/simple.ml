(* TEST
 flags = "-dshape";
 expect;
*)

let x = ()
[%%expect{|
{
 "x"[value] -> <.2>;
 }
val x : unit = ()
|}]

external y : int -> int = "%identity"
[%%expect{|
{
 "y"[value] -> <.3>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] -> {<.5>
                 "Bar"[constructor] -> {<.7>};
                 };
 "t"[type] -> {<.4>
               "A"[constructor] -> {<.6>};
               };
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.9>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> {<.10>};
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <.11>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> {<.12>};
 "B"[extension constructor] -> {<.13>};
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.15>
                 "C"[extension constructor] -> {<.14>};
                 };
 }
module M : sig type ext += C end
|}]

module _ = struct
  type t = Should_not_appear_in_shape
end
[%%expect{|
{}
|}]

module rec M1 : sig
  type t = C of M2.t
end = struct
  type t = C of M2.t
end

and M2 : sig
  type t
  val x : t
end = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "M1"[module] -> {
                  "t"[type] -> {<.29>
                                "C"[constructor] -> {<.30>};
                                };
                  };
 "M2"[module] ->
   {
    "t"[type] -> {<.31>
                  "T"[constructor] -> {<.32>};
                  };
    "x"[value] -> <.33>;
    };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.34>;
 "c"[class] -> <.34>;
 "c"[class type] -> <.34>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.47>;
 "c"[class type] -> <.47>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] -> <.48>;
 }
type u = t
|}]
