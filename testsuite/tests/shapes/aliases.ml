(* TEST
 flags = "-dshape";
 expect;
*)
module A = struct type t end
module B = A
[%%expect{|
{
 "A"[module] -> {<.3>
                 "t"[type] -> <.2>;
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.4>
                      {<.3>
                       "t"[type] -> <.2>;
                       });
 }
module B = A
|}]

type u = B.t

[%%expect{|
{
 "u"[type] -> <.5>;
 }
type u = B.t
|}]

module F (X : sig type t end) = X
module F' = F
[%%expect{|
{
 "F"[module] -> Abs<.8>(X, X<.7>);
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.9>
                       Abs<.8>(X, X<.7>));
 }
module F' = F
|}]

module C = F'(A)
[%%expect{|
{
 "C"[module] -> {<.10>
                 "t"[type] -> <.2>;
                 };
 }
module C : sig type t = A.t end
|}]


module C = F(B)

[%%expect{|
{
 "C"[module] -> Alias(<.11>
                      {<.3>
                       "t"[type] -> <.2>;
                       });
 }
module C : sig type t = B.t end
|}]

module D = C

[%%expect{|
{
 "D"[module] -> Alias(<.12>
                      Alias(<.11>
                            {<.3>
                             "t"[type] -> <.2>;
                             }));
 }
module D = C
|}]

module G (X : sig type t end) = struct include X end
[%%expect{|
{
 "G"[module] -> Abs<.15>(X, {
                             "t"[type] -> X<.14> . "t"[type];
                             });
 }
module G : functor (X : sig type t end) -> sig type t = X.t end
|}]

module E = G(B)
[%%expect{|
{
 "E"[module] -> {<.16>
                 "t"[type] -> <.2>;
                 };
 }
module E : sig type t = B.t end
|}]

module M = struct type t let x = 1 end
module N : sig type t end = M
module O = N
[%%expect{|
{
 "M"[module] -> {<.19>
                 "t"[type] -> <.17>;
                 "x"[value] -> <.18>;
                 };
 }
module M : sig type t val x : int end
{
 "N"[module] -> {<.21>
                 "t"[type] -> <.17>;
                 };
 }
module N : sig type t end
{
 "O"[module] -> Alias(<.22>
                      {<.21>
                       "t"[type] -> <.17>;
                       });
 }
module O = N
|}]
