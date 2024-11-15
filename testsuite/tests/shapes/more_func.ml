(* TEST
 flags = "-dshape";
 expect;
*)
module M = struct end (* uid 0 *)
module F(X : sig end) = M
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.2>};
 }
module M : sig end
{
 "F"[module] -> Abs<.4>(X, {<.2>});
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.5>};
 }
module App : sig end
|}]


module M = struct end (* uid 4 *)
module F(X : sig end) = struct include M type t end
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.6>};
 }
module M : sig end
{
 "F"[module] -> Abs<.9>(X, {
                            "t"[type] -> <.8>;
                            });
 }
module F : functor (X : sig end) -> sig type t end
{
 "App"[module] -> {<.10>
                   "t"[type] -> <.8>;
                   };
 }
module App : sig type t = F(List).t end
|}]

module M = struct end (* uid 9 *)
module F(X : sig end) = X
module App = F(M)
[%%expect{|
{
 "M"[module] -> {<.11>};
 }
module M : sig end
{
 "F"[module] -> Abs<.13>(X, X<.12>);
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.14>};
 }
module App : sig end
|}]

module Id(X : sig end) = X
module Struct = struct
  module L = List
end
[%%expect{|
{
 "Id"[module] -> Abs<.16>(X, X<.15>);
 }
module Id : functor (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.18>
    "L"[module] -> Alias(<.17>
                         CU Stdlib . "List"[module]);
    };
 }
module Struct : sig module L = List end
|}]

module App = Id(List) (* this should have the App uid *)
module Proj = Struct.L
  (* this should have the Proj uid and be an alias to Struct.L *)
[%%expect{|
{
 "App"[module] -> (CU Stdlib . "List"[module])<.19>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.20>
                         Alias(<.17>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}]

module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
[%%expect{|
{
 "F"[module] -> Abs<.23>(X, {
                             "M"[module] -> X<.21>;
                             });
 }
module F : functor (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.24>
                 "M"[module] -> {<.21>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.25>
                      {<.21>});
 }
module O = N.M
|}]
