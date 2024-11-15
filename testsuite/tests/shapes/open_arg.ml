(* TEST
 flags = "-dshape";
 expect;
*)

module type Make = functor (I : sig end) -> sig
  open I
end
;;

[%%expect{|
{
 "Make"[module type] -> <.3>;
 }
module type Make = functor (I : sig end) -> sig end
|}]

module Make (I : sig end) : sig
  open I
end = struct end
;;

[%%expect{|
{
 "Make"[module] -> Abs<.5>(I, {});
 }
module Make : functor (I : sig end) -> sig end
|}]

module type Make = functor (I : sig end) ->
module type of struct
  open I
end

[%%expect{|
{
 "Make"[module type] -> <.7>;
 }
module type Make = functor (I : sig end) -> sig end
|}]
