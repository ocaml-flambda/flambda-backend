(* TEST
 flags = "-dshape -extension layouts_beta";
 expect;
*)

type t = #{ a : int; b : string }
[%%expect{|
{
 "t"[type] -> {<.0>
               "a"[unboxed label] -> <.1>;
               "b"[unboxed label] -> <.2>;
               };
 }
type t = #{ a : int; b : string; }
|}]
