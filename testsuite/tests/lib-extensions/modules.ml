(* TEST
 flags = "-extension-universe alpha";
 include upstream_compatible;
 include stable;
 include beta;
 include alpha;
 {
   bytecode;
 }{
   native;
 }
*)

(* Check that extension library modules exist. *)
module Upstream_compatible = Upstream_compatible
module Stable = Stable
module Beta = Beta
module Alpha = Alpha
