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

(* Check that old names for the libraries still work. *)
module Upstream_compatible = Upstream_compatible
module Stable = Stable
module Beta = Beta
module Alpha = Alpha
