(* TEST
 flags = "-extension-universe alpha";
 include stdlib_upstream_compatible;
 include stdlib_stable;
 include stdlib_beta;
 include stdlib_alpha;
 {
   bytecode;
 }{
   native;
 }
*)

(* Check that extension library modules exist. *)
module Upstream_compatible = Stdlib_upstream_compatible
module Stable = Stdlib_stable
module Beta = Stdlib_beta
module Alpha = Stdlib_alpha
