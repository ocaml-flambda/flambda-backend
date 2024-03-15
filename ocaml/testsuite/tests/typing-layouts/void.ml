(* TEST
   * expect
   * expect
   flags = "-extension layouts_beta"
*)

(* CR layouts: all tests from this file moved to [void_alpha.ml].  Move back
   here when the stuff from v5 no longer needs extension flags. *)
type t_void : void;;
[%%expect {|
Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]
