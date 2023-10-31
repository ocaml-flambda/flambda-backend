(* TEST
   flags = "-extension layouts_beta"
   * expect
*)

(* CR layouts v5: all tests from this file moved to [void_alpha.ml].  Move back
   here. *)
type t_void : void;;
[%%expect {|
Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}]
