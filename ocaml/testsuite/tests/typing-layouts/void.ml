(* TEST
   * expect
*)

(* CR layouts: all tests from this file moved to [void_alpha.ml].  Move back
   here when the stuff from v5 no longer needs extension flags. *)
type t_void : void;;
[%%expect {|
Line 1, characters 0-18:
1 | type t_void : void;;
    ^^^^^^^^^^^^^^^^^^
Error: The extension "layouts" is disabled and cannot be used
|}]
