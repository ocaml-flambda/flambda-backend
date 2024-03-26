(* TEST
   flags = "-extension layouts_alpha"

   * runtime4
   ** expect
*)

(* Mixed blocks aren't supported in runtime 4 (yet). When they are, we can
   remove this test and remove the preludes from other tests that run them for
   just runtime 5.
*)

type t =
  { x : float;
    y : float#;
  }

[%%expect{|
Lines 1-4, characters 0-3:
1 | type t =
2 |   { x : float;
3 |     y : float#;
4 |   }
Error: This OCaml runtime doesn't support mixed records. Contact Jane Street compiler devs if you see this error.
|}]

type t =
  { x : string;
    y : float#;
  }

[%%expect{|
Lines 1-4, characters 0-3:
1 | type t =
2 |   { x : string;
3 |     y : float#;
4 |   }
Error: This OCaml runtime doesn't support mixed records. Contact Jane Street compiler devs if you see this error.
|}]
