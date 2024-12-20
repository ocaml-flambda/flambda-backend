(* TEST
   include stdlib_beta;
   modules = "test_smallint.ml";
   flags = "-extension small_numbers_beta";
*)

let () =
  Test_smallint.run
    (module Stdlib_beta.Int16)
    ~min_int:(-0x8000)
    ~max_int:0x7fff
