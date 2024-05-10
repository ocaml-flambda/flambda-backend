(* TEST
 include unix;
 libunix;
 expect;
*)

(* Test that [include] works in expect tests: the Unix module is not
   available by default. *)
Unix.sleepf 0.01;;
[%%expect {|
- : unit = ()
|}]
