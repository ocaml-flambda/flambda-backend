(* TEST
   flags = "-dlambda";
   expect;
*)

let _ = [: [: :] :];;

[%%expect {|
(makearray_imm[gen] (makearray_imm[gen]))
- : 'a iarray iarray = [:[::]:]
|}]

