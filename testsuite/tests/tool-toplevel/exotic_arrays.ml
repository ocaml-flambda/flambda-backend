(* TEST
  toplevel.opt;
*)

let y = [| 1.0; 2.0; 3.0; 4.0 |];;

let y = [| #1.0; #2.0; #3.0; #4.0 |];;

let y = [| #1l; #2l; #3l; #4l |];;

let y = [| #1n; #2n; #3n; #4n |];;

let y = [| #1L; #2L; #3L; #4L |];;
