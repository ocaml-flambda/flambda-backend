(* TEST
<<<<<<< HEAD
 ocamlrunparam += "l=100000";
||||||| 121bedcfd2

ocamlrunparam += "l=100000"
=======
 ocamlrunparam += "l=100000";
 no-tsan; (* TSan does not support call stacks bigger than 64k frames *)
 {
   bytecode;
 }
 {
   native;
 }
>>>>>>> 5.2.0
*)

let rec f x =
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in
  let _ = f x in
  ()

let _ =
  try Sys.with_async_exns (fun () -> f 1)
  with Stack_overflow -> Printf.printf "OK\n"