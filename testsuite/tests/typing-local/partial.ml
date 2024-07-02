(* TEST
 flags += " -g";
*)

let f1 ~a ~b ~c ~d ~e = a + b + c + d + e
let f2 ~b ~c ~e = f1 ~b ~c ~e
let f3 = f2 ~a:1
let f4 () = f3 ~b:2 ~c:3 ~d:4
let () = Printf.printf "%d\n" (f4 () ~e:5)


(* the mutable pattern causes this function to split
   into two functions *)
let next {contents} a b = contents a b

(* nonetheless, this should still be a tail call *)
let[@inline never] app c d e = next c d e

let backtrace () () =
  let open Printexc in
  get_callstack 1000
  |> backtrace_slots
  |> Option.value ~default:[||]
  |> Array.map Slot.name
  |> Array.map (Option.value ~default:"??")

let test () =
  let slots = app (ref backtrace) () () in
  Array.iteri (fun i s -> Printf.printf "%s%s" (if i > 0 then "; " else "") s) slots;
  Printf.printf "\n%!"

let () = test ()

let[@inline never] rec spin ({contents} as r) a b =
  if b = 0 then contents + a
  else (Sys.opaque_identity spin) r a (b-1)

let () = Printf.printf "%d\n" (spin (ref 1) 1 1_000_000)
