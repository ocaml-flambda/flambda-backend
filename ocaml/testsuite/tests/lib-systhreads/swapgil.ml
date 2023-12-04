(* TEST
modules = "swapgil_stubs.c"
* runtime5
** skip
* runtime4
** hassysthreads
include systhreads
*** hasunix
**** native
*)

external setup : unit -> unit = "swap_gil_setup"
let () = setup ()

let counter = ref 0

external blocking_section : unit -> unit = "blocking_section"

type c_thread
external create_c_thread : (unit -> unit) -> c_thread = "create_c_thread"
external join_c_thread : c_thread -> unit = "join_c_thread"

external swap_gil : unit -> unit = "swap_gil"

let threadfn () =
  for i = 1 to 1_000 do
    incr counter;
    let junk = Sys.opaque_identity (ref !counter) in
    ignore junk;
    match i mod 100, i mod 10 with
    | _, 0 -> Thread.yield ()
    | _, 1 -> blocking_section ()
    | 22, _ -> Gc.minor ()
    | _, 3 -> swap_gil ()
    | _ -> ()
  done

let () =
  let open Either in
  let threads =
    List.init 40 (fun i ->
      if i land 1 = 0 then
        Left (Thread.create threadfn ())
      else
        Right (create_c_thread threadfn))
  in
  List.iter (function Left th -> Thread.join th | Right ct -> join_c_thread ct) threads;
  Printf.printf "%d\n" !counter
