let[@inline never] make n = List.init n Fun.id

exception Exn of int
let[@inline never] break n =
  raise (Exn n)

let[@inline never] check_backtrace n =
  try
    break n
  with
  | Exn i ->
    let raw_backtrace = Printexc.get_raw_backtrace () in
    let len = Printexc.raw_backtrace_length raw_backtrace in
    assert (len > 0);
    let slot = Printexc.get_raw_backtrace_slot raw_backtrace 0
               |> Printexc.convert_raw_backtrace_slot  in
    let name = Printexc.Slot.format 0 slot |> Option.get in
    Printf.printf "i=%d name=%s\n" i name;
    ()

let test n =
  let l = make n in
  (*$
    for i = 1 to Sys.opaque_identity 20 do
      Printf.printf "let a%d = Sys.opaque_identity 1 in\n" i
    done;
  *)let a1 = Sys.opaque_identity 1 in
let a2 = Sys.opaque_identity 1 in
let a3 = Sys.opaque_identity 1 in
let a4 = Sys.opaque_identity 1 in
let a5 = Sys.opaque_identity 1 in
let a6 = Sys.opaque_identity 1 in
let a7 = Sys.opaque_identity 1 in
let a8 = Sys.opaque_identity 1 in
let a9 = Sys.opaque_identity 1 in
let a10 = Sys.opaque_identity 1 in
let a11 = Sys.opaque_identity 1 in
let a12 = Sys.opaque_identity 1 in
let a13 = Sys.opaque_identity 1 in
let a14 = Sys.opaque_identity 1 in
let a15 = Sys.opaque_identity 1 in
let a16 = Sys.opaque_identity 1 in
let a17 = Sys.opaque_identity 1 in
let a18 = Sys.opaque_identity 1 in
let a19 = Sys.opaque_identity 1 in
let a20 = Sys.opaque_identity 1 in
(*$*)
  check_backtrace n;
  let l = make (List.length l) in
  Gc.compact ();
  [
    (*$
    for i = 1 to Sys.opaque_identity 20 do
      Printf.printf "a%d;\n" i
    done;
  *)a1;
a2;
a3;
a4;
a5;
a6;
a7;
a8;
a9;
a10;
a11;
a12;
a13;
a14;
a15;
a16;
a17;
a18;
a19;
a20;
(*$*)
  ]@l
  |> Sys.opaque_identity


let () =
  Printexc.record_backtrace true;
  10_000
  |> Sys.opaque_identity
  |> test
  |> ignore
