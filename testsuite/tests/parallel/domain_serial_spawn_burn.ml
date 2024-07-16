(* TEST
<<<<<<< HEAD
 reason = "CR ocaml 5 domains: re-enable this test";
 skip;
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
||||||| 121bedcfd2
* hasunix
include unix
** bytecode
** native
=======
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
>>>>>>> 5.2.0
*)

open Domain

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 0


(* This test looks to spawn domains while doing a bunch of explicit minor and major GC calls
   from parallel domains *)

(* Don't run the test if we have only 2 cores available, it times out often. *)

let list_size =
  if test_size < 2 then begin print_endline "ok"; exit 0 end
  else if test_size = 2 then 14
  else 15

let rec burn l =
  if List.hd l > test_size then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let test_serial_domain_spawn () =
  for i = 1 to 250 do
    let d = Domain.spawn (fun () -> burn [0]) in
    join d
  done

let () =
  let running = Atomic.make true in
  let rec run_until_stop fn () =
    while Atomic.get running do
      fn ();
    done
  in

  let domain_minor_gc = Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.minor ())) in
  let domain_major_gc = Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.major ())) in

  test_serial_domain_spawn ();

  Atomic.set running false;
  join domain_minor_gc;
  join domain_major_gc;

  print_endline "ok"
