(* TEST
 flags += "-alert -unsafe_parallelism";
 runtime5;
 { bytecode; }
 { native; }
*)

type u = U of unit
let () =
  (* See https://github.com/ocaml-multicore/ocaml-multicore/issues/252 *)
  let make_cell (x : unit) : u Atomic.t =
    let cell = Atomic.make (U x) in
    Atomic.set cell (U x) ;
    cell in
  (* the error shows up with an array of length 256 or larger *)
  let a = Array.make 256 (make_cell ()) in
  ignore (Sys.opaque_identity a)


let test_fetch_add () =
  let ndoms = 4 in
  let count = 10000 in
  let arr = Array.make (ndoms * count) (-1) in
  let step = 1493 in
  let r = Atomic.make 0 in
  (* step is relatively prime to Array.length arr *)
  let loop () =
    let self = (Domain.self () :> int) in
    for i = 1 to count do
      let n = Atomic.fetch_and_add r step mod Array.length arr in
      assert (arr.(n) == (-1));
      arr.(n) <- self
    done in
  let _ = Array.init 4 (fun i ->
      Domain.spawn loop)
      |> Array.map Domain.join in
  assert (Array.for_all (fun x -> x >= 0) arr)

let () =
  test_fetch_add ();
  print_endline "ok"


let test_fetch_land () =
  let n = Atomic.make max_int in
  let clear_bit b = Atomic.fetch_and_land n (lnot (1 lsl b)) in
  let loop i () =
    for b = i to (i + 16) do
      if b <> 64 then ignore (clear_bit b)
    done
  in
  let _ = Array.init 4 (fun i ->
      Domain.spawn (loop i))
      |> Array.map Domain.join in
  print_string "CURRENT VALUE: ";
  print_int (Atomic.get n);
  print_endline "";

  assert (Atomic.get n = 0)

let () =
  test_fetch_land ();
  print_endline "ok"



let test v =
  let open Atomic in
  assert (get v = 42);
  set v 10;
  assert (get v = 10);
  let b = compare_and_set v 11 20 in
  assert (b = false);
  assert (get v = 10);
  let b = compare_and_set v 10 20 in
  assert (b = true);
  assert (get v = 20)

let () =
  let r = Atomic.make 42 in
  test r;
  Atomic.set r 42;
  Gc.full_major ();
  test r;
  print_endline "ok"
