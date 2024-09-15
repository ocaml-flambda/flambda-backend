open Effect
open Effect.Deep

type _ Effect.t += Xchg: int -> int t

external opaque : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

let[@inline never] rand () = Random.int 42

external local_stack_offset : unit -> int = "caml_local_stack_offset"
let local_stack_offset () = local_stack_offset () / (Sys.word_size / 8)

let initial_stack_offset = local_stack_offset ()

let[@inline never] expect_local_stack_offset expected =
  let now = local_stack_offset () in
  if now <> initial_stack_offset + expected then (
    Printf.eprintf "Callstack:\n%s\n"
      (Printexc.get_callstack 100 |> Printexc.raw_backtrace_to_string);
    failwith (Printf.sprintf "local stack offset mismatch (in words): \
        initial = %d, now = %d, expected = %d"
      initial_stack_offset now expected)
  )

(* -------------- Simple tests of the local stack offset ------------------- *)

(* When a fiber stack is created, its accompanying local stack is empty,
   even if the parent fiber stack's local stack is not. *)
let[@inline never] f1 () =
  let (_ : int * int) = opaque (local_ (rand (), rand ())) in
  expect_local_stack_offset 3;
  let result =
    try_with (fun () ->
        (* This is on a new fiber stack *)
        expect_local_stack_offset 0;
        42
      )
      ()
      { effc = (fun (type a) (_eff: a t) -> None) }
  in
  assert (result = 42)

(* When a fiber stack is created, local allocation from the corresponding
   fiber does not touch the local stack of the parent. *)
let[@inline never] f2 () =
  let (_ : int * int) = opaque (local_ (rand (), rand ())) in
  expect_local_stack_offset 3;
  let result =
    try_with (fun () ->
        let (_ : int * int * int) =
          opaque (local_ (rand (), rand (), rand ()))
        in
        expect_local_stack_offset 4;
        42
      )
      ()
      { effc = (fun (type a) (_eff: a t) -> None) }
  in
  (* The allocation of the triple should not have affected the local stack
     offset here. *)
  expect_local_stack_offset 3;
  assert (result = 42)

(* As for f2, but with the check for the parent's local stack in an
   effect handler.  In addition we check that when the [perform] returns,
   the fiber's local stack offset is unaffected. *)
type _ Effect.t += Eff_f3 : unit -> unit t
let[@inline never] f3 () =
  let (_ : int * int) = opaque (local_ (rand (), rand ())) in
  expect_local_stack_offset 3;
  let result =
    try_with (fun () ->
        let (_ : int * int * int) =
          opaque (local_ (rand (), rand (), rand ()))
        in
        expect_local_stack_offset 4;
        perform (Eff_f3 ());
        expect_local_stack_offset 4;
        42
      )
      ()
      { effc = (fun (type a) (eff: a t) ->
          match eff with
          | Eff_f3 () ->
            (* This is like the check on the penultimate line of [f3] below *)
            expect_local_stack_offset 3;
            Some (fun (k: (a, _) continuation) ->
              (* Ditto *)
              expect_local_stack_offset 3;
              continue k ())
          | _ -> assert false)
      }
  in
  (* The allocation of the triple should not have affected the local stack
     offset here. *)
  expect_local_stack_offset 3;
  assert (result = 42)

(* Like f3, except the effect handler raises an exception that is not caught
   in the fiber's computation. *)
type _ Effect.t += Eff_f4 : unit -> unit t
exception Exn_f4
let[@inline never] f4 () =
  let (p : int * int) = opaque (local_ (rand (), rand ())) in
  expect_local_stack_offset 3;
  match
    try_with (fun () ->
        let (_ : int * int * int) =
          opaque (local_ (rand (), rand (), rand ()))
        in
        expect_local_stack_offset 4;
        perform (Eff_f4 ());
        assert false
      )
      ()
      { effc = (fun (type a) (eff: a t) ->
          match eff with
          | Eff_f4 () ->
            expect_local_stack_offset 3;
            Some (fun (k: (a, _) continuation) ->
              (* Ditto *)
              expect_local_stack_offset 3;
              raise Exn_f4)
          | _ -> assert false)
      }
  with
  | exception Exn_f4 ->
    expect_local_stack_offset 3;
    (* Ensure that the region for [p] extends for long enough: *)
    let (_ : int) = opaque (fst p) in
    ()
  | _ -> assert false

(* Like f4, except that the exception is raised directly in the fiber's
   computation. *)
type _ Effect.t += Eff_f5 : unit -> unit t
exception Exn_f5
let[@inline never] f5 () =
  let (p : int * int) = opaque (local_ (rand (), rand ())) in
  expect_local_stack_offset 3;
  match
    try_with (fun () ->
        let (_ : int * int * int) =
          opaque (local_ (rand (), rand (), rand ()))
        in
        expect_local_stack_offset 4;
        perform (Eff_f5 ());
        expect_local_stack_offset 4;
        raise Exn_f5
      )
      ()
      { effc = (fun (type a) (eff: a t) ->
          match eff with
          | Eff_f5 () ->
            expect_local_stack_offset 3;
            Some (fun (k: (a, _) continuation) ->
              (* Ditto *)
              expect_local_stack_offset 3;
              continue k ())
          | _ -> assert false)
      }
  with
  | exception Exn_f5 ->
    expect_local_stack_offset 3;
    let (_ : int) = opaque (fst p) in
    ()
  | _ -> assert false

(* ------ Tests to make sure local allocations are scanned by the GC ------ *)

let[@inline never] g1_local_alloc x y =
  Gc.compact ();
  let in_minor = opaque (opaque x, opaque y) in
  Gc.full_major (); (* XXX *)
  local_ (opaque (opaque in_minor, opaque in_minor))

let g1_comp () =
  let p = g1_local_alloc 1 2 in
  perform (Xchg (fst (fst (opaque p))))
    + (opaque (
        let q = opaque (local_ (opaque 100, opaque 200)) in
        fst q
      ))
    + perform (Xchg (snd (snd (opaque p))))

let[@inline never] g1' () =
    try_with g1_comp ()
    { effc = fun (type a) (eff: a t) ->
        Gc.compact ();
        match eff with
        | Xchg n -> Some (fun (k: (a, _) continuation) ->
            Gc.compact ();
            let q = opaque (local_ (opaque 10, opaque 20)) in
            continue k (n + fst q))
        | _ -> None
    }

let g1 () = assert (g1' () = 123)

(* ------------------------------------------------------------------------ *)

let () =
  print_endline "f1"; f1 ();
  print_endline "f2"; f2 ();
  print_endline "f3"; f3 ();
  print_endline "f4"; f4 ();
  print_endline "f5"; f5 ();
  print_endline "g1"; g1 ()
