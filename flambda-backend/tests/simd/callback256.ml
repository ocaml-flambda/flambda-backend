open Stdlib

external run_callback : (unit -> unit) -> unit = "" "vec128_run_callback"

external run_callback_stack_args :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  (int -> int -> int -> int -> int -> int -> int -> int -> unit) ->
  unit = "" "vec128_run_callback_stack_args"

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external lots_of_vectors :
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 = "" "lots_of_vectors256"
  [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d =
  let v1, v2, v3, v4 =
    ( int64x4_first_int64 v,
      int64x4_second_int64 v,
      int64x4_third_int64 v,
      int64x4_fourth_int64 v )
  in
  eq v1 a;
  eq v2 b;
  eq v3 c;
  eq v4 d

let callback () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let v6 = int64x4_of_int64s 25L 26L 27L 28L in
  let v7 = int64x4_of_int64s 29L 30L 31L 32L in
  let v8 = int64x4_of_int64s 33L 34L 35L 36L in
  let v9 = int64x4_of_int64s 37L 38L 39L 40L in
  let v10 = int64x4_of_int64s 41L 42L 43L 44L in
  let v11 = int64x4_of_int64s 45L 46L 47L 48L in
  let v12 = int64x4_of_int64s 49L 50L 51L 52L in
  let v13 = int64x4_of_int64s 53L 54L 55L 56L in
  let v14 = int64x4_of_int64s 57L 58L 59L 60L in
  let v15 = int64x4_of_int64s 61L 62L 63L 64L in
  let sum =
    lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  in
  check sum 496L 512L 528L 544L

let callback_n i0 i1 i2 i3 i4 i5 i6 i7 =
  assert (
    i0 = 0 && i1 = 1 && i2 = 2 && i3 = 3 && i4 = 4 && i5 = 5 && i6 = 6 && i7 = 7);
  callback ()

(* Previously failing tests *)

let () = run_callback callback

let () = run_callback_stack_args 0 1 2 3 4 5 6 7 callback_n

let () =
  let[@inline never] finalizer () =
    let x = ref () in
    Gc.finalise (fun _ -> callback ()) x
  in
  finalizer ();
  Gc.full_major ()

let () = Sys.with_async_exns callback

(* Additional checks *)

let () = callback ()

let () =
  try Sys.with_async_exns (fun () -> raise Sys.Break) with
  | Sys.Break -> callback ()
  | _ -> assert false

let[@loop never] rec stack_overflow () = (stack_overflow () [@nontail])

let () =
  try Sys.with_async_exns stack_overflow with
  | Stack_overflow -> callback ()
  | _ -> assert false

(* Effects *)

module Effect = Stdlib__Effect

type _ Effect.t += E : unit Effect.t

let eff0 () =
  try
    Effect.Deep.try_with
      (fun () -> Effect.perform E)
      ()
      { effc = (fun (type a) (_ : a Effect.t) -> None) }
  with Effect.Unhandled E -> callback ()

let eff1 () =
  Effect.Deep.try_with
    (fun () -> callback ())
    ()
    { effc = (fun (type a) (_ : a Effect.t) -> None) };
  callback ()

let eff2 () =
  Effect.Deep.try_with
    (fun () -> Effect.perform E)
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E ->
            callback ();
            Some (fun (_ : (a, unit) Effect.Deep.continuation) -> callback ())
          | _ -> None)
    };
  callback ()

let eff3 () =
  Effect.Deep.try_with
    (fun () ->
      callback ();
      Effect.perform E;
      callback ())
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E ->
            Some
              (fun (k : (a, unit) Effect.Deep.continuation) ->
                callback ();
                Effect.Deep.continue k ())
          | _ -> None)
    };
  callback ()

let eff4 () =
  Effect.Deep.match_with
    (fun () -> ())
    ()
    { retc = (fun () -> callback ());
      exnc = (fun _ -> ());
      effc = (fun (type a) (_ : a Effect.t) -> None)
    };
  callback ()

let eff5 () =
  Effect.Deep.match_with
    (fun () -> assert false)
    ()
    { retc = (fun () -> assert false);
      exnc = (fun _ -> callback ());
      effc = (fun (type a) (_ : a Effect.t) -> None)
    };
  callback ()

type _ Effect.t += E2 : unit Effect.t

let eff6 () =
  try
    Effect.Deep.match_with
      (fun () ->
        callback ();
        Effect.Deep.try_with
          (fun () ->
            callback ();
            Effect.perform E2;
            assert false)
          ()
          { effc =
              (fun (type a) (_ : a Effect.t) ->
                callback ();
                Effect.perform E;
                callback ();
                None)
          })
      ()
      { retc = (fun () -> assert false);
        exnc =
          (function
          | Effect.Unhandled E2 ->
            callback ();
            Effect.perform E
          | _ -> assert false);
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | E ->
              Some
                (fun (k : (a, unit) Effect.Deep.continuation) ->
                  callback ();
                  Effect.Deep.continue k ())
            | _ -> None)
      }
  with Effect.Unhandled E ->
    callback ();
    callback ()

external runtime5 : unit -> bool = "%runtime5"

let () =
  if runtime5 ()
  then (
    eff0 ();
    eff1 ();
    eff2 ();
    eff3 ();
    eff4 ();
    eff5 ();
    eff6 ();
    run_callback eff0;
    run_callback eff1;
    run_callback eff2;
    run_callback eff3;
    run_callback eff4;
    run_callback eff5;
    run_callback eff6)
  else ()
