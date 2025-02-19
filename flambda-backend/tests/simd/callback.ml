open Stdlib

external run_callback : (unit -> unit) -> unit = "" "vec128_run_callback"

external run_callback_stack_args :
  int -> int -> int -> int -> int -> int -> int -> int ->
  (int -> int -> int -> int -> int -> int -> int -> int -> unit) -> unit = "" "vec128_run_callback_stack_args"

external low_of : float32 -> float32x4 = "caml_vec128_unreachable" "caml_float32x4_low_of_float32"
  [@@noalloc] [@@unboxed] [@@builtin]
external low_to : float32x4 -> float32 = "caml_vec128_unreachable" "caml_float32x4_low_to_float32"
  [@@noalloc] [@@unboxed] [@@builtin]

external add : float32x4 -> float32x4 -> float32x4 = "caml_vec128_unreachable" "caml_sse_float32x4_add"
  [@@noalloc] [@@unboxed] [@@builtin]

let callback () =
  let x0 = low_of 0.0s in
  let x1 = low_of 1.0s in
  let x2 = low_of 2.0s in
  let x3 = low_of 3.0s in
  let x4 = low_of 4.0s in
  let x5 = low_of 5.0s in
  let x6 = low_of 6.0s in
  let x7 = low_of 7.0s in
  let x8 = low_of 8.0s in
  let x9 = low_of 9.0s in
  let x10 = low_of 10.0s in
  let x11 = low_of 11.0s in
  let x12 = low_of 12.0s in
  let x13 = low_of 13.0s in
  let x14 = low_of 14.0s in
  let x15 = low_of 15.0s in
  let x16 = low_of 16.0s in
  let sum = add x16
            (add (add (add (add x0 x1) (add x2 x3)) (add (add x4 x5) (add x6 x7)))
                (add (add (add x8 x9) (add x10 x11)) (add (add x12 x13) (add x14 x15)))) in
  assert (low_to sum = 136.s)

let callback_n i0 i1 i2 i3 i4 i5 i6 i7 =
  assert (i0 = 0 && i1 = 1 && i2 = 2 && i3 = 3 && i4 = 4 && i5 = 5 && i6 = 6 && i7 = 7);
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

let[@loop never] rec stack_overflow () = stack_overflow () [@nontail]

let () =
  try Sys.with_async_exns stack_overflow with
  | Stack_overflow -> callback ()
  | _ -> assert false

(* Effects *)

module Effect = Stdlib__Effect

type _ Effect.t += E : unit Effect.t

let eff0 () =
  try Effect.Deep.try_with (fun () -> Effect.perform E) ()
    { effc = (fun (type a) (e : a Effect.t) -> None) }
  with Effect.Unhandled E -> callback ()

let eff1 () =
  Effect.Deep.try_with (fun () -> callback ()) ()
    { effc = (fun (type a) (e : a Effect.t) -> None) };
  callback ()

let eff2 () =
  Effect.Deep.try_with (fun () -> Effect.perform E) ()
    { effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | E -> callback ();
          Some (fun (k : (a, unit) Effect.Deep.continuation) -> callback ())
        | _ -> None) };
  callback ()

let eff3 () =
  Effect.Deep.try_with (fun () -> callback (); Effect.perform E; callback ()) ()
  { effc = (fun (type a) (e : a Effect.t) ->
      match e with
      | E -> Some (fun (k : (a, unit) Effect.Deep.continuation) ->
          callback (); Effect.Deep.continue k ())
      | _ -> None) };
  callback ()

let eff4 () =
  Effect.Deep.match_with (fun () -> ()) ()
    { retc = (fun () -> callback ())
    ; exnc = (fun _ -> ())
    ; effc = (fun (type a) (e : a Effect.t) -> None)
    };
  callback ()

let eff5 () =
  Effect.Deep.match_with (fun () -> assert false) ()
    { retc = (fun () -> assert false)
    ; exnc = (fun _ -> callback ())
    ; effc = (fun (type a) (e : a Effect.t) -> None)
    };
  callback ()

type _ Effect.t += E2 : unit Effect.t

let eff6 () =
  try Effect.Deep.match_with (fun () ->
    callback ();
    Effect.Deep.try_with (fun () -> callback (); Effect.perform E2; assert false) ()
          { effc = (fun (type a) (e : a Effect.t) -> None) }) ()
    { retc = (fun () -> callback (); Effect.perform E; assert false)
    ; exnc = (function
              | Effect.Unhandled E2 -> callback (); Effect.perform E
              | _ -> assert false)
    ; effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | E -> Some (fun (k : (a, unit) Effect.Deep.continuation) ->
          callback (); Effect.Deep.continue k ())
        | _ -> None)
    };
  with Effect.Unhandled E -> callback ();
  callback ()

let () = eff0 ()
let () = eff1 ()
let () = eff2 ()
let () = eff3 ()
let () = eff4 ()
let () = eff5 ()
let () = eff6 ()

let () = run_callback eff0
let () = run_callback eff1
let () = run_callback eff2
let () = run_callback eff3
let () = run_callback eff4
let () = run_callback eff5
let () = run_callback eff6
