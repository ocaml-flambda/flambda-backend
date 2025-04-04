(* TEST
   include runtime_events;
*)
open Runtime_events

(* Compaction runs in several phases: evacuation, forwarding, and
releasing.

With the OCaml 5.2 compaction
algorithm (OCAMLRUNPARAM=-Xcompaction=52), there is exactly one of
each phase, in that order, in each compaction.

With the chunk compactor, the phases are:

  evacuate,
  forward,
  release
  evacuate,
  [ if we run phase two:
    forward,
    release,
  ]
  release

So "forward" follows "evacuate"
"evacuate" follows none or "release"
"release" follows some other phase

*)

type state =
  | OUTSIDE             (* not compacting *)
  | INSIDE              (* between compaction phases *)
  | EVACUATING          (* in evacuation phase *)
  | FORWARDING          (* in forwarding (fixing) phase *)
  | RELEASING           (* in releasing phase *)

type phase_completed =
  | NONE
  | EVACUATE
  | FORWARD
  | RELEASE

let state = ref OUTSIDE
let last = ref NONE

let () =
    start ();
    let cursor = create_cursor None in
    let runtime_begin domain_id ts phase =
      match phase with
      | EV_COMPACT ->
        begin
          assert (!state = OUTSIDE);
          assert (!last = NONE);
          state := INSIDE
        end
      | EV_COMPACT_EVACUATE -> begin
          assert (!state = INSIDE);
          assert ((!last = NONE) || (!last = RELEASE));
          state := EVACUATING
        end
      | EV_COMPACT_FORWARD -> begin
          assert (!state = INSIDE);
          assert (!last = EVACUATE);
          state := FORWARDING
        end
      | EV_COMPACT_RELEASE -> begin
          assert (!state = INSIDE);
          assert (!last != NONE);
          state := RELEASING
        end
      | _ -> () in
      let runtime_end domain_id ts phase =
        match phase with
        | EV_COMPACT ->
          begin
            assert (!state = INSIDE);
            assert (!last = RELEASE);
            state := OUTSIDE;
            last := NONE
          end
        | EV_COMPACT_EVACUATE -> begin
            assert (!state = EVACUATING);
            state := INSIDE;
            last := EVACUATE
          end
        | EV_COMPACT_FORWARD -> begin
            assert (!state = FORWARDING);
            state := INSIDE;
            last := FORWARD
          end
        | EV_COMPACT_RELEASE -> begin
            assert (!state = RELEASING);
            state := INSIDE;
            last := RELEASE
          end
        | _ -> ()
        in
    let callbacks = Callbacks.create ~runtime_begin ~runtime_end ()
    in
    Gc.compact ();
    ignore(read_poll cursor callbacks (Some 1_000));
    assert(!state = OUTSIDE)
