# 2 "domain.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

external runtime5 : unit -> bool @@ portable = "%runtime5"

exception Encapsulated of string

module Runtime_4 = struct
  module DLS = struct
    module Access = struct
      type t = Access
      let for_initial_domain = Access
    end

    let[@inline] access f =
      try f Access.Access with
      | exn ->
        let bt = Printexc.get_raw_backtrace () in
        let exn_string = Printexc.to_string exn in
        Printexc.raise_with_backtrace (Encapsulated exn_string) bt

    let unique_value = Obj.magic_portable (Obj.repr (ref 0))
    let state = Obj.magic_portable (ref (Array.make 8 unique_value))

    let init () = ()

    type 'a key : value mod portable contended = Key of (int * (Access.t -> 'a))
    [@@unboxed]
    [@@unsafe_allow_any_mode_crossing "runtime4 only"]

    let key_counter = Obj.magic_portable (ref 0)

    let new_key' ?split_from_parent:_ init_orphan =
      let key_counter = Obj.magic_uncontended key_counter in
      let idx = !key_counter in
      key_counter := idx + 1;
      Key (idx, init_orphan)

    (* If necessary, grow the current domain's local state array such that [idx]
    * is a valid index in the array. *)
    let maybe_grow idx =
      let state = Obj.magic_uncontended state in
      let st = !state in
      let sz = Array.length st in
      if idx < sz then st
      else begin
        let rec compute_new_size s =
          if idx < s then s else compute_new_size (2 * s)
        in
        let new_sz = compute_new_size sz in
        let new_st = Array.make new_sz (Obj.magic_uncontended unique_value) in
        Array.blit st 0 new_st 0 sz;
        state := new_st;
        new_st
      end

    let set (_ : Access.t) (Key (idx, _init)) x =
      let st = maybe_grow idx in
      (* [Sys.opaque_identity] ensures that flambda does not look at the type of
      * [x], which may be a [float] and conclude that the [st] is a float array.
      * We do not want OCaml's float array optimisation kicking in here. *)
      st.(idx) <- Obj.repr (Sys.opaque_identity x)

    let get access (Key (idx, init)) =
      let st = maybe_grow idx in
      let v = st.(idx) in
      if v == Obj.magic_uncontended unique_value then
        let v' = Obj.repr (init access) in
        st.(idx) <- (Sys.opaque_identity v');
        Obj.magic v'
      else Obj.magic v
  end

  (******** Callbacks **********)

  (* first spawn, domain startup and at exit functionality *)
  let first_domain_spawned = Atomic.make false

  let first_spawn_function = ref (fun () -> ())

  let before_first_spawn f =
    if Atomic.Contended.get first_domain_spawned then
      raise (Invalid_argument "first domain already spawned")
    else begin
      let old_f = !first_spawn_function in
      let new_f () = old_f (); f () in
      first_spawn_function := new_f
    end

  let at_exit_key = DLS.new_key' (fun (_ : DLS.Access.t) -> (fun () -> ()))

  let at_exit' access f =
    let old_exit : unit -> unit = DLS.get access at_exit_key in
    let new_exit () =
      (* The domain termination callbacks ([at_exit]) are run in
        last-in-first-out (LIFO) order in order to be symmetric with the domain
        creation callbacks ([at_each_spawn]) which run in first-in-fisrt-out
        (FIFO) order. *)
      f (); old_exit ()
    in
    DLS.set access at_exit_key new_exit

  let do_at_exit () =
    let f : unit -> unit = DLS.get DLS.Access.for_initial_domain at_exit_key in
    f ()

  (* Unimplemented functions *)
  let not_implemented () =
    failwith "Multi-domain functionality not supported in runtime4"
  type !'a t : value mod contended portable
  type id = int
  let spawn' _ = not_implemented ()
  let join _ = not_implemented ()
  let get_id _ = not_implemented ()
  let self () = not_implemented ()
  let cpu_relax () = not_implemented ()
  let is_main_domain () = not_implemented ()
  let recommended_domain_count () = not_implemented ()
end

module Runtime_5 = struct
  module Raw = struct
    (* Low-level primitives provided by the runtime *)
    type t = private int

    (* The layouts of [state] and [term_sync] are hard-coded in
      [runtime/domain.c] *)

    type 'a state =
      | Running
      | Finished of ('a, exn) result [@warning "-unused-constructor"]

    type 'a term_sync : value mod contended portable = {
      (* protected by [mut] *)
      mutable state : 'a state [@warning "-unused-field"] ;
      mut : Mutex.t ;
      cond : Condition.t ;
    }
    [@@unsafe_allow_any_mode_crossing "mutable state is protected by mutex"]

    external spawn : (unit -> 'a) @ portable -> 'a term_sync -> t @@ portable
      = "caml_domain_spawn"
    external self : unit -> t @@ portable
      = "caml_ml_domain_id" [@@noalloc]
    external cpu_relax : unit -> unit @@ portable
      = "caml_ml_domain_cpu_relax"
    external get_recommended_domain_count: unit -> int @@ portable
      = "caml_recommended_domain_count" [@@noalloc]
  end

  let cpu_relax () = Raw.cpu_relax ()

  type id = Raw.t

  type 'a t = {
    domain : Raw.t;
    term_sync : 'a Raw.term_sync;
  }

  module DLS = struct

    module Access = struct
      type t = Access

      let for_initial_domain = Access
    end

    let[@inline] access (f : Access.t -> 'a @ portable contended) =
      try f Access.Access with
      | exn ->
        let bt = Printexc.get_raw_backtrace () in
        let exn_string = Printexc.to_string exn in
        Printexc.raise_with_backtrace (Encapsulated exn_string) bt

    module Obj_opt : sig @@ portable
      type t
      val none : t
      val some : 'a -> t
      val is_some : t -> bool

      (** [unsafe_get obj] may only be called safely
          if [is_some] is true.

          [unsafe_get (some v)] is equivalent to
          [Obj.obj (Obj.repr v)]. *)
      val unsafe_get : t -> 'a
    end = struct
      type t = Obj.t
      let none = Obj.magic_portable (Obj.repr (ref 0))
      let some v = Obj.repr v
      let is_some obj = (obj != Obj.magic_uncontended none)
      let unsafe_get obj = Obj.obj obj
    end

    type dls_state = Obj_opt.t array

    external get_dls_state : unit -> dls_state @@ portable = "%dls_get"

    external set_dls_state : dls_state -> unit @@ portable =
      "caml_domain_dls_set" [@@noalloc]

    external compare_and_set_dls_state : dls_state -> dls_state -> bool @@ portable =
      "caml_domain_dls_compare_and_set" [@@noalloc]

    let create_dls () =
      let st = Array.make 8 (Obj.magic_uncontended Obj_opt.none) in
      set_dls_state st

    let init () = create_dls ()

    type 'a key = int * (Access.t -> 'a) Modes.Portable.t

    let key_counter = Atomic.make 0

    type key_initializer : immutable_data =
        KI: 'a key * ('a -> (Access.t -> 'a) @ portable) @@ portable -> key_initializer
    [@@unsafe_allow_any_mode_crossing "CR with-kinds"]

    type key_initializer_list : immutable_data = key_initializer list

    let parent_keys = Atomic.make ([] : key_initializer_list)

    let rec add_parent_key ki =
      let l = Atomic.Contended.get parent_keys in
      if not (Atomic.Contended.compare_and_set parent_keys l (ki :: l))
      then add_parent_key ki

    let new_key' ?split_from_parent init_orphan =
      let idx = Atomic.fetch_and_add key_counter 1 in
      let k = idx, { Modes.Portable.portable = init_orphan } in
      begin match split_from_parent with
      | None -> ()
      | Some split -> add_parent_key (KI(k, split))
      end;
      k

    (* If necessary, grow the current domain's local state array such that [idx]
    * is a valid index in the array. *)
    let rec maybe_grow idx =
      (* CR ocaml 5 all-runtime5: remove this hack which is here to stop
        the backend seeing the dls_get operation and failing on runtime4 *)
      if not (runtime5 ()) then assert false else
      (* end of hack *)
      let st = get_dls_state () in
      let sz = Array.length st in
      if idx < sz then st
      else begin
        let rec compute_new_size s =
          if idx < s then s else compute_new_size (2 * s)
        in
        let new_sz = compute_new_size sz in
        let new_st = Array.make new_sz (Obj.magic_uncontended Obj_opt.none) in
        Array.blit st 0 new_st 0 sz;
        (* We want a implementation that is safe with respect to
          single-domain multi-threading: retry if the DLS state has
          changed under our feet.
          Note that the number of retries will be very small in
          contended scenarios, as the array only grows, with
          exponential resizing. *)
        if compare_and_set_dls_state st new_st
        then new_st
        else maybe_grow idx
      end

    let set (type a) (_ : Access.t) (idx, _init) (x : a) =
      let st = maybe_grow idx in
      (* [Sys.opaque_identity] ensures that flambda does not look at the type of
      * [x], which may be a [float] and conclude that the [st] is a float array.
      * We do not want OCaml's float array optimisation kicking in here. *)
      st.(idx) <- Obj_opt.some (Sys.opaque_identity x)


    let[@inline never] array_compare_and_set a i oldval newval =
      (* Note: we cannot use [@poll error] due to the
        allocations on a.(i) in the Double_array case. *)
      let curval = a.(i) in
      if curval == oldval then (
        Array.unsafe_set a i newval;
        true
      ) else false

    let get (type a) access ((idx, init) : a key) : a =
      let st = maybe_grow idx in
      let obj = st.(idx) in
      if Obj_opt.is_some obj
      then (Obj_opt.unsafe_get obj : a)
      else begin
        let v : a = init.portable access in
        let new_obj = Obj_opt.some (Sys.opaque_identity v) in
        (* At this point, [st] or [st.(idx)] may have been changed
          by another thread on the same domain.

          If [st] changed, it was resized into a larger value,
          we can just reuse the new value.

          If [st.(idx)] changed, we drop the current value to avoid
          letting other threads observe a 'revert' that forgets
          previous modifications. *)
        let st = get_dls_state () in
        if array_compare_and_set st idx obj new_obj
        then v
        else begin
          (* if st.(idx) changed, someone must have initialized
            the key in the meantime. *)
          let updated_obj = st.(idx) in
          if Obj_opt.is_some updated_obj
          then (Obj_opt.unsafe_get updated_obj : a)
          else assert false
        end
      end

    type key_value : value mod portable contended =
        KV : 'a key * (Access.t -> 'a) @@ portable -> key_value
    [@@unsafe_allow_any_mode_crossing "CR with-kinds"]

    let get_initial_keys access : key_value list =
      List.map
        (fun (KI (k, split)) -> KV (k, (split (get access k))))
        (Atomic.Contended.get parent_keys)

    let set_initial_keys access (l: key_value list) =
      List.iter (fun (KV (k, v)) -> set access k (v access)) l
  end

  (******** Identity **********)

  let get_id { domain; _ } = domain

  let self () = Raw.self ()

  let is_main_domain () = (self () :> int) = 0

  (******** Callbacks **********)

  (* first spawn, domain startup and at exit functionality *)
  let first_domain_spawned = Atomic.make false

  let first_spawn_function = Obj.magic_portable (ref (fun () -> ()))

  let before_first_spawn f =
    if Atomic.Contended.get first_domain_spawned then
      raise (Invalid_argument "first domain already spawned")
    else begin
      let old_f = !first_spawn_function in
      let new_f () = old_f (); f () in
      first_spawn_function := new_f
    end

  let do_before_first_spawn () =
    if not (Atomic.Contended.get first_domain_spawned) then begin
      Atomic.Contended.set first_domain_spawned true;
      let first_spawn_function = Obj.magic_uncontended first_spawn_function in
      !first_spawn_function();
      (* Release the old function *)
      first_spawn_function := (fun () -> ())
    end

  let at_exit_key = DLS.new_key' (fun (_ : DLS.Access.t) -> (fun () -> ()))

  let at_exit' access f =
    let old_exit : unit -> unit = DLS.get access at_exit_key in
    let new_exit () =
      f (); old_exit ()
    in
    DLS.set access at_exit_key new_exit

  let do_at_exit () =
    let f : unit -> unit = DLS.get DLS.Access.for_initial_domain at_exit_key in
    f ()

  let _ = Stdlib.do_domain_local_at_exit := do_at_exit

  (******* Creation and Termination ********)

  let spawn' f =
    do_before_first_spawn ();
    let pk = DLS.access (fun access -> DLS.get_initial_keys access) in

    (* [term_sync] is used to synchronize with the joining domains *)
    let term_sync =
      Raw.{ state = Running ;
            mut = Mutex.create () ;
            cond = Condition.create () }
    in

    let body () =
      match
        DLS.create_dls ();
        let access = DLS.Access.Access in
        DLS.set_initial_keys access pk;
        let res = f access in
        res
      with
      (* Run the [at_exit] callbacks when the domain computation either
        terminates normally or exceptionally. *)
      | res ->
          (* If the domain computation terminated normally, but the
            [at_exit] callbacks raised an exception, then return the
            exception. *)
          do_at_exit ();
          res
      | exception exn ->
          (* If both the domain computation and the [at_exit] callbacks
            raise exceptions, then ignore the exception from the
            [at_exit] callbacks and return the original exception. *)
          (try do_at_exit () with _ -> ());
          raise exn
    in
    let domain = Raw.spawn body term_sync in
    { domain ; term_sync }

  let join { term_sync ; _ } =
    let open Raw in
    let rec loop () =
      match term_sync.state with
      | Running ->
          Condition.wait term_sync.cond term_sync.mut;
          loop ()
      | Finished res ->
          res
    in
    match Mutex.protect term_sync.mut loop with
    | Ok x -> x
    | Error ex -> raise ex

  let recommended_domain_count = Raw.get_recommended_domain_count
end

module type S = sig
  module DLS : sig
    module Access : sig
      type t : value mod external_ global portable many unique

      val for_initial_domain : t @@ nonportable
    end

    type 'a key : value mod portable contended

    val access
      :  (Access.t -> 'a @ portable contended) @ local portable
      -> 'a @ portable contended
      @@ portable

    val new_key'
      :  ?split_from_parent:('a -> (Access.t -> 'a) @ portable) @ portable
      -> (Access.t -> 'a) @ portable
      -> 'a key
      @@ portable

    val get : Access.t -> 'a key -> 'a @@ portable
    val set : Access.t -> 'a key -> 'a -> unit @@ portable

    val init : unit -> unit
  end

  type !'a t : value mod contended portable
  val spawn' : (DLS.Access.t -> 'a) @ portable -> 'a t @@ portable
  val join : 'a t -> 'a @@ portable
  type id = private int
  val get_id : 'a t -> id @@ portable
  val self : unit -> id @@ portable
  val cpu_relax : unit -> unit @@ portable
  val is_main_domain : unit -> bool @@ portable
  val recommended_domain_count : unit -> int @@ portable
  val before_first_spawn : (unit -> unit) -> unit @@ nonportable
  val at_exit' : DLS.Access.t -> (unit -> unit) -> unit @@ portable
  val do_at_exit : unit -> unit @@ nonportable
end

let runtime_4_impl = (module Runtime_4 : S)
let runtime_5_impl = (module Runtime_5 : S)

let impl = if runtime5 () then runtime_5_impl else runtime_4_impl

module M : S = (val impl)
include M

module Safe = struct
  module DLS = struct
    include DLS

    exception Encapsulated = Encapsulated

    let new_key ?split_from_parent f =
      let split_from_parent =
        match split_from_parent with
        | None -> None
        | Some split_from_parent ->
          Some (fun a ->
            let f = split_from_parent a in
            (fun (_ : Access.t) -> f ()))
      in
      let f = (fun (_ : Access.t) -> f ()) in
      new_key' ?split_from_parent f
    ;;
  end

  let spawn' = spawn'
  let spawn f = spawn' (fun _ -> f ())
  let at_exit' = at_exit'
  let at_exit f = DLS.access (fun access -> at_exit' access f)
end

module DLS = struct
  type 'a key = 'a Safe.DLS.key

  let new_key ?split_from_parent f =
    let split_from_parent =
      match split_from_parent with
      | None -> None
      | Some split_from_parent ->
        Some (Obj.magic_portable (fun x -> Obj.magic_portable (fun () -> split_from_parent x)))
    in
    Safe.DLS.new_key ?split_from_parent (Obj.magic_portable f)
  ;;

  let get key = Safe.DLS.get (Obj.magic () : Safe.DLS.Access.t) key
  let set key value = Safe.DLS.set (Obj.magic () : Safe.DLS.Access.t) key value

  let init = Safe.DLS.init
end

let spawn f = Safe.spawn (Obj.magic_portable f)
let at_exit f = Safe.at_exit (Obj.magic_portable f)

let () = DLS.init ()

let _ = Stdlib.do_domain_local_at_exit := do_at_exit
