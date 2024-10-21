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

module Runtime_4 = struct
  module DLS = struct

    let unique_value = Obj.repr (ref 0)
    let state = ref (Array.make 8 unique_value)

    let init () = ()

    type password = unit
    type 'a key = int * (unit -> 'a)

    let initial_password = ()
    let with_password f = f ()

    let key_counter = ref 0

    let new_key ?split_from_parent:_ init_orphan =
      let idx = !key_counter in
      key_counter := idx + 1;
      (idx, init_orphan)

    let new_key_safe ?split_from_parent:_ init_orphan = new_key init_orphan

    (* If necessary, grow the current domain's local state array such that [idx]
    * is a valid index in the array. *)
    let maybe_grow idx =
      let st = !state in
      let sz = Array.length st in
      if idx < sz then st
      else begin
        let rec compute_new_size s =
          if idx < s then s else compute_new_size (2 * s)
        in
        let new_sz = compute_new_size sz in
        let new_st = Array.make new_sz unique_value in
        Array.blit st 0 new_st 0 sz;
        state := new_st;
        new_st
      end

    let set' () (idx, _init) x =
      let st = maybe_grow idx in
      (* [Sys.opaque_identity] ensures that flambda does not look at the type of
      * [x], which may be a [float] and conclude that the [st] is a float array.
      * We do not want OCaml's float array optimisation kicking in here. *)
      st.(idx) <- Obj.repr (Sys.opaque_identity x)

    let set key x = set' () key x

    let get' () (idx, init) =
      let st = maybe_grow idx in
      let v = st.(idx) in
      if v == unique_value then
        let v' = Obj.repr (init ()) in
        st.(idx) <- (Sys.opaque_identity v');
        Obj.magic v'
      else Obj.magic v

    let get key = get' () key
  end

  (******** Callbacks **********)

  (* first spawn, domain startup and at exit functionality *)
  let first_domain_spawned = Atomic.make false

  let first_spawn_function = ref (fun () -> ())

  let before_first_spawn f =
    if Atomic.get_safe first_domain_spawned then
      raise (Invalid_argument "first domain already spawned")
    else begin
      let old_f = !first_spawn_function in
      let new_f () = old_f (); f () in
      first_spawn_function := new_f
    end

  let at_exit_key = DLS.new_key (fun () -> (fun () -> ()))

  let at_exit' () f =
    let old_exit : unit -> unit = DLS.get at_exit_key in
    let new_exit () =
      (* The domain termination callbacks ([at_exit]) are run in
        last-in-first-out (LIFO) order in order to be symmetric with the domain
        creation callbacks ([at_each_spawn]) which run in first-in-fisrt-out
        (FIFO) order. *)
      f (); old_exit ()
    in
    DLS.set at_exit_key new_exit

  let at_exit_safe = at_exit' ()

  let at_exit = at_exit_safe

  let do_at_exit () =
    let f : unit -> unit = DLS.get at_exit_key in
    f ()

  (* Unimplemented functions *)
  let not_implemented () =
    failwith "Multi-domain functionality not supported in runtime4"
  type !'a t
  type id = int
  let spawn _ = not_implemented ()
  let spawn_safe _ = not_implemented ()
  let spawn_with_dls _ = not_implemented ()
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
    external spawn : (unit -> unit) -> Mutex.t -> t @@ portable
      = "caml_domain_spawn"
    external self : unit -> t @@ portable
      = "caml_ml_domain_id"
    external cpu_relax : unit -> unit @@ portable
      = "caml_ml_domain_cpu_relax"
    external get_recommended_domain_count: unit -> int @@ portable
      = "caml_recommended_domain_count" [@@noalloc]
  end

  let cpu_relax () = Raw.cpu_relax ()

  type id = Raw.t

  type 'a state =
  | Running
  | Finished of ('a, exn) result

  type 'a t = {
    domain : Raw.t;
    term_mutex: Mutex.t;
    term_condition: Condition.t;
    term_state: 'a state ref (* protected by [term_mutex] *)
  }

  module DLS = struct

    type dls_state = Obj.t array

    let unique_value = Obj.magic_portable (Obj.repr (ref 0))

    external get_dls_state : unit -> dls_state @@ portable = "%dls_get"

    external set_dls_state : dls_state -> unit @@ portable =
      "caml_domain_dls_set" [@@noalloc]

    let create_dls () =
      let st = Array.make 8 (Obj.magic_uncontended unique_value) in
      set_dls_state st

    let init () = create_dls ()

    type password = unit

    type 'a key : value mod portable uncontended = K of int * (password -> 'a) @@ portable

    let initial_password = ()
    let with_password f = f ()

    let key_counter = Atomic.make 0

    type key_initializer =
      KI: 'a key * ('a -> (password -> 'a) @ portable) @@ portable -> key_initializer

    (* CR tdelvecchio: Remove when we have [with]. *)
    type key_initializer_list : value mod portable uncontended = KIs of key_initializer list

    let parent_keys = Atomic.make (KIs [])

    let rec add_parent_key ki =
      let (KIs l) as cur = Atomic.get_safe parent_keys in
      if not (Atomic.compare_and_set parent_keys cur (KIs (ki :: l)))
      then add_parent_key ki

    let new_key_safe ?split_from_parent init_orphan =
      let idx = Atomic.fetch_and_add key_counter 1 in
      let k = K (idx, init_orphan) in
      begin match split_from_parent with
      | None -> ()
      | Some split -> add_parent_key (KI(k, split))
      end;
      k

    let new_key ?split_from_parent init_orphan =
      let split_from_parent = Obj.magic_portable split_from_parent in
      let init_orphan = Obj.magic_portable init_orphan in
      let split_from_parent =
        match split_from_parent with
        | None -> None
        | Some f ->
          Some (fun x -> Obj.magic_portable (fun (_ : password) -> f x))
      in
      new_key_safe ?split_from_parent init_orphan

    (* If necessary, grow the current domain's local state array such that [idx]
    * is a valid index in the array. *)
    let maybe_grow idx =
      let st = get_dls_state () in
      let sz = Array.length st in
      if idx < sz then st
      else begin
        let rec compute_new_size s =
          if idx < s then s else compute_new_size (2 * s)
        in
        let new_sz = compute_new_size sz in
        let new_st = Array.make new_sz (Obj.magic_uncontended unique_value) in
        Array.blit st 0 new_st 0 sz;
        set_dls_state new_st;
        new_st
      end

    let set' () (K (idx, _init)) x =
      let st = maybe_grow idx in
      (* [Sys.opaque_identity] ensures that flambda does not look at the type of
      * [x], which may be a [float] and conclude that the [st] is a float array.
      * We do not want OCaml's float array optimisation kicking in here. *)
      st.(idx) <- Obj.repr (Sys.opaque_identity x)

    let set k x = set' () k x

    let get' () (K (idx, init)) =
      let st = maybe_grow idx in
      let v = st.(idx) in
      if v == (Obj.magic_uncontended unique_value) then
        let v' = Obj.repr (init ()) in
        st.(idx) <- (Sys.opaque_identity v');
        Obj.magic v'
      else Obj.magic v

    let get k = get' () k

    let get_initial_keys () : (int * Obj.t) list =
      let (KIs parent_keys) : key_initializer_list = Atomic.get_safe parent_keys in
      List.map
        (fun (KI (K (idx, _) as k, split)) ->
            (idx, Obj.repr ((split (get k)) ())))
        parent_keys

    let set_initial_keys (l: (int * Obj.t) list) =
      List.iter
        (fun (idx, v) ->
          let st = maybe_grow idx in st.(idx) <- v)
        l

  end

  (******** Identity **********)

  let get_id { domain; _ } = domain

  let self () = Raw.self ()

  let is_main_domain () = (self () :> int) = 0

  (******** Callbacks **********)

  (* first spawn, domain startup and at exit functionality *)
  let first_domain_spawned = Atomic.make false

  type f : value mod portable = F of (unit -> unit) @@ portable

  let first_spawn_function = Atomic.make (F (fun () -> ()))

  let before_first_spawn f =
    if Atomic.get_safe first_domain_spawned then
      raise (Invalid_argument "first domain already spawned")
    else begin
      (* We are guaranteed to still only have one domain, so this is safe. *)
      let f = Obj.magic_portable f in
      let F old_f = Atomic.get_safe first_spawn_function in
      let new_f () = old_f (); f () in
      Atomic.set first_spawn_function (F new_f)
    end

  let at_exit_key = DLS.new_key (fun () -> (fun () -> ()))

  let at_exit' () f =
    let old_exit : unit -> unit = DLS.get at_exit_key in
    let new_exit () =
      (* The domain termination callbacks ([at_exit]) are run in
        last-in-first-out (LIFO) order in order to be symmetric with the domain
        creation callbacks ([at_each_spawn]) which run in first-in-fisrt-out
        (FIFO) order. *)
      f (); old_exit ()
    in
    DLS.set at_exit_key new_exit

  let at_exit_safe = at_exit' ()

  let at_exit = at_exit_safe

  let do_at_exit () =
    let f : unit -> unit = DLS.get at_exit_key in
    f ()

  (******* Creation and Termination ********)

  let do_before_first_spawn () =
    if not (Atomic.get_safe first_domain_spawned) then begin
      Atomic.set first_domain_spawned true;
      let F f = Atomic.get_safe first_spawn_function in
      f ();
      (* Release the old function *)
      Atomic.set first_spawn_function (F (fun () -> ()))
    end

  let spawn_with_dls f =
    do_before_first_spawn ();
    let pk = DLS.get_initial_keys () in

    (* The [term_mutex] and [term_condition] are used to
      synchronize with the joining domains *)
    let term_mutex = Mutex.create () in
    let term_condition = Condition.create () in
    let term_state = ref Running in

    let body () =
      let result =
        match
          DLS.create_dls ();
          DLS.set_initial_keys pk;
          let res = f () in
          res
        with
        | x -> Ok x
        | exception ex -> Error ex
      in

      let result' =
        (* Run the [at_exit] callbacks when the domain computation either
          terminates normally or exceptionally. *)
        match do_at_exit () with
        | () -> result
        | exception ex ->
            begin match result with
            | Ok _ ->
                (* If the domain computation terminated normally, but the
                  [at_exit] callbacks raised an exception, then return the
                  exception. *)
                Error ex
            | Error _ ->
                (* If both the domain computation and the [at_exit] callbacks
                  raised exceptions, then ignore the exception from the
                  [at_exit] callbacks and return the original exception. *)
                result
            end
      in

      (* Synchronize with joining domains *)
      Mutex.lock term_mutex;
      match !term_state with
      | Running ->
          term_state := Finished result';
          Condition.broadcast term_condition;
      | Finished _ ->
          failwith "internal error: Am I already finished?"
      (* [term_mutex] is unlocked in the runtime after the cleanup functions on
        the C side are finished. *)
    in
    { domain = Raw.spawn body term_mutex;
      term_mutex;
      term_condition;
      term_state }

  let spawn_safe f = spawn_with_dls (fun (_ : DLS.password) -> f ())

  let spawn = spawn_safe

  let join { term_mutex; term_condition; term_state; _ } =
    Mutex.lock term_mutex;
    let rec loop () =
      match !term_state with
      | Running ->
          Condition.wait term_condition term_mutex;
          loop ()
      | Finished res ->
          Mutex.unlock term_mutex;
          res
    in
    match loop () with
    | Ok x -> x
    | Error ex -> raise ex

  let recommended_domain_count = Raw.get_recommended_domain_count
end

module type S4 = sig
  [@@@warning "-32"]

  module DLS : sig
    type password
    val initial_password : password
    val with_password : (password -> 'a) -> 'a

    type 'a key
    val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key
    val new_key_safe : ?split_from_parent:('a -> (password -> 'a)) -> (password -> 'a) -> 'a key
    val get : 'a key -> 'a
    val get' : password -> 'a key -> 'a
    val set : 'a key -> 'a -> unit
    val set' : password -> 'a key -> 'a -> unit

    val init : unit -> unit
  end

  type !'a t
  val spawn : (unit -> 'a) -> 'a t
  val spawn_safe : (unit -> 'a) -> 'a t
  val spawn_with_dls : (DLS.password -> 'a) -> 'a t
  val join : 'a t -> 'a
  type id = private int
  val get_id : 'a t -> id
  val self : unit -> id
  val cpu_relax : unit -> unit
  val is_main_domain : unit -> bool
  val recommended_domain_count : unit -> int
  val before_first_spawn : (unit -> unit) -> unit
  val at_exit : (unit -> unit) -> unit
  val at_exit_safe : (unit -> unit) -> unit
  val at_exit' : DLS.password -> (unit -> unit) -> unit
  val do_at_exit : unit -> unit
end

module type S5 = sig
  module DLS : sig
    type password
    val initial_password : password
    val with_password : (password -> 'a @ contended portable) @ portable -> 'a @ contended portable @@ portable

    type 'a key : value mod portable uncontended
    val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key
    val new_key_safe : ?split_from_parent:('a -> (password -> 'a) @ portable) @ portable -> (password -> 'a) @ portable -> 'a key @@ portable
    val get : 'a key -> 'a
    val get' : password -> 'a key -> 'a @@ portable
    val set : 'a key -> 'a -> unit
    val set' : password -> 'a key -> 'a -> unit @@ portable

    val init : unit -> unit
  end

  type !'a t
  val spawn : (unit -> 'a) -> 'a t
  val spawn_safe : (unit -> 'a) @ portable -> 'a t @@ portable
  val spawn_with_dls : (DLS.password -> 'a) @ portable -> 'a t @@ portable
  val join : 'a t -> 'a @@ portable
  type id = private int
  val get_id : 'a t -> id @@ portable
  val self : unit -> id @@ portable
  val cpu_relax : unit -> unit @@ portable
  val is_main_domain : unit -> bool @@ portable
  val recommended_domain_count : unit -> int @@ portable
  val before_first_spawn : (unit -> unit) -> unit
  val at_exit : (unit -> unit) -> unit
  val at_exit_safe : (unit -> unit) @ portable -> unit @@ portable
  val at_exit' : DLS.password -> (unit -> unit) -> unit @@ portable
  val do_at_exit : unit -> unit @@ portable
end

let runtime_4_impl = (module Runtime_4 : S4)
let runtime_5_impl = (module Runtime_5 : S5)

external runtime5 : unit -> bool @@ portable = "%runtime5"

let impl = if runtime5 () then runtime_5_impl else (Obj.magic (runtime_4_impl : (module S4)) : (module S5))

include (val impl : S5)

let () = DLS.init ()

let _ = Stdlib.set_do_domain_local_at_exit do_at_exit
