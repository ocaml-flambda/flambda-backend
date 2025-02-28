(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Leo White, Jane Street, London                      *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Handler_index : sig
  @@ portable

  type ('es1, 'es2) t : immediate
  (** [(es1, es2) t] represents an index into the effect list [es2]. [es1]
      is the tail sublist containing the index and all following effects.
      For instance, [('b * ('c * unit), 'a * ('b * ('c * unit)) t] represents
      effect 1, ['b].

      It is used for building [Raw_handler.t]s, which represent one of the
      effects handled by a particular handler. *)

  val zero : ('es, 'es) t
  (** [zero] is the index of the first element of an effect list. *)

  val one : ('es, 'e * 'es) t
  (** [zero] is the index of the second element of an effect list. *)

  val succ : ('e * 'es1, 'es2) t -> ('es1, 'es2) t
  (** [succ t] is the index of the next element of an effect list after [t]. *)

  val is_zero : ('es1, 'es2) t -> ('es1, 'es2) Type.eq option
  (** [is_zero t] tests if [t] is the index of the first element of an effect
      list. If it is then we learn that the remaining list is equal to the
      full list. *)

  val weaken : ('es1, 'e * 'es2) t -> ('es1, 'es2) t
  (** [weaken t] is an index for [es], where [t] is an index for [e * es].
      If [t] is [zero] then [weaken t] is an invalid index and does not
      correspond to an element in [es]. *)

  val to_int : ('es1, 'es2) t -> int
  (** [to_int t] is the integer representation of [t]. *)

end = struct

  type ('es1, 'es2) t = int

  let zero = 0

  let one = 1

  let succ t = t + 1

  let is_zero (type es1 es2) (t : (es1, es2) t) =
    if Int.equal t 0 then (Obj.magic (Some Type.Equal) : (es1, es2) Type.eq option)
    else None

  let weaken t = t - 1

  let to_int t = t

end

module Raw_handler : sig
  @@ portable

  type ('e, 'es) t : immediate
  (** [(e, es) t] is a handler for the effect [e], which must be an element
      of the effect list [es].

      It is used to represent one of the effects handled by a particular
      handler. *)

  val of_index : ('e * _, 'es) Handler_index.t -> ('e, 'es) t
  (** [of_index i] is the handler corresponding to index [t]. *)

  val zero : ('e, 'e * _) t
  (** [zero] is [of_index (Handler_index.zero)]. *)

  val is_zero : ('e1, 'e2 * _) t -> ('e1, 'e2) Type.eq option
  (** [is_zero t] tests if this handler is [zero]. If it is we learn
      that the handled effect is the first effect in the list. *)

  val weaken : ('e1, 'e2 * 'es2) t -> ('e1, 'es2) t
  (** [weaken t] is a handler from the list [es], where [t] is a handler
      from the list [e * es]. If [t] is [zero] then [weaken t] is an
      invalid handler (i.e. not actually in the list). *)

  val to_int : ('e, 'es) t -> int
  (** [to_int t] is the integer representation of [t]. *)

end = struct

  type ('e, 'es) t =
      Raw_handler : ('e * _, 'es) Handler_index.t -> ('e, 'es) t
  [@@unboxed]

  let of_index i = Raw_handler i

  let zero = Raw_handler Handler_index.zero

  let is_zero (type e1 e2 es) (Raw_handler i : (e1, e2 * es) t)
    : (e1, e2) Type.eq option =
    match Handler_index.is_zero i with
    | Some Equal as eq -> eq
    | None as eq -> eq

  let weaken (Raw_handler i) = Raw_handler (Handler_index.weaken i)

  let to_int (Raw_handler i) = Handler_index.to_int i

end

module Handler : sig
  @@ portable

  type 'e t' = private ..
  (** [e t] is a handler for the effect [e]. *)

  type 'e t = { h : 'e t' @@ global aliased } [@@unboxed]
  (** Heap-allocated handler. *)

  type _ t' += Dummy : 'a t'

  module List : sig
    type 'e handler := 'e t

    type 'es t =
      | [] : unit t
      | (::) : 'e handler * 'es t -> ('e * 'es) t
    (** [es t] is a list of handlers for effects [es]. *)

    module Length : sig

      type x = X
      (** [x] is the type of [X]s *)

      type 'es t =
        | [] : unit t
        | (::) : x * 'es t -> ('e * 'es) t
      (** [es t] is the length of effect list [es]. It has slightly unusual
          constructors so that lengths can be written as [[X;X;X]] rather
          than e.g. [(S (S (S Z)))]. This looks nicer on calls to [fiber_with]:

          {[
            fiber_with [X; X; X] (fun [a; b; c] -> ...)
          ]}
      *)

    end

    val length : local_ 'es t -> 'es Length.t
    (** [length t] is the length of [t]. *)

  end

  module type Effs = sig
    type e
    type es
  end

  module type Create = sig

    type e

    type es

    type 'e t' += C : ('e, e * es) Raw_handler.t -> 'e t'

    val initial : length:local_ es List.Length.t -> es List.t
    (** [initial ~length] is a list of handlers for effects [es], where [length]
        is the length of [es]. *)

    val initial_from : local_ es List.t -> es List.t
    (** [initial_from hs] is a list of handlers for effect [es], where [hs] is
        another list of handlers for effects [es]. These handlers are selected
        from the effect list [e * es] for some effect [e]. Note that [hs] is
        being used only for its length -- the actual handlers in it do not
        affect the output. *)

  end

  module Create (Effs : Effs)
    : Create with type e = Effs.e and type es = Effs.es

end = struct

  type 'e t' = ..
  (** [e t] is a handler for the effect [e]. *)

  type 'e t = { h : 'e t' @@ global aliased } [@@unboxed]
  (** Heap-allocated handler. *)

  type _ t' += Dummy : 'a t'

  module List = struct

    type 'e handler = 'e t

    type 'es t =
      | [] : unit t
      | (::) : 'e handler * 'es t -> ('e * 'es) t

    module Length = struct

      type x = X
      (** [x] is the type of [X]s *)

      type 'es t =
        | [] : unit t
        | (::) : x * 'es t -> ('e * 'es) t
      (** [es t] is the length of effect list [es]. It has slightly unusual
          constructors so that lengths can be written as [[X;X;X]] rather
          than e.g. [(S (S (S Z)))]. This looks nicer on calls to [fiber_with]:

          {[
            fiber_with [X; X; X] (fun [a; b; c] -> ...)
          ]}
      *)

    end

    let length (local_ t) =
      let rec loop : type es . local_ es t -> es Length.t = function
        | [] -> []
        | _ :: rest -> X :: (loop rest)
      in
      loop t [@nontail]

  end

  module type Effs = sig
    type e
    type es
  end

  module type Create = sig

    type e

    type es

    type 'e t' += C : ('e, e * es) Raw_handler.t -> 'e t'

    val initial : length:local_ es List.Length.t -> es List.t
    (** [initial ~length] is a list of handlers for effects [es], where [length]
        is the length of [es]. *)

    val initial_from : local_ es List.t -> es List.t
    (** [initial_from hs] is a list of handlers for effect [es], where [hs] is
        another list of handlers for effects [es]. These handlers are selected
        from the effect list [e * es] for some effect [e]. Note that [hs] is
        being used only for its length -- the actual handlers in it do not
        affect the output. *)

  end

  module[@inline] Create (Effs : Effs) = struct

    type e = Effs.e

    type es = Effs.es

    type 'e t' += C : ('e, e * es) Raw_handler.t -> 'e t'

    let initial
          ~(local_ length : es List.Length.t) : es List.t =
      let rec loop
        : type esr. (esr, e * es) Handler_index.t
               -> local_ esr List.Length.t -> esr List.t =
        fun i l ->
          match l with
          | [] -> []
          | X :: l' ->
              let h = Raw_handler.of_index i in
              { h = C h } :: loop (Handler_index.succ i) l'
      in
      loop Handler_index.one length [@nontail]

    let initial_from (local_ t : es List.t) : es List.t =
      let rec loop
        : type esr. (esr, e * es) Handler_index.t -> local_ esr List.t
               -> esr List.t =
        fun i l ->
          match l with
          | [] -> []
          | _ :: rest ->
              let h = Raw_handler.of_index i in
              { h = C h } :: loop (Handler_index.succ i) rest
      in
      loop Handler_index.one t [@nontail]

  end

end

module Mapping : sig
  @@ portable

  type 'es t
  (** [es t] represents a mutable mapping of handlers selected from
      list [es]. *)

  val lookup :
    local_ ('e, 'es) Raw_handler.t
    -> 'es t
    -> 'e Handler.t
  (** [lookup h t] looks up handler [h] in mapping [t] and returns the
      corresponding handler. *)

  val empty : unit -> unit t
  (** [empty] is the mapping out of the empty list. *)

  val create : local_ 'es Handler.List.t -> 'es t
  (** [create hs] creates a new mapping from [es], where [hs] is a list
      of handlers for effects [es]. Its initial value is to map each
      handler from [es] to the corresponding handler in [hs]. *)

  val set : local_ 'es Handler.List.t -> 'es t -> unit
  (** [set hs t] updates the mapping [t] to map each handler to the
      corresponding handler in [hs]. *)

  val create_unset : local_ 'es Handler.List.Length.t -> 'es t
  (** [create len] creates a new uninitialized mapping from [es], where
      [len] is the length of [es]. The mapping must be initialized with
      [set] before it is used. *)

end = struct

  type element

  let uninitialized () : element = Obj.magic (-1)

  type 'es1 t = element array

  let lookup (type e es) (h : (e, es) Raw_handler.t) (t : es t) =
    let elt = Array.unsafe_get t (Raw_handler.to_int h) in
    (Obj.magic elt : e Handler.t)

  let empty () = [||]

  let make (type es) (idx : (unit, es) Handler_index.t) : es t =
    Array.make (Handler_index.to_int idx) (uninitialized ())

  let set_element (type e esr es) (t : es t)
      (idx : (e * esr, es) Handler_index.t) (local_ h : e Handler.t) =
    let elt : element = Obj.magic h.h in
    Array.unsafe_set t (Handler_index.to_int idx) elt

  let create (type es) (local_ l : es Handler.List.t) =
    let rec loop : type esr.
      (esr, es) Handler_index.t -> local_ esr Handler.List.t -> es t =
      fun idx l ->
        match l with
        | [] -> make idx
        | h :: rest ->
          let t = loop (Handler_index.succ idx) rest in
          set_element t idx h;
          t
    in
    loop Handler_index.zero l [@nontail]

  let set (type es) (local_ hs : es Handler.List.t) (t : es t) =
    let rec loop : type esr.
      (esr, es) Handler_index.t -> local_ esr Handler.List.t -> es t -> unit =
      fun idx hs t ->
        match hs with
        | [] -> ()
        | h :: rest ->
            set_element t idx h;
            loop (Handler_index.succ idx) rest t
    in
    loop Handler_index.zero hs t [@nontail]

  let create_unset (type es) (local_ l : es Handler.List.Length.t) =
    let rec loop
      : type esr. (esr, es) Handler_index.t ->
             local_ esr Handler.List.Length.t -> es t =
      fun idx l ->
        match l with
        | [] -> make idx
        | X :: l' -> loop (Handler_index.succ idx) l'
    in
    loop Handler_index.zero l [@nontail]

end

type ('a, 'e) op

type ('a, 'e) perform = ('a, 'e) op * 'e Handler.t'

external perform : ('a, 'e) perform -> 'a @@ portable = "%perform"

type (-'a, +'b) stack : immediate

type last_fiber : immediate

external resume : ('a, 'b) stack -> ('c -> 'a) -> 'c -> last_fiber -> 'b @@ portable = "%resume"
external runstack : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b @@ portable = "%runstack"

type (-'a, +'b) cont

external take_cont_noexc : ('a, 'b) cont -> ('a, 'b) stack @@ portable =
  "caml_continuation_use_noexc" [@@noalloc]

external get_cont_callstack :
  ('a, 'b) cont -> int -> Printexc.raw_backtrace @@ portable =
  "caml_get_continuation_callstack"

external cont_last_fiber : ('a, 'b) cont -> last_fiber @@ portable = "%field1"
external cont_set_last_fiber :
  ('a, 'b) cont -> last_fiber -> unit @@ portable = "%setfield1"

type 'b effc =
  { effc : 'o 'e. ('o, 'e) perform -> ('o, 'b) cont -> last_fiber -> 'b }
  [@@unboxed][@@warning "-69"]

external alloc_stack :
  ('a -> 'b) ->
  (exn -> 'b) ->
  'b effc ->
  ('a, 'b) stack @@ portable = "caml_alloc_stack"

type (+'a, 'es) r =
  | Val : 'a @@ global aliased -> ('a, 'es) r
  | Exn : exn @@ global aliased -> ('a, 'es) r
  | Op :
      ('o, 'e) op @@ global aliased
      * ('e, 'es) Raw_handler.t
      * ('o, ('a, 'es) r) cont
      * last_fiber -> ('a, 'es) r

let valuec v = Val v
let exnc e = Exn e

external reperform :
  ('a, 'e) perform -> ('a, 'b) cont -> last_fiber -> 'b @@ portable = "%reperform"

let alloc_cont
    (type a b h e es)
    (module H : Handler.Create with type e = e and type es = es)
    (f : local_ h -> a -> b)
    (h : h) : (a, (b, e * es) r) cont =
  let exception Ready__ of (a, (b, e * es) r) cont in
  let effc (type o eh) ((op, h) as perf : (o, eh) perform)
      (k : (o, (b, e * es) r) cont) last_fiber =
    match h with
    | H.C h ->
      Op(op, h, k, last_fiber)
    | Handler.Dummy ->
        let k = (Obj.magic k : (a, (b, e * es) r) cont) in
        cont_set_last_fiber k last_fiber;
        raise_notrace (Ready__ k)
    | _ -> reperform perf k last_fiber
  in
  let s = alloc_stack valuec exnc {effc} in
  let dummy_op : (a, e) op = Obj.magic () in
  let p = dummy_op, Handler.Dummy in
  match runstack s (fun () -> f h (perform p)) () with
  | _ -> assert false
  | exception Ready__ k -> k

let run_stack
    (type a h e es)
    (module H : Handler.Create with type e = e and type es = es)
     (f : local_ h -> a) (h : h) : (a, e * es) r =
  let effc ((op, h) as perf) k last_fiber =
    match h with
    | H.C h ->
      Op(op, h, k, last_fiber)
    | _ -> reperform perf k last_fiber
  in
  let s = alloc_stack valuec exnc {effc} in
  runstack s (fun h -> f h) h

type (-'a, +'b, 'e, 'es) continuation =
  Cont :
    { cont : ('a, ('b, 'e * 'es) r) cont;
      mapping : 'es Mapping.t @@ global aliased; }
    -> ('a, 'b, 'e, 'es) continuation

type ('a, 'e, 'es) res =
  | Value : 'a @@ global aliased -> ('a, 'e, 'es) res
  | Exception : exn @@ global aliased -> ('a, 'e, 'es) res
  | Operation :
      ('o, 'e) op @@ global aliased
      * ('o, 'a, 'e, 'es) continuation
      -> ('a, 'e, 'es) res

let get_callstack (Cont { cont; _ }) i =
  get_cont_callstack cont i

let rec handle :
  type a e es. es Mapping.t -> (a, e * es) r -> (a, e, es) res =
  fun mapping -> function
    | Val x -> Value x
    | Exn e -> Exception e
    | Op(op, handler, k, last_fiber) -> begin
        match Raw_handler.is_zero handler with
        | Some Equal ->
          cont_set_last_fiber k last_fiber;
          Operation(op, Cont { cont = k; mapping })
        | None ->
            let handler = Raw_handler.weaken handler in
            let fwd = Mapping.lookup handler mapping in
            let result = (fun () -> reperform (op, fwd.h) k last_fiber) () in
            handle mapping result
      end

let resume (Cont { cont; mapping }) f x (local_ handlers) =
  Mapping.set handlers mapping;
  handle mapping (resume (take_cont_noexc cont) f x (cont_last_fiber cont))

let continue k v (local_ hs) = resume k (fun x -> x) v hs

let discontinue k e (local_ hs) = resume k (fun e -> raise e) e hs

let discontinue_with_backtrace k e bt (local_ hs) =
  resume k (fun e -> Printexc.raise_with_backtrace e bt) e hs

let fiber (type a b e)
    (f : local_ e Handler.t -> a -> b) =
  let module Effs = struct
      type nonrec e = e
      type nonrec es = unit
    end
  in
  let module H = Handler.Create(Effs) in
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = { h = H.C handler } in
  let mapping = Mapping.empty () in
  let cont = alloc_cont (module H) f handler in
  Cont { cont; mapping }

let fiber_with (type a b e es) (local_ l : es Handler.List.Length.t)
    (f : local_ (e * es) Handler.List.t -> a -> b) =
  let module H = Handler.Create(struct
      type nonrec e = e
      type nonrec es = es
    end) in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let handlers : (e * es) Handler.List.t =
    { h = H.C handler } :: H.initial ~length:l
  in
  let mapping = Mapping.create_unset l in
  let cont = alloc_cont (module H) f handlers in
  Cont { cont; mapping }

let run (type a e) (f : local_ e Handler.t -> a) =
  let module Effs = struct
      type nonrec e = e
      type nonrec es = unit
    end
  in
  let module H = Handler.Create(Effs) in
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = { h =  H.C handler } in
  let res = run_stack (module H) f handler in
  handle (Mapping.empty ()) res

let run_with (type a e es) (local_ hs : es Handler.List.t)
    (f : local_ (e * es) Handler.List.t -> a) =
  let module Effs = struct
      type nonrec e = e
      type nonrec es = es
    end
  in
  let module H = Handler.Create(Effs) in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let handlers : (e * es) Handler.List.t =
    { h = H.C handler } :: H.initial_from hs
  in
  let mapping = Mapping.create hs in
  let res = run_stack (module H) f handlers in
  handle mapping res

module type Aux = sig
  val fiber : 'a 'b 'e . ('e Handler.t @ local -> 'a -> 'b)
    -> ('a, 'b, 'e, unit) continuation

  val fiber_with : 'a 'b 'e 'es . 'es Handler.List.Length.t @ local
    -> (('e * 'es) Handler.List.t @ local -> 'a -> 'b)
    -> ('a, 'b, 'e, 'es) continuation

  val run : 'a 'e . ('e Handler.t @ local -> 'a)
    -> ('a, 'e, unit) res

  val run_with : 'a 'e 'es . 'es Handler.List.t @ local
    -> (('e * 'es) Handler.List.t @ local -> 'a)
    -> ('a, 'e, 'es) res
end

module type PortableAux = sig include Aux @@ portable end
let magic_aux (aux : (module Aux)) : (module PortableAux) = Obj.magic aux

(* CR dkalinichenko: [fiber], [fiber_wth], [run] and [run_with] are not
   [portable] because they use first-class modules, even though it's safe
   to declare them [portable] becuase the first-class module is created locally.
   
   Since they are polymorphic, they can't just be [Obj.magic_portable]ed either
   due to the value restriction. I ended up packing them into a first class module
   and [Obj.magic]ing it: *)
include (val (magic_aux (module struct
  let fiber = fiber
  let fiber_with = fiber_with
  let run = run
  let run_with = run_with
end)))

module Continuation = struct

  type (-'a, +'b, 'es) t : value mod contended =
    Continuation : ('a, 'c, 'e, 'es) continuation -> ('a, 'b, 'es) t
  [@@unsafe_allow_any_mode_crossing
    "CR dkalinichenko: only access mutable data uniquely."]
  [@@unboxed]
  (* This type has an unexpressible constraint that ['b] is a type that
     can safely be [Obj.magic]ed from [(c, e, es) res] *)

  let get_callstack (Continuation cont) i =
    get_callstack cont i
end

let continue (type a b es)
    (k : (a, b, es) Continuation.t) v (local_ hs) =
  let Continuation (type e c) (cont : (a, c, e, es) continuation) = k in
  let res : (c, e, es) res = continue cont v hs in
  (Obj.magic res : b)

let discontinue (type a b es)
    (k : (a, b, es) Continuation.t) e (local_ hs) =
  let Continuation (type e c) (cont : (a, c, e, es) continuation) = k in
  let res : (c, e, es) res = discontinue cont e hs in
  (Obj.magic res : b)

let discontinue_with_backtrace (type a b es)
    (k : (a, b, es) Continuation.t) e bt (local_ hs) =
  let Continuation (type e c) (cont : (a, c, e, es) continuation) = k in
  let res : (c, e, es) res = discontinue_with_backtrace cont e bt hs in
  (Obj.magic res : b)

module type S = sig
  @@ portable

  type ('o, 'e) ops

  type t

  module Result : sig

    type eff := t

    type ('a, 'es) t =
      | Value : 'a @@ global aliased -> ('a, 'es) t
      | Exception : exn @@ global aliased -> ('a, 'es) t
      | Operation :
          ('o, eff) ops @@ global aliased
          * ('o, ('a, 'es) t, 'es) Continuation.t
          -> ('a, 'es) t

    type ('a, 'es) handler =
      { handle :
          'o. ('o, eff) ops
          -> ('o, ('a, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'es) t -> ('a, 'es) handler -> 'a

    module Portable : sig
      type ('a, 'es) t = 
        | Value : 'a @@ global aliased -> ('a, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'es) t
        | Operation :
            ('o, eff) ops @@ global aliased
            * ('o, ('a, 'es) t, 'es) Continuation.t @@ portable
            -> ('a, 'es) t
            
      module Contended : sig
        type ('a, 'es) t =
          | Value : 'a @@ global aliased -> ('a, 'es) t
          | Exception : exn @@ global aliased -> ('a, 'es) t
          | Operation :
              ('o, eff) ops @@ global aliased contended
              * ('o Modes.Portable.t, ('a, 'es) t, 'es) Continuation.t @@ portable
              -> ('a, 'es) t
      end
    end
    
    module Contended : sig
      type ('a, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'es) t
        | Operation :
            ('o, eff) ops @@ global aliased contended
            * ('o Modes.Portable.t, ('a, 'es) t, 'es) Continuation.t
            -> ('a, 'es) t
    end
  end

  type ('a, 'es) result = ('a, 'es) Result.t =
    | Value : 'a @@ global aliased -> ('a, 'es) result
    | Exception : exn @@ global aliased -> ('a, 'es) result
    | Operation :
        ('o, t) ops @@ global aliased
        * ('o, ('a, 'es) result, 'es) Continuation.t
        -> ('a, 'es) result

  val fiber :
    (local_ t Handler.t -> 'a -> 'b)
    -> ('a, ('b, unit) Result.t, unit) Continuation.t

  val portable_fiber :
    (local_ t Handler.t -> 'a -> 'b) @ portable
    -> ('a, ('b, unit) Result.Portable.t, unit) Continuation.t @ portable
    
  val fiber_with :
    local_ 'es Handler.List.Length.t
    -> (local_ (t * 'es) Handler.List.t -> 'a -> 'b)
    -> ('a, ('b, 'es) Result.t, 'es) Continuation.t

  val portable_fiber_with :
    local_ 'es Handler.List.Length.t
    -> (local_ (t * 'es) Handler.List.t -> 'a -> 'b) @ portable
    -> ('a, ('b, 'es) Result.Portable.t, 'es) Continuation.t @ portable

  val run : (local_ t Handler.t -> 'a) -> ('a, unit) Result.t

  val portable_run :
    (local_ t Handler.t -> 'a) @ portable
    -> ('a, unit) Result.Portable.t @ portable

  val run_with :
    local_ 'es Handler.List.t
    -> (local_ (t * 'es) Handler.List.t -> 'a)
    -> ('a, 'es) Result.t

  val portable_run_with :
    local_ 'es Handler.List.t
    -> (local_ (t * 'es) Handler.List.t -> 'a) @ portable
    -> ('a, 'es) Result.Portable.t @ portable

  val perform : local_ t Handler.t -> ('a, t) ops -> 'a

  module Portable : sig
    val fiber :
      (t Handler.t @ local portable -> 'a @ contended -> 'b)
      -> ('a Modes.Portable.t, ('b, unit) Result.Contended.t, unit) Continuation.t
    (** [fiber f] constructs a continuation that runs the computation [f]. [f]
        is passed a [t Handler.t] so that it can perform operations from effect
        [t]. *)

    val portable_fiber :
      (t Handler.t @ local portable -> 'a @ contended -> 'b) @ portable
      -> ('a Modes.Portable.t, ('b, unit) Result.Portable.Contended.t, unit) Continuation.t @ portable
    (** [portable_fiber f] works the same as [fiber], but takes a [portable]
        closure and creates a [portable] continuation. *)

    val fiber_with :
      'es Handler.List.Length.t @ local
      -> ((t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b)
      -> ('a Modes.Portable.t, ('b, 'es) Result.Contended.t, 'es) Continuation.t
    (** [fiber_with l f] constructs a continuation that runs the computation [f],
        which requires handlers for [l] additional effects. [f] is passed a typed
        list of handlers so that it can perform operations from effect [t] as
        well as from the additional effects ['es]. *)

    val portable_fiber_with :
      'es Handler.List.Length.t @ local
      -> ((t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b) @ portable
      -> ('a Modes.Portable.t, ('b, 'es) Result.Portable.Contended.t, 'es) Continuation.t @ portable
    (** [portable_fiber_with l f] works the same as [fiber_with], but takes a
        [portable] closure and creates a [portable] continuation. *)

    val run : 
      (t Handler.t @ local portable -> 'a) 
      -> ('a, unit) Result.Contended.t
    (** [run f] constructs a continuation that runs the computation [f], and
        immediately continues it. [f] is passed a [t Handler.t] so that it can
        perform operations from effect [t]. *)

    val portable_run : 
      (t Handler.t @ local portable -> 'a) @ portable
      -> ('a, unit) Result.Portable.Contended.t @ portable
    (** [portable_run f] works the same as [run], but takes a [portable] closure
        and returns [Result.Portable.Contended.t] which can contain [portable] continuations. *)

    val run_with :
      'es Handler.List.t @ local portable
      -> ((t * 'es) Handler.List.t @ local portable -> 'a)
      -> ('a, 'es) Result.Contended.t
    (** [run_with hs f] constructs a continuation that runs the computation [f],
        and immediately continues it with handlers [hs]. [f] is passed a typed list
        of handlers so that it can perform operations from effect [t] as well as
        from the additional effects ['es]. *)

    val portable_run_with :
      'es Handler.List.t @ local portable
      -> ((t * 'es) Handler.List.t @ local portable -> 'a) @ portable
      -> ('a, 'es) Result.Portable.Contended.t @ portable
    (** [portable_run_with hs f] works the same as [run_with], but takes a
        [portable] closure and returns [Result.Portable.Contended.t] which can contain
        [portable] continuations. *)
        
    val perform : t Handler.t @ local contended -> ('a, t) ops @ portable -> 'a
    (** [perform h e] performs an effect [e] at the [contended] handler [h] *)
  end

  module Handler : sig

    type nonrec t = t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'es) t =
      ('a, ('b, 'es) Result.t, 'es) Continuation.t
  end

end

module type S1 = sig
  @@ portable

  type ('o, 'p, 'e) ops

  type 'p t

  module Result : sig

    type 'p eff := 'p t

    type ('a, 'p, 'es) t =
      | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
      | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
      | Operation :
          ('o, 'p, 'p eff) ops @@ global aliased
          * ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'es) t

    type ('a, 'p, 'es) handler =
      { handle :
          'o. ('o, 'p, 'p eff) ops
          -> ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'p, 'es) t -> ('a, 'p, 'es) handler -> 'a

    module Portable : sig
      type ('a, 'p, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
        | Operation :
            ('o, 'p, 'p eff) ops @@ global aliased
            * ('o, ('a, 'p, 'es) t, 'es) Continuation.t @@ portable
            -> ('a, 'p, 'es) t
            
      module Contended : sig
        type ('a, 'p, 'es) t =
          | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
          | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
          | Operation :
              ('o, 'p, 'p eff) ops @@ global aliased contended
              * ('o Modes.Portable.t, ('a, 'p, 'es) t, 'es) Continuation.t @@ portable
              -> ('a, 'p, 'es) t
      end
    end
    
    module Contended : sig
      type ('a, 'p, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
        | Operation :
            ('o, 'p, 'p eff) ops @@ global aliased contended
            * ('o Modes.Portable.t, ('a, 'p, 'es) t, 'es) Continuation.t
            -> ('a, 'p, 'es) t
    end
  end

  type ('a, 'p, 'es) result = ('a, 'p, 'es) Result.t =
    | Value : 'a @@ global aliased -> ('a, 'p, 'es) result
    | Exception : exn @@ global aliased -> ('a, 'p, 'es) result
    | Operation :
        ('o, 'p, 'p t) ops @@ global aliased
        * ('o, ('a, 'p, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'es) result

  val fiber :
    (local_ 'p t Handler.t -> 'a -> 'b)
    -> ('a, ('b, 'p, unit) Result.t, unit) Continuation.t

  val portable_fiber :
    (local_ 'p t Handler.t -> 'a -> 'b) @ portable
    -> ('a, ('b, 'p, unit) Result.Portable.t, unit) Continuation.t @ portable

  val fiber_with :
    local_ 'es Handler.List.Length.t
    -> (local_ ('p t * 'es) Handler.List.t -> 'a -> 'b)
    -> ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t

  val portable_fiber_with :
    local_ 'es Handler.List.Length.t
    -> (local_ ('p t * 'es) Handler.List.t -> 'a -> 'b) @ portable
    -> ('a, ('b, 'p, 'es) Result.Portable.t, 'es) Continuation.t @ portable

  val run : (local_ 'p t Handler.t -> 'a) -> ('a, 'p, unit) Result.t

  val portable_run :
    (local_ 'p t Handler.t -> 'a) @ portable
    -> ('a, 'p, unit) Result.Portable.t @ portable

  val run_with :
    local_ 'es Handler.List.t
    -> (local_ ('p t * 'es) Handler.List.t -> 'a)
    -> ('a, 'p, 'es) Result.t

  val portable_run_with :
    local_ 'es Handler.List.t
    -> (local_ ('p t * 'es) Handler.List.t -> 'a) @ portable
    -> ('a, 'p, 'es) Result.Portable.t @ portable

  val perform : local_ 'p t Handler.t -> ('a, 'p, 'p t) ops -> 'a

  module Portable : sig
    val fiber :
      ('p t Handler.t @ local portable -> 'a @ contended -> 'b)
      -> ('a Modes.Portable.t, ('b, 'p, unit) Result.Contended.t, unit) Continuation.t
    (** [fiber f] constructs a continuation that runs the computation [f]. [f]
        is passed a [t Handler.t] so that it can perform operations from effect
        [t]. *)

    val portable_fiber :
      ('p t Handler.t @ local portable -> 'a @ contended -> 'b) @ portable
      -> ('a Modes.Portable.t, ('b, 'p, unit) Result.Portable.Contended.t, unit) Continuation.t @ portable
    (** [portable_fiber f] works the same as [fiber], but takes a [portable]
        closure and creates a [portable] continuation. *)

    val fiber_with :
      'es Handler.List.Length.t @ local
      -> (('p t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b)
      -> ('a Modes.Portable.t, ('b, 'p, 'es) Result.Contended.t, 'es) Continuation.t
    (** [fiber_with l f] constructs a continuation that runs the computation [f],
        which requires handlers for [l] additional effects. [f] is passed a typed
        list of handlers so that it can perform operations from effect [t] as
        well as from the additional effects ['es]. *)

    val portable_fiber_with :
      'es Handler.List.Length.t @ local
      -> (('p t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b) @ portable
      -> ('a Modes.Portable.t, ('b, 'p, 'es) Result.Portable.Contended.t, 'es) Continuation.t @ portable
    (** [portable_fiber_with l f] works the same as [fiber_with], but takes a
        [portable] closure and creates a [portable] continuation. *)

    val run : 
      ('p t Handler.t @ local portable -> 'a) 
      -> ('a, 'p, unit) Result.Contended.t
    (** [run f] constructs a continuation that runs the computation [f], and
        immediately continues it. [f] is passed a [t Handler.t] so that it can
        perform operations from effect [t]. *)

    val portable_run : 
      ('p t Handler.t @ local portable -> 'a) @ portable
      -> ('a, 'p, unit) Result.Portable.Contended.t @ portable
    (** [portable_run f] works the same as [run], but takes a [portable] closure
        and returns [Result.Portable.Contended.t] which can contain [portable] continuations. *)

    val run_with :
      'es Handler.List.t @ local portable
      -> (('p t * 'es) Handler.List.t @ local portable -> 'a)
      -> ('a, 'p, 'es) Result.Contended.t
    (** [run_with hs f] constructs a continuation that runs the computation [f],
        and immediately continues it with handlers [hs]. [f] is passed a typed list
        of handlers so that it can perform operations from effect [t] as well as
        from the additional effects ['es]. *)

    val portable_run_with :
      'es Handler.List.t @ local portable
      -> (('p t * 'es) Handler.List.t @ local portable -> 'a) @ portable
      -> ('a, 'p, 'es) Result.Portable.Contended.t @ portable
    (** [portable_run_with hs f] works the same as [run_with], but takes a
        [portable] closure and returns [Result.Portable.Contended.t] which can contain
        [portable] continuations. *)
        
    val perform : 'p t Handler.t @ local contended -> ('a, 'p, 'p t) ops @ portable -> 'a  
    (** [perform h e] performs an effect [e] at the [contended] handler [h] *)
  end

  module Handler : sig

    type nonrec 'p t = 'p t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'p, 'es) t =
      ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t
  end

end

module type S2 = sig
  @@ portable

  type ('o, 'p, 'q, 'e) ops

  type ('p, 'q) t

  module Result : sig

    type ('p, 'q) eff := ('p, 'q) t

    type ('a, 'p, 'q, 'es) t =
      | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
      | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
      | Operation :
          ('o, 'p, 'q, ('p, 'q) eff) ops @@ global aliased
          * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'q, 'es) t

    type ('a, 'p, 'q, 'es) handler =
      { handle :
          'o. ('o, 'p, 'q, ('p, 'q) eff) ops
          -> ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'p, 'q, 'es) t -> ('a, 'p, 'q, 'es) handler -> 'a

    module Portable : sig
      type ('a, 'p, 'q, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Operation :
            ('o, 'p, 'q, ('p, 'q) eff) ops @@ global aliased
            * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t @@ portable
            -> ('a, 'p, 'q, 'es) t
            
      module Contended : sig
        type ('a, 'p, 'q, 'es) t =
          | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
          | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
          | Operation :
              ('o, 'p, 'q, ('p, 'q) eff) ops @@ global aliased contended
              * ('o Modes.Portable.t, ('a, 'p, 'q, 'es) t, 'es) Continuation.t @@ portable
              -> ('a, 'p, 'q, 'es) t
      end
    end
    
    module Contended : sig
      type ('a, 'p, 'q, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Operation :
            ('o, 'p, 'q, ('p, 'q) eff) ops @@ global aliased contended
            * ('o Modes.Portable.t, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
            -> ('a, 'p, 'q, 'es) t
    end
  end

  type ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) Result.t =
    | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) result
    | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) result
    | Operation :
        ('o, 'p, 'q, ('p, 'q) t) ops @@ global aliased
        * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'q, 'es) result

  val fiber :
    (local_ ('p, 'q) t Handler.t -> 'a -> 'b)
    -> ('a, ('b, 'p, 'q, unit) result, unit) Continuation.t

  val portable_fiber :
    (local_ ('p, 'q) t Handler.t -> 'a -> 'b) @ portable
    -> ('a, ('b, 'p, 'q, unit) Result.Portable.t, unit) Continuation.t @ portable

  val fiber_with :
    local_ 'es Handler.List.Length.t
    -> (local_ (('p, 'q) t * 'es) Handler.List.t -> 'a -> 'b)
    -> ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t

  val portable_fiber_with :
    local_ 'es Handler.List.Length.t
    -> (local_ (('p, 'q) t * 'es) Handler.List.t -> 'a -> 'b) @ portable
    -> ('a, ('b, 'p, 'q, 'es) Result.Portable.t, 'es) Continuation.t @ portable

  val run :
    (local_ ('p, 'q) t Handler.t -> 'a)
    -> ('a, 'p, 'q, unit) result

  val portable_run :
    (local_ ('p, 'q) t Handler.t -> 'a) @ portable
    -> ('a, 'p, 'q, unit) Result.Portable.t @ portable

  val run_with :
    local_ 'es Handler.List.t
    -> (local_ (('p, 'q) t * 'es) Handler.List.t -> 'a)
    -> ('a, 'p, 'q, 'es) result

  val portable_run_with :
    local_ 'es Handler.List.t
    -> (local_ (('p, 'q) t * 'es) Handler.List.t -> 'a) @ portable
    -> ('a, 'p, 'q, 'es) Result.Portable.t @ portable

  val perform :
    local_ ('p, 'q) t Handler.t
    -> ('a, 'p, 'q, ('p, 'q) t) ops
    -> 'a

  module Portable : sig
    val fiber :
      (('p, 'q) t Handler.t @ local portable -> 'a @ contended -> 'b)
      -> ('a Modes.Portable.t, ('b, 'p, 'q, unit) Result.Contended.t, unit) Continuation.t
    (** [fiber f] constructs a continuation that runs the computation [f]. [f]
        is passed a [t Handler.t] so that it can perform operations from effect
        [t]. *)

    val portable_fiber :
      (('p, 'q) t Handler.t @ local portable -> 'a @ contended -> 'b) @ portable
      -> ('a Modes.Portable.t, ('b, 'p, 'q, unit) Result.Portable.Contended.t, unit) Continuation.t @ portable
    (** [portable_fiber f] works the same as [fiber], but takes a [portable]
        closure and creates a [portable] continuation. *)

    val fiber_with :
      'es Handler.List.Length.t @ local
      -> ((('p, 'q) t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b)
      -> ('a Modes.Portable.t, ('b, 'p, 'q, 'es) Result.Contended.t, 'es) Continuation.t
    (** [fiber_with l f] constructs a continuation that runs the computation [f],
        which requires handlers for [l] additional effects. [f] is passed a typed
        list of handlers so that it can perform operations from effect [t] as
        well as from the additional effects ['es]. *)

    val portable_fiber_with :
      'es Handler.List.Length.t @ local
      -> ((('p, 'q) t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b) @ portable
      -> ('a Modes.Portable.t, ('b, 'p, 'q, 'es) Result.Portable.Contended.t, 'es) Continuation.t @ portable
    (** [portable_fiber_with l f] works the same as [fiber_with], but takes a
        [portable] closure and creates a [portable] continuation. *)

    val run : 
      (('p, 'q) t Handler.t @ local portable -> 'a) 
      -> ('a, 'p, 'q, unit) Result.Contended.t
    (** [run f] constructs a continuation that runs the computation [f], and
        immediately continues it. [f] is passed a [t Handler.t] so that it can
        perform operations from effect [t]. *)

    val portable_run : 
      (('p, 'q) t Handler.t @ local portable -> 'a) @ portable
      -> ('a, 'p, 'q, unit) Result.Portable.Contended.t @ portable
    (** [portable_run f] works the same as [run], but takes a [portable] closure
        and returns [Result.Portable.Contended.t] which can contain [portable] continuations. *)

    val run_with :
      'es Handler.List.t @ local portable
      -> ((('p, 'q) t * 'es) Handler.List.t @ local portable -> 'a)
      -> ('a, 'p, 'q, 'es) Result.Contended.t
    (** [run_with hs f] constructs a continuation that runs the computation [f],
        and immediately continues it with handlers [hs]. [f] is passed a typed list
        of handlers so that it can perform operations from effect [t] as well as
        from the additional effects ['es]. *)

    val portable_run_with :
      'es Handler.List.t @ local portable
      -> ((('p, 'q) t * 'es) Handler.List.t @ local portable -> 'a) @ portable
      -> ('a, 'p, 'q, 'es) Result.Portable.Contended.t @ portable
    (** [portable_run_with hs f] works the same as [run_with], but takes a
        [portable] closure and returns [Result.Portable.Contended.t] which can contain
        [portable] continuations. *)
        
    val perform : ('p, 'q) t Handler.t @ local contended -> ('a, 'p, 'q, ('p, 'q) t) ops @ portable -> 'a
    (** [perform h e] performs an effect [e] at the [contended] handler [h] *)
  end

  module Handler : sig

    type nonrec ('p, 'q) t = ('p, 'q) t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'p, 'q, 'es) t =
      ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t
  end

end

module type Operations = sig

  type 'a t

end

module type Operations_rec = sig

  type ('a, 'e) t

end

module type Operations1 = sig

  type ('a, 'p) t

end

module type Operations1_rec = sig

  type ('a, 'p, 'e) t

end

module type Operations2 = sig

  type ('a, 'p, 'q) t

end

module type Operations2_rec = sig

  type ('a, 'p, 'q, 'e) t

end

module Make_rec (Ops : Operations_rec)
  : S with type ('a, 'e) ops := ('a, 'e) Ops.t
= struct

  type t

  module Result = struct

    type eff = t

    type ('e, 'es) t =
    | Value : 'a @@ global aliased -> ('a, 'es) t
    | Exception : exn @@ global aliased -> ('a, 'es) t
    | Operation :
        ('o, eff) Ops.t @@ global aliased
        * ('o, ('a, 'es) t, 'es) Continuation.t
        -> ('a, 'es) t

    type ('a, 'es) handler =
      { handle : 'o. ('o, eff) Ops.t -> ('o, ('a, 'es) t, 'es) Continuation.t -> 'a }
      [@@unboxed]

    let handle r {handle} =
      match r with
      | Value x -> x
      | Exception e -> raise e
      | Operation(op, k) -> handle op k

    module Portable = struct
      type ('a, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'es) t
        | Operation :
            ('o, eff) Ops.t @@ global aliased
            * ('o, ('a, 'es) t, 'es) Continuation.t @@ portable
            -> ('a, 'es) t
            
      module Contended = struct
        type ('a, 'es) t =
          | Value : 'a @@ global aliased -> ('a, 'es) t
          | Exception : exn @@ global aliased -> ('a, 'es) t
          | Operation :
              ('o, eff) Ops.t @@ global aliased contended
              * ('o Modes.Portable.t, ('a, 'es) t, 'es) Continuation.t @@ portable
              -> ('a, 'es) t
      end
    end
    
    module Contended = struct
      type ('a, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'es) t
        | Operation :
            ('o, eff) Ops.t @@ global aliased contended
            * ('o Modes.Portable.t, ('a, 'es) t, 'es) Continuation.t
            -> ('a, 'es) t
    end
  end

  type ('a, 'es) result = ('a, 'es) Result.t =
    | Value : 'a @@ global aliased -> ('a, 'es) result
    | Exception : exn @@ global aliased -> ('a, 'es) result
    | Operation :
        ('o, t) Ops.t @@ global aliased
        * ('o, ('a, 'es) result, 'es) Continuation.t
        -> ('a, 'es) result

  (* Here and below, [portable] versions of functions are defined before
     non-[portable] ones to avoid shadowing the original functions. *)

  module Portable = struct
    let portable_fiber (type a b) f =
    (* CR dkalinichenko: here and below, we use [magic_portable]
       to indicate that the handler is [portable] when it receives
       [contended] values.
      
       We may be able to rewrite this to avoid magic. *)
      let f h a = f (Obj.magic_portable h) a [@nontail] in
      let k : (a Modes.Portable.t, b, t, unit) continuation = Obj.magic (fiber f) in
    (* CR dkalinichenko: here and below, unsafe use of [Obj.magic_portable]:
       the continuation contains mutable data [Mapping.t].
       
       Require [f] to be unique. *)
      Obj.magic_portable
        (Continuation k : (a Modes.Portable.t, (b, unit)
          Result.Portable.Contended.t, unit) Continuation.t)

    let fiber (type a b) f =
      let f h a = f (Obj.magic_portable h) a [@nontail] in
      let k : (a Modes.Portable.t, b, t, unit) continuation = Obj.magic (fiber f) in
      (Continuation k : (a Modes.Portable.t, (b, unit)
        Result.Contended.t, unit) Continuation.t)

    let portable_fiber_with (type a b es) l f =
      let f hs a = f (Obj.magic_portable hs) a [@nontail] in
      let k : (a Modes.Portable.t, b, t, es) continuation = Obj.magic (fiber_with l f) in
      Obj.magic_portable
        (Continuation k : (a Modes.Portable.t, (b, es)
          Result.Portable.Contended.t, es) Continuation.t)
    
    let fiber_with (type a b es) l f =
      let f hs a = f (Obj.magic_portable hs) a [@nontail] in
      let k : (a Modes.Portable.t, b, t, es) continuation = Obj.magic (fiber_with l f) in
      (Continuation k : (a Modes.Portable.t, (b, es)
        Result.Contended.t, es) Continuation.t)
          
    let portable_run (type a) f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      let res : (a, unit) Result.t = Obj.magic (run f) in
      Obj.magic_portable (Obj.magic res : (a, unit) Result.Portable.Contended.t)
    
    let run (type a) f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      let res : (a, unit) Result.t = Obj.magic (run f) in
      (Obj.magic res : (a, unit) Result.Contended.t)
      
    let portable_run_with (type a es) hs f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      let res : (a, es) Result.t = Obj.magic (run_with hs f) in
      Obj.magic_portable (Obj.magic res : (a, es) Result.Portable.Contended.t)
    
    let run_with (type a es) hs f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      let res : (a, es) Result.t = Obj.magic (run_with hs f) in
      (Obj.magic res : (a, es) Result.Contended.t)
      
    let perform (type a) (h : t Handler.t) op : a =
      let op : (a, t) op = Obj.magic op in
      (* CR dkalinichenko: here and below, we use [Obj.magic_uncontended]
         to mark a [contended] handler safe to use with [portable]
         operations. 
         
         We may be able to rewrite this to avoid magic. *)
      perform (op, Obj.magic_uncontended h.h)
  end

  let portable_fiber (type a b) f =
    let k : (a, b, t, unit) continuation = Obj.magic (fiber f) in
    Obj.magic_portable
      (Continuation k : (a, (b, unit) Result.Portable.t, unit) Continuation.t)

  let fiber (type a b) f =
    let k : (a, b, t, unit) continuation = fiber f in
    (Continuation k : (a, (b, unit) Result.t, unit) Continuation.t)

  let portable_fiber_with (type a b es) (local_ hs) f =
    let k : (a, b, t, es) continuation = Obj.magic (fiber_with hs f) in
    Obj.magic_portable
      (Continuation k : (a, (b, es) Result.Portable.t, es) Continuation.t)

  let fiber_with (type a b es) (local_ hs) f =
    let k : (a, b, t, es) continuation = fiber_with hs f in
    (Continuation k : (a, (b, es) Result.t, es) Continuation.t)

  let portable_run (type a) f =
    let res : (a, t, unit) res = run f in
    Obj.magic_portable (Obj.magic res : (a, unit) Result.Portable.t)

  let run (type a) f =
    let res : (a, t, unit) res = run f in
    (Obj.magic res : (a, unit) Result.t)

  let portable_run_with (type a es) (local_ hs) f =
    let res : (a, t, es) res = run_with hs f in
    Obj.magic_portable (Obj.magic res : (a, es) Result.Portable.t)

  let run_with (type a es) (local_ hs) f =
    let res : (a, t, es) res = run_with hs f in
    (Obj.magic res : (a, es) Result.t)

  let perform (type a) (local_ h : _ Handler.t) (op : (a, t) Ops.t) =
    let op : (a, t) op = Obj.magic op in
    perform (op, h.h)

  module Handler = struct

    type nonrec t = t Handler.t

  end

  module Continuation = struct

    type ('a, 'b, 'es) t =
      ('a, ('b, 'es) Result.t, 'es) Continuation.t
  end

end

module Make (Ops : Operations)
  : S with type ('a, 'e) ops := 'a Ops.t
  = Make_rec(struct type ('a, 'e) t = 'a Ops.t end)

module Make1_rec (Ops : Operations1_rec)
  : S1 with type ('a, 'p, 'e) ops := ('a, 'p, 'e) Ops.t
= struct

  type 'p t

  module Result = struct

    type 'p eff = 'p t

    type ('a, 'p, 'es) t =
      | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
      | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
      | Operation :
          ('o, 'p, 'p eff) Ops.t @@ global aliased
          * ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'es) t

    type ('a, 'p, 'es) handler =
      { handle :
          'o. ('o, 'p, 'p eff) Ops.t
          -> ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    let handle r {handle} =
      match r with
      | Value x -> x
      | Exception e -> raise e
      | Operation(op, k) -> handle op k

    module Portable = struct
      type ('a, 'p, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
        | Operation :
            ('o, 'p, 'p eff) Ops.t @@ global aliased
            * ('o, ('a, 'p, 'es) t, 'es) Continuation.t @@ portable
            -> ('a, 'p, 'es) t
            
      module Contended = struct
        type ('a, 'p, 'es) t =
          | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
          | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
          | Operation :
              ('o, 'p, 'p eff) Ops.t @@ global aliased contended
              * ('o Modes.Portable.t, ('a, 'p, 'es) t, 'es) Continuation.t @@ portable
              -> ('a, 'p, 'es) t
      end
    end
    
    module Contended = struct
      type ('a, 'p, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'es) t
        | Operation :
            ('o, 'p, 'p eff) Ops.t @@ global aliased contended
            * ('o Modes.Portable.t, ('a, 'p, 'es) t, 'es) Continuation.t
            -> ('a, 'p, 'es) t
    end
  end

  type ('a, 'p, 'es) result = ('a, 'p, 'es) Result.t =
    | Value : 'a @@ global aliased -> ('a, 'p, 'es) result
    | Exception : exn @@ global aliased -> ('a, 'p, 'es) result
    | Operation :
        ('o, 'p, 'p t) Ops.t @@ global aliased
        * ('o, ('a, 'p, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'es) result

  module Portable = struct
    let portable_fiber (type a b p) f =
      let f h a = f (Obj.magic_portable h) a [@nontail] in
      let k : (a Modes.Portable.t, b, p t, unit) continuation = Obj.magic (fiber f) in
      Obj.magic_portable
        (Continuation k : (a Modes.Portable.t, (b, p, unit)
          Result.Portable.Contended.t, unit) Continuation.t)
    
    let fiber (type a b p) f =
      let f h a = f (Obj.magic_portable h) a [@nontail] in
      let k : (a Modes.Portable.t, b, p t, unit) continuation = Obj.magic (fiber f) in
      (Continuation k : (a Modes.Portable.t, (b, p, unit)
        Result.Contended.t, unit) Continuation.t)
    
    let portable_fiber_with (type a b p es) l f =
      let f hs a = f (Obj.magic_portable hs) a [@nontail] in
      let k : (a Modes.Portable.t, b, p t, es) continuation = Obj.magic (fiber_with l f) in
      Obj.magic_portable
        (Continuation k : (a Modes.Portable.t, (b, p, es)
          Result.Portable.Contended.t, es) Continuation.t)
    
    let fiber_with (type a b p es) l f =
      let f hs a = f (Obj.magic_portable hs) a [@nontail] in
      let k : (a Modes.Portable.t, b, p t, es) continuation = Obj.magic (fiber_with l f) in
      (Continuation k : (a Modes.Portable.t, (b, p, es)
        Result.Contended.t, es) Continuation.t)
    
    let portable_run (type a p) f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      let res : (a, p, unit) Result.t = Obj.magic (run f) in
      Obj.magic_portable (Obj.magic res : (a, p, unit) Result.Portable.Contended.t)
    
    let run (type a p) f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      let res : (a, p, unit) Result.t = Obj.magic (run f) in
      (Obj.magic res : (a, p, unit) Result.Contended.t)
    
    let portable_run_with (type a p es) hs f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      let res : (a, p, es) Result.t = Obj.magic (run_with hs f) in
      Obj.magic_portable (Obj.magic res : (a, p, es) Result.Portable.Contended.t)
    
    let run_with (type a p es) hs f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      let res : (a, p, es) Result.t = Obj.magic (run_with hs f) in
      (Obj.magic res : (a, p, es) Result.Contended.t)
      
    let perform (type a p) (h : p t Handler.t) op : a =
      let op : (a, p t) op = Obj.magic op in
      perform (op, Obj.magic_uncontended h.h)
  end

  let portable_fiber (type a b p) f =
    let k : (a, b, p t, unit) continuation = Obj.magic (fiber f) in
    Obj.magic_portable
      (Continuation k : (a, (b, p, unit) Result.Portable.t, unit) Continuation.t)

  let fiber (type a b p) f =
    let k : (a, b, p t, unit) continuation = fiber f in
    (Continuation k : (a, (b, p, unit) Result.t, unit) Continuation.t)

  let fiber_with (type a b p es) (local_ hs) f =
    let k : (a, b, p t, es) continuation = fiber_with hs f in
    (Continuation k : (a, (b, p, es) Result.t, es) Continuation.t)

  let portable_fiber_with (type a b p es) (local_ hs) f =
    let k : (a, b, p t, es) continuation = Obj.magic (fiber_with hs f) in
    Obj.magic_portable
      (Continuation k : (a, (b, p, es) Result.Portable.t, es) Continuation.t)

  let portable_run (type a p) f =
    let res : (a, p t, unit) res = run f in
    Obj.magic_portable (Obj.magic res : (a, p, unit) Result.Portable.t)

  let run (type a p) f =
    let res : (a, p t, unit) res = run f in
    (Obj.magic res : (a, p, unit) Result.t)

  let portable_run_with (type a p es) (local_ hs) f =
    let res : (a, p t, es) res = run_with hs f in
    Obj.magic_portable (Obj.magic res : (a, p, es) Result.Portable.t)

  let run_with (type a p es) (local_ hs) f =
    let res : (a, p t, es) res = run_with hs f in
    (Obj.magic res : (a, p, es) Result.t)

  let perform (type a p) (local_ h : _ Handler.t) (op : (a, p, p t) Ops.t) =
    let op : (a, p t) op = Obj.magic op in
    perform (op, h.h)

  module Handler = struct

    type nonrec 'p t = 'p t Handler.t

  end

  module Continuation = struct

    type ('a, 'b, 'p, 'es) t =
      ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t
  end

end

module Make1 (Ops : Operations1)
  : S1 with type ('a, 'p, 'e) ops := ('a, 'p) Ops.t
  = Make1_rec(struct type ('a, 'p, 'e) t = ('a, 'p) Ops.t end)

module Make2_rec (Ops : Operations2_rec)
  : S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Ops.t
= struct

  type ('p, 'q) t

  module Result = struct

    type ('p, 'q) eff = ('p, 'q) t

    type ('a, 'p, 'q, 'es) t =
      | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
      | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
      | Operation :
          ('o, 'p, 'q, ('p, 'q) eff) Ops.t @@ global aliased
          * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'q, 'es) t

    type ('a, 'p, 'q, 'es) handler =
      { handle :
          'o. ('o, 'p, 'q, ('p, 'q) eff) Ops.t
          -> ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    let handle r {handle} =
      match r with
      | Value x -> x
      | Exception e -> raise e
      | Operation(op, k) -> handle op k

    module Portable = struct
      type ('a, 'p, 'q, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Operation :
            ('o, 'p, 'q, ('p, 'q) eff) Ops.t @@ global aliased
            * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t @@ portable
            -> ('a, 'p, 'q, 'es) t
            
      module Contended = struct
        type ('a, 'p, 'q, 'es) t =
          | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
          | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
          | Operation :
              ('o, 'p, 'q, ('p, 'q) eff) Ops.t @@ global aliased contended
              * ('o Modes.Portable.t, ('a, 'p, 'q, 'es) t, 'es) Continuation.t @@ portable
              -> ('a, 'p, 'q, 'es) t
      end
    end
    
    module Contended = struct
      type ('a, 'p, 'q, 'es) t =
        | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) t
        | Operation :
            ('o, 'p, 'q, ('p, 'q) eff) Ops.t @@ global aliased contended
            * ('o Modes.Portable.t, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
            -> ('a, 'p, 'q, 'es) t
    end
  end

  type ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) Result.t =
    | Value : 'a @@ global aliased -> ('a, 'p, 'q, 'es) result
    | Exception : exn @@ global aliased -> ('a, 'p, 'q, 'es) result
    | Operation :
        ('o, 'p, 'q, ('p, 'q) t) Ops.t @@ global aliased
        * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'q, 'es) result


  module Portable = struct
    let portable_fiber (type a b p q) f =
      let f h a = f (Obj.magic_portable h) a [@nontail] in
      let k : (a Modes.Portable.t, b, (p, q) t, unit) continuation = Obj.magic (fiber f) in
      Obj.magic_portable
        (Continuation k : (a Modes.Portable.t, (b, p, q, unit)
          Result.Portable.Contended.t, unit) Continuation.t)
    
    let fiber (type a b p q) f =
      let f h a = f (Obj.magic_portable h) a [@nontail] in
      let k : (a Modes.Portable.t, b, (p, q) t, unit) continuation = Obj.magic (fiber f) in
      (Continuation k : (a Modes.Portable.t, (b, p, q, unit)
        Result.Contended.t, unit) Continuation.t)
    
    let portable_fiber_with (type a b p q es) l f =
      let f hs a = f (Obj.magic_portable hs) a [@nontail] in
      let k : (a Modes.Portable.t, b, (p, q) t, es) continuation = Obj.magic (fiber_with l f) in
      Obj.magic_portable
        (Continuation k : (a Modes.Portable.t, (b, p, q, es)
          Result.Portable.Contended.t, es) Continuation.t)
    
    let fiber_with (type a b p q es) l f =
      let f hs a = f (Obj.magic_portable hs) a [@nontail] in
      let k : (a Modes.Portable.t, b, (p, q) t, es) continuation = Obj.magic (fiber_with l f) in
      (Continuation k : (a Modes.Portable.t, (b, p, q, es)
        Result.Contended.t, es) Continuation.t)
    
    let portable_run (type a p q) f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      let res : (a, p, q, unit) Result.t = Obj.magic (run f) in
      Obj.magic_portable (Obj.magic res : (a, p, q, unit) Result.Portable.Contended.t)
    
    let run (type a p q) f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      let res : (a, p, q, unit) Result.t = Obj.magic (run f) in
      (Obj.magic res : (a, p, q, unit) Result.Contended.t)
    
    let portable_run_with (type a p q es) hs f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      let res : (a, p, q, es) Result.t = Obj.magic (run_with hs f) in
      Obj.magic_portable (Obj.magic res : (a, p, q, es) Result.Portable.Contended.t)
    
    let run_with (type a p q es) hs f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      let res : (a, p, q, es) Result.t = Obj.magic (run_with hs f) in
      (Obj.magic res : (a, p, q, es) Result.Contended.t)
      
    let perform (type a p q) (h : (p, q) t Handler.t) op : a =
      let op : (a, (p, q) t) op = Obj.magic op in
      perform (op, Obj.magic_uncontended h.h)
  end

  let portable_fiber (type a p q b) f =
    let k : (a, b, (p, q) t, unit) continuation = Obj.magic (fiber f) in
    Obj.magic_portable
      (Continuation k : (a, (b, p, q, unit) Result.Portable.t, unit) Continuation.t)

  let fiber (type a p q b) f =
    let k : (a, b, (p, q) t, unit) continuation = fiber f in
    (Continuation k : (a, (b, p, q, unit) result, unit) Continuation.t)

  let portable_fiber_with (type a p q b es) (local_ hs) f =
    let k : (a, b, (p, q) t, es) continuation = Obj.magic (fiber_with hs f) in
    Obj.magic_portable
      (Continuation k : (a, (b, p, q, es) Result.Portable.t, es) Continuation.t)

  let fiber_with (type a p q b es) (local_ hs) f =
    let k : (a, b, (p, q) t, es) continuation = fiber_with hs f in
    (Continuation k : (a, (b, p, q, es) result, es) Continuation.t)
      
  let portable_run (type a p q) f =
    let res : (a, (p, q) t, unit) res = run f in
    Obj.magic_portable (Obj.magic res : (a, p, q, unit) Result.Portable.t)

  let run (type a p q) f =
    let res : (a, (p, q) t, unit) res = run f in
    (Obj.magic res : (a, p, q, unit) result)

  let portable_run_with (type a p q es) (local_ hs) f =
    let res : (a, (p, q) t, es) res = run_with hs f in
    Obj.magic_portable (Obj.magic res : (a, p, q, es) Result.Portable.t)

  let run_with (type a p q es) (local_ hs) f =
    let res : (a, (p, q) t, es) res = run_with hs f in
    (Obj.magic res : (a, p, q, es) result)

  let perform (type a p q) (local_ h : _ Handler.t) (op : (a, p, q, (p, q) t) Ops.t) =
    let op : (a, (p, q) t) op = Obj.magic op in
    perform (op, h.h)

  module Handler = struct

    type nonrec ('p, 'q) t = ('p, 'q) t Handler.t

  end

  module Continuation = struct

    type ('a, 'b, 'p, 'q, 'es) t =
      ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t
  end

end

module Make2 (Ops : Operations2)
  : S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q) Ops.t
  = Make2_rec(struct type ('a, 'p, 'q, 'e) t = ('a, 'p, 'q) Ops.t end)

exception Continuation_already_resumed
type exn += Unhandled : 'e Handler.t -> exn

(* Register the exceptions so that the runtime can access it *)
let _ = Callback.register_exception "Effect.Unhandled"
          (Unhandled { h = Handler.Dummy})
let _ = Callback.register_exception "Effect.Continuation_already_resumed"
          Continuation_already_resumed
