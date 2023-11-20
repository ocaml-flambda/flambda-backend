# 1 "camlinternalLazy.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

(* Internals of forcing lazy values. *)

[@@@ocaml.afl_inst_ratio 0]

type 'a t = 'a lazy_t

exception Undefined

module Lazy5 = struct
  (* [update_to_forcing blk] tries to update a [blk] with [lazy_tag] to
    [forcing_tag] using compare-and-swap (CAS), taking care to handle concurrent
    marking of the header word by a concurrent GC thread. Returns [0] if the
    CAS is successful. If the CAS fails, then the tag was observed to be
    something other than [lazy_tag] due to a concurrent mutator. In this case,
    the function returns [1]. *)
  external update_to_forcing : Obj.t -> int =
    "caml_lazy_update_to_forcing" [@@noalloc]

  (* [reset_to_lazy blk] expects [blk] to be a lazy object with [Obj.forcing_tag]
    and updates the tag to [Obj.lazy_tag], taking care to handle concurrent
    marking of this object's header by a concurrent GC thread. *)
  external reset_to_lazy : Obj.t -> unit = "caml_lazy_reset_to_lazy" [@@noalloc]

  (* [update_to_forward blk] expects [blk] to be a lazy object with
    [Obj.forcing_tag] and updates the tag to [Obj.forward_tag], taking care to
    handle concurrent marking of this object's header by a concurrent GC thread.
  *)
  external update_to_forward : Obj.t -> unit =
    "caml_lazy_update_to_forward" [@@noalloc]

  (* Assumes [blk] is a block with tag forcing *)
  let do_force_block blk =
    let b = Obj.repr blk in
    let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
    Obj.set_field b 0 (Obj.repr ()); (* Release the closure *)
    try
      let result = closure () in
      Obj.set_field b 0 (Obj.repr result);
      update_to_forward b;
      result
    with e ->
      Obj.set_field b 0 (Obj.repr (fun () -> raise e));
      reset_to_lazy b;
      raise e

  (* Assumes [blk] is a block with tag forcing *)
  let do_force_val_block blk =
    let b = Obj.repr blk in
    let closure = (Obj.obj (Obj.field b 0) : unit -> 'arg) in
    Obj.set_field b 0 (Obj.repr ()); (* Release the closure *)
    let result = closure () in
    Obj.set_field b 0 (Obj.repr result);
    update_to_forward b;
    result

  (* Called by [force_gen] *)
  let force_gen_lazy_block ~only_val (blk : 'arg lazy_t) =
    (* We expect the tag to be [lazy_tag], but may be other tags due to
      concurrent forcing of lazy values. *)
    match update_to_forcing (Obj.repr blk) with
    | 0 when only_val -> do_force_val_block blk
    | 0 -> do_force_block blk
    | _ -> raise Undefined

  (* used in the %lazy_force primitive *)
  let force_lazy_block blk = force_gen_lazy_block ~only_val:false blk

  (* [force_gen ~only_val:false] is not used, since [Lazy.force] is
    declared as a primitive whose code inlines the tag tests of its
    argument, except when afl instrumentation is turned on. *)
  let force_gen ~only_val (lzv : 'arg lazy_t) =
    (* Using [Sys.opaque_identity] prevents two potential problems:
      - If the value is known to have Forward_tag, then it could have been
        shortcut during GC, so that information must be forgotten (see GPR#713
        and issue #7301). This is not an issue here at the moment since
        [Obj.tag] is not simplified by the compiler, and GPR#713 also
        ensures that no value will be known to have Forward_tag.
      - If the value is known to be immutable, then if the compiler
        cannot prove that the last branch is not taken it will issue a
        warning 59 (modification of an immutable value) *)
    let lzv = Sys.opaque_identity lzv in
    let x = Obj.repr lzv in
    (* START no safe points. If a GC occurs here, then the object [x] may be
      short-circuited, and getting the first field of [x] would get us the wrong
      value. Luckily, the compiler does not insert GC safe points at this place,
      so it is ok. *)
    let t = Obj.tag x in
    if t = Obj.forward_tag then
      (Obj.obj (Obj.field x 0) : 'arg)
    (* END no safe points *)
    else if t = Obj.forcing_tag then raise Undefined
    else if t <> Obj.lazy_tag then (Obj.obj x : 'arg)
    else force_gen_lazy_block ~only_val lzv
end

(* CR ocaml 5 runtime: delete the old implementation *)
module Lazy4 = struct
  let raise_undefined = Obj.repr (fun () -> raise Undefined)

  external make_forward : Obj.t -> Obj.t -> unit = "caml_obj_make_forward"

  (* Assume [blk] is a block with tag lazy *)
  let force_lazy_block (blk : 'arg lazy_t) =
    let closure = (Obj.obj (Obj.field (Obj.repr blk) 0) : unit -> 'arg) in
    Obj.set_field (Obj.repr blk) 0 raise_undefined;
    try
      let result = closure () in
      make_forward (Obj.repr blk) (Obj.repr result);
      result
    with e ->
      Obj.set_field (Obj.repr blk) 0 (Obj.repr (fun () -> raise e));
      raise e


  (* Assume [blk] is a block with tag lazy *)
  let force_val_lazy_block (blk : 'arg lazy_t) =
    let closure = (Obj.obj (Obj.field (Obj.repr blk) 0) : unit -> 'arg) in
    Obj.set_field (Obj.repr blk) 0 raise_undefined;
    let result = closure () in
    make_forward (Obj.repr blk) (Obj.repr result);
    result


  (* [force] is not used, since [Lazy.force] is declared as a primitive
    whose code inlines the tag tests of its argument, except when afl
    instrumentation is turned on. *)

  let force (lzv : 'arg lazy_t) =
    (* Using [Sys.opaque_identity] prevents two potential problems:
      - If the value is known to have Forward_tag, then its tag could have
        changed during GC, so that information must be forgotten (see GPR#713
        and issue #7301)
      - If the value is known to be immutable, then if the compiler
        cannot prove that the last branch is not taken it will issue a
        warning 59 (modification of an immutable value) *)
    let lzv = Sys.opaque_identity lzv in
    let x = Obj.repr lzv in
    let t = Obj.tag x in
    if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
    if t <> Obj.lazy_tag then (Obj.obj x : 'arg)
    else force_lazy_block lzv


  let force_val (lzv : 'arg lazy_t) =
    let x = Obj.repr lzv in
    let t = Obj.tag x in
    if t = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg) else
    if t <> Obj.lazy_tag then (Obj.obj x : 'arg)
    else force_val_lazy_block lzv
end

external runtime5 : unit -> bool = "%runtime5"
let runtime5 = runtime5 ()

let force_lazy_block =
  if runtime5 then Lazy5.force_lazy_block else Lazy4.force_lazy_block

let force_gen ~only_val l =
  if runtime5 then Lazy5.force_gen ~only_val l
  else if only_val then Lazy4.force_val l
  else Lazy4.force l

let force l =
  if runtime5 then Lazy5.force_gen ~only_val:false l
  else Lazy4.force l

let force_val l =
  if runtime5 then Lazy5.force_gen ~only_val:true l
  else Lazy4.force_val l
