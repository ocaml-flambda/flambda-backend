(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Anmol Sahoo, Purdue University                     *)
(*                        Olivier Nicole, Tarides                         *)
(*                         Fabrice Buoro, Tarides                         *)
(*                                                                        *)
(*   Copyright 2023 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@warning "@4"]

open Asttypes
open Cmm
module V = Backend_var
module VP = Backend_var.With_provenance

type read_or_write =
  | Read
  | Write

let cextcall (func, ty, ty_args, alloc) =
  Cextcall { func; ty; ty_args; alloc;
             builtin = false; (* XXX *)
             returns = true; (* XXX *)
             effects = Arbitrary_effects; (* XXX *)
             coeffects = Has_coeffects; (* XXX *)
           }

let init_code () =
  Cmm_helpers.return_unit Debuginfo.none
    (Cop (cextcall ("__tsan_init", typ_void, [], false), [], Debuginfo.none))

let bit_size memory_chunk =
  match memory_chunk with
  | Byte_unsigned | Byte_signed -> 8
  | Sixteen_unsigned | Sixteen_signed -> 16
  | Thirtytwo_unsigned | Thirtytwo_signed -> 32
  | Word_int | Word_val -> Sys.word_size
  | Single { reg = Float32; } -> 32
  | Single { reg = Float64; } -> 64
  | Double -> 64
  | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
    128

let select_function read_or_write memory_chunk =
  let bit_size = bit_size memory_chunk in
  let acc_string =
    match read_or_write with Read -> "read" | Write -> "write"
  in
  Printf.sprintf "__tsan_%s%d" acc_string (bit_size / 8)

module TSan_memory_order = struct
  (* Constants defined in the LLVM ABI *)
  (*let relaxed = Cconst_int (0, Debuginfo.none)*)
  (*let consume = Cconst_int (1, Debuginfo.none)*)
  (*let acquire = Cconst_int (2, Debuginfo.none)*)
  (*let release = Cconst_int (3, Debuginfo.none)*)
  (*let acq_rel = Cconst_int (4, Debuginfo.none)*)
  let seq_cst = Cconst_int (5, Debuginfo.none)
end

let machtype_of_memory_chunk = function
  | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
  | Thirtytwo_unsigned | Thirtytwo_signed | Word_int ->
    typ_int
  | Word_val -> typ_val
  | Single { reg = Float32; } -> assert false (* XXX *)
  | Single { reg = Float64; } -> assert false (* XXX *)
  | Double -> typ_float
  | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
    assert false (* XXX *)

let dbg_none = Debuginfo.none

let wrap_entry_exit expr =
  let call_entry =
    Cmm_helpers.return_unit dbg_none
      (Cop
         ( cextcall ("__tsan_func_entry", typ_void, [], false),
           [Creturn_addr],
           dbg_none ))
  in
  let call_exit =
    Cmm_helpers.return_unit dbg_none
      (Cop (cextcall ("__tsan_func_exit", typ_void, [], false), [], dbg_none))
  in
  (* [is_tail] is true when the expression is in tail position *)
  let[@ocaml.warning "-8"](*XXX*) rec insert_call_exit is_tail = function
    | Clet (v, e, body) -> Clet (v, e, insert_call_exit is_tail body)
    | Clet_mut (v, typ, e, body) ->
      Clet_mut (v, typ, e, insert_call_exit is_tail body)
    | Cphantom_let (v, e, body) ->
      Cphantom_let (v, e, insert_call_exit is_tail body)
    | Cassign (v, body) -> Cassign (v, insert_call_exit is_tail body)
    | Csequence (op1, op2) -> Csequence (op1, insert_call_exit is_tail op2)
    | Cifthenelse (cond, t_dbg, t, f_dbg, f, dbg_none, kind_for_unboxing) ->
      Cifthenelse
        ( cond,
          t_dbg,
          insert_call_exit is_tail t,
          f_dbg,
          insert_call_exit is_tail f,
          dbg_none,
          kind_for_unboxing )
    | Cswitch (e, cases, handlers, dbg_none, kind_for_unboxing) ->
      let handlers =
        Array.map
          (fun (handler, handler_dbg) ->
            insert_call_exit is_tail handler, handler_dbg)
          handlers
      in
      Cswitch (e, cases, handlers, dbg_none, kind_for_unboxing)
    | Ccatch (isrec, handlers, next, kind_for_unboxing) ->
      let handlers =
        List.map
          (fun (id, args, e, dbg_none, is_cold) ->
            id, args, insert_call_exit is_tail e, dbg_none, is_cold)
          handlers
      in
      Ccatch (isrec, handlers, insert_call_exit is_tail next, kind_for_unboxing)
    | Cexit (ex, args, trap_actions) ->
      (* A [Cexit] is like a goto to the beginning of a handler. Therefore, it
         is never the last thing evaluated in a function; there is no need to
         insert a call to [__tsan_func_exit] here. *)
      Cexit (ex, args, trap_actions)
    | Ctrywith (e, lbl, v, handler, dbg_none, kind_for_unboxing) ->
      (* We need to insert a call to [__tsan_func_exit] at the tail of both the
         body and the handler. If this is a [try ... with] in tail position,
         then the body expression is not in tail position (as code is inserted
         at the end of it to pop the exception handler), the handler expression
         is. *)
      Ctrywith
        ( insert_call_exit false e,
          lbl,
          v,
          insert_call_exit is_tail handler,
          dbg_none,
          kind_for_unboxing )
    | Cop (Capply (fn, region_close), args, dbg_none) when is_tail ->
      (* This is a tail call. We insert the call to [__tsan_func_exit] right
         before the call, but after evaluating the arguments (from right to
         left). *)
      let fun_ = List.hd args in
      let args =
        List.map (fun e -> VP.create (V.create_local "arg"), e) (List.tl args)
      in
      let tail =
        Csequence
          ( call_exit,
            Cop
              ( Capply (fn, region_close),
                fun_ :: List.map (fun (id, _) -> Cvar (VP.var id)) args,
                dbg_none ) )
      in
      List.fold_left (fun acc (id, arg) -> Clet (id, arg, acc)) tail args
    | ( Cop
          ( ( Calloc _ | Caddi | Csubi | Cmuli | Cdivi | Cmodi | Cand | Cmulhi _
            | Cor | Cxor | Clsl | Clsr | Casr | Caddv | Cadda | Cnegf _ | Cabsf _
            | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ (* XXX | Cfloatofint | Cintoffloat *)
            (* XXX | Ccheckbound *) | Copaque | Cdls_get (* XXX | Cpoll *) | Capply _ | Cextcall _
            | Cload _ | Cstore _ | Ccmpi _ | Ccmpa _ | Ccmpf _ | Craise _ ),
            _,
            _ )
      | Cconst_int (_, _)
      | Cconst_natint (_, _)
      | Cconst_float (_, _)
      | Cconst_symbol (_, _)
      | Cvar _ | Ctuple _ (* XXX | Creturn_addr *) ) as expr ->
      let id = VP.create (V.create_local "res") in
      Clet (id, expr, Csequence (call_exit, Cvar (VP.var id)))
  in
  Csequence (call_entry, insert_call_exit true expr)

let instrument body =
  let[@ocaml.warning "-8"](*XXX*) rec aux = function
    | Cop
        ( (Cload { memory_chunk; mutability = Mutable; is_atomic = false } as
          load_op),
          [loc],
          dbginfo ) ->
      (* Emit a call to [__tsan_readN] before the load *)
      let loc_id = VP.create (V.create_local "loc") in
      let loc_exp = Cvar (VP.var loc_id) in
      Clet
        ( loc_id,
          loc,
          Csequence
            ( Cmm_helpers.return_unit dbg_none
                (Cop
                   ( cextcall
                       (select_function Read memory_chunk, typ_void, [], false),
                     [loc_exp],
                     dbg_none )),
              Cop (load_op, [loc_exp], dbginfo) ) )
    | Cop
        ( Cload { memory_chunk; mutability = Mutable; is_atomic = true },
          [loc],
          dbginfo ) ->
      (* Replace the atomic load with a call to [__tsan_atomicN_load] *)
      let ret_typ = machtype_of_memory_chunk memory_chunk in
      Cop
        ( cextcall
            ( Printf.sprintf "__tsan_atomic%d_load" (bit_size memory_chunk),
              ret_typ,
              [],
              false ),
          [loc; TSan_memory_order.seq_cst],
          dbginfo )
    | Cop
        (Cload { memory_chunk = _; mutability = Mutable; is_atomic = _ }, _, _)
      ->
      invalid_arg "instrument: wrong number of arguments for operation Cload"
    | Cop (Cstore (memory_chunk, init_or_assn), [loc; v], dbginfo) as c -> (
      (* Emit a call to [__tsan_writeN] before the store *)
      match init_or_assn with
      | Assignment ->
        (* We make sure that 1. the location and value expressions are
           evaluated before the call to TSan, and 2. the location expression is
           evaluated right before that call, as it might not be a valid OCaml
           value (e.g. a pointer into an array), in which case it must not be
           live across a function call or allocation point. *)
        let loc_id = VP.create (V.create_local "loc") in
        let loc_exp = Cvar (VP.var loc_id) in
        let v_id = VP.create (V.create_local "newval") in
        let v_exp = Cvar (VP.var v_id) in
        let args = [loc_exp; v_exp] in
        Clet
          ( v_id,
            v,
            Clet
              ( loc_id,
                loc,
                Csequence
                  ( Cmm_helpers.return_unit dbg_none
                      (Cop
                         ( cextcall
                             ( select_function Write memory_chunk,
                               typ_void,
                               [],
                               false ),
                           [loc_exp],
                           dbg_none )),
                    Cop (Cstore (memory_chunk, init_or_assn), args, dbginfo) )
              ) )
      | Initialization ->
        (* Initializing writes need not be instrumented as they are always
           domain-safe *)
        c)
    | Cop (Cstore _, _, _) ->
      invalid_arg "instrument: wrong number of arguments for operation Cstore"
    | Cop ((Cload { mutability = Immutable; _ } as op), es, dbg_none) ->
      (* Loads of immutable location require no instrumentation *)
      Cop (op, List.map aux es, dbg_none)
    | Cop (Craise _, _, _) as raise ->
      (* Call a routine that will call [__tsan_func_exit] for every function
         about to be exited due to the exception *)
      Csequence
        (Cmm_helpers.return_unit dbg_none
           (Cop (Capply (typ_int, Obj.magic 0 (* XXX *)),
                 [Cconst_symbol (Obj.magic (* XXX*) "caml_tsan_exit_on_raise_asm", dbg_none);
                  Cconst_int (0, dbg_none)],
                dbg_none)),
         raise)
    | Cop
        ( (( Capply _ | Caddi | Calloc _ | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
           | Cand | Cor | Cxor | Clsl | Clsr | Casr | Caddv | Cadda | Cnegf _
           | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ (* XXX | Cfloatofint | Cintoffloat *)
           (* XXX | Ccheckbound *) | Copaque | Cdls_get (* XXX | Cpoll *) | Cextcall _ | Ccmpi _
           | Ccmpa _ | Ccmpf _ ) as op),
          es,
          dbg_none ) ->
      Cop (op, List.map aux es, dbg_none)
    | Clet (v, e, body) -> Clet (v, aux e, aux body)
    | Clet_mut (v, k, e, body) -> Clet_mut (v, k, aux e, aux body)
    | Cphantom_let (v, e, body) -> Cphantom_let (v, e, aux body)
    | Cassign (v, e) -> Cassign (v, aux e)
    | Ctuple es -> Ctuple (List.map aux es)
    | Csequence (c1, c2) -> Csequence (aux c1, aux c2)
    | Ccatch (isrec, cases, body, kind_for_unboxing) ->
      let cases =
        List.map
          (fun (nfail, ids, e, dbg_none, is_cold) -> nfail, ids, aux e, dbg_none, is_cold)
          cases
      in
      Ccatch (isrec, cases, aux body, kind_for_unboxing)
    | Cexit (ex, args, trap_actions) -> Cexit (ex, List.map aux args, trap_actions)
    | Cifthenelse (cond, t_dbg, t, f_dbg, f, dbg_none, kind_for_unboxing) ->
      Cifthenelse (aux cond, t_dbg, aux t, f_dbg, aux f, dbg_none, kind_for_unboxing)
    | Ctrywith (e, label, ex, handler, dbg_none, kind_for_unboxing) ->
      Ctrywith (aux e, label, ex, aux handler, dbg_none, kind_for_unboxing)
    | Cswitch (e, cases, handlers, dbg_none, kind_for_unboxing) ->
      let handlers =
        handlers
        |> Array.map (fun (handler, handler_dbg) -> aux handler, handler_dbg)
      in
      Cswitch (aux e, cases, handlers, dbg_none, kind_for_unboxing)
    (* no instrumentation *)
    | ( Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
      | Cvar _ (* XXX | Creturn_addr *) ) as c ->
      c
  in
  body |> aux |> wrap_entry_exit
