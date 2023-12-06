(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2016 Stephen Dolan.                                        *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Insert instrumentation for afl-fuzz *)

open Cmm

module V = Backend_var
module VP = Backend_var.With_provenance

let sym s = { sym_name = s; sym_global = Global }
let afl_area_ptr dbg = Cconst_symbol (sym "caml_afl_area_ptr", dbg)
let afl_prev_loc dbg = Cconst_symbol (sym "caml_afl_prev_loc", dbg)
let afl_map_size = 1 lsl 16

let rec with_afl_logging b dbg =
  if !Clflags.afl_inst_ratio < 100 &&
    Random.int 100 >= !Clflags.afl_inst_ratio then instrument b else
  let instrumentation =
    (* The instrumentation that afl-fuzz requires is:

         cur_location = <COMPILE_TIME_RANDOM>;
         shared_mem[cur_location ^ prev_location]++;
         prev_location = cur_location >> 1;

       See http://lcamtuf.coredump.cx/afl/technical_details.txt or
       docs/technical_details.txt in afl-fuzz source for for a full
       description of what's going on. *)
    let cur_location = Random.int afl_map_size in
    let cur_pos = V.create_local "pos" in
    let afl_area = V.create_local "shared_mem" in
    let op oper args = Cop (oper, args, dbg) in
    Clet(VP.create afl_area,
         op (Cload ({memory_chunk=Word_int;
                     mutability=Asttypes.Mutable;
                     is_atomic=false})) [afl_area_ptr dbg],
         Clet(VP.create cur_pos, op Cxor [op (Cload {memory_chunk=Word_int;
                                                     mutability=Asttypes.Mutable;
                                                     is_atomic=false})
        [afl_prev_loc dbg]; Cconst_int (cur_location, dbg)],
      Csequence(
        op (Cstore(Byte_unsigned, Assignment))
          [op Cadda [Cvar afl_area; Cvar cur_pos];
           op Cadda [op (Cload {memory_chunk=Byte_unsigned;
                                mutability=Asttypes.Mutable;
                                is_atomic=false})
                        [op Cadda [Cvar afl_area; Cvar cur_pos]];
                      Cconst_int (1, dbg)]],
        op (Cstore(Word_int, Assignment))
          [afl_prev_loc dbg; Cconst_int (cur_location lsr 1, dbg)]))) in
  Csequence(instrumentation, instrument b)

and instrument = function
  (* these cases add logging, as they may be targets of conditional branches *)
  | Cifthenelse (cond, t_dbg, t, f_dbg, f, dbg, kind) ->
     Cifthenelse (instrument cond, t_dbg, with_afl_logging t t_dbg,
       f_dbg, with_afl_logging f f_dbg, dbg, kind)
  | Ctrywith (e, kind, ex, handler, dbg, value_kind) ->
     Ctrywith (instrument e, kind, ex, with_afl_logging handler dbg, dbg, value_kind)
  | Cswitch (e, cases, handlers, dbg, value_kind) ->
     let handlers =
       Array.map (fun (handler, handler_dbg) ->
           let handler = with_afl_logging handler handler_dbg in
           handler, handler_dbg)
         handlers
     in
     Cswitch (instrument e, cases, handlers, dbg, value_kind)

  (* these cases add no logging, but instrument subexpressions *)
  | Clet (v, e, body) -> Clet (v, instrument e, instrument body)
  | Clet_mut (v, k, e, body) ->
    Clet_mut (v, k, instrument e, instrument body)
  | Cphantom_let (v, defining_expr, body) ->
    Cphantom_let (v, defining_expr, instrument body)
  | Cassign (v, e) -> Cassign (v, instrument e)
  | Ctuple es -> Ctuple (List.map instrument es)
  | Cop (op, es, dbg) -> Cop (op, List.map instrument es, dbg)
  | Csequence (e1, e2) -> Csequence (instrument e1, instrument e2)
  | Ccatch (isrec, cases, body, kind) ->
     let cases =
       List.map (fun (nfail, ids, e, dbg, is_cold) ->
           nfail, ids, instrument e, dbg, is_cold)
         cases
     in
     Ccatch (isrec, cases, instrument body, kind)
  | Cexit (ex, args, traps) -> Cexit (ex, List.map instrument args, traps)
  | Cregion e -> Cregion (instrument e)
  | Ctail e -> Ctail (instrument e)

  (* these are base cases and have no logging *)
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_vec128 _
  | Cconst_symbol _
  | Cvar _ as c -> c

let instrument_function c dbg =
  with_afl_logging c dbg

let instrument_initialiser c dbg =
  (* Each instrumented module calls caml_setup_afl at
     initialisation, which is a no-op on the second and subsequent
     calls *)
  Csequence
    (Cop (Cextcall { func = "caml_setup_afl";
                     builtin = false;
                     returns = true;
                     effects = Arbitrary_effects;
                     coeffects = Has_coeffects;
                     ty = typ_int; alloc = false; ty_args = []; },
          [Cconst_int (0, dbg ())],
          dbg ()),
     with_afl_logging c (dbg ()))
