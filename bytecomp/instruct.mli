(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The type of the instructions of the abstract machine *)

type structured_constant = Lambda.structured_constant

type raise_kind = Lambda.raise_kind

type comparison =
  | Eq
  | Neq
  | Ltint
  | Gtint
  | Leint
  | Geint
  | Ultint
  | Ugeint

(* Structure of compilation environments *)

type closure_entry = Debug_event.closure_entry =
  | Free_variable of int
  | Function of int

type closure_env = Debug_event.closure_env =
  | Not_in_closure
  | In_closure of {
      entries: closure_entry Ident.tbl; (* Offsets of the free variables and
                                           recursive functions from the start of
                                           the block *)
      env_pos: int;                     (* Offset of the current function from
                                           the start of the block *)
    }

type compilation_env = Debug_event.compilation_env =
  { ce_stack: int Ident.tbl;  (* Positions of variables in the stack *)
    ce_closure: closure_env } (* Structure of the heap-allocated env *)

(* The ce_stack component gives locations of variables residing
   in the stack. The locations are offsets w.r.t. the origin of the
   stack frame.
   The ce_closure component gives the positions of variables residing in the
   heap-allocated environment. The env_pos component gives the position of
   the current function from the start of the closure block, and the entries
   component gives the positions of free variables and functions bound by the
   same let rec as the current function, from the start of the closure block.
   These are used by the ENVACC and OFFSETCLOSURE instructions to recover the
   relevant value from the env register (which points to the current function).
*)

(* Debugging events *)

type debug_event = Debug_event.debug_event =
  { mutable ev_pos: int;
    ev_module: string;
    ev_loc: Location.t;
    ev_kind: debug_event_kind;
    ev_defname: string;
    ev_info: debug_event_info;
    ev_typenv: Env.summary;
    ev_typsubst: Subst.t;
    ev_compenv: compilation_env;
    ev_stacksize: int;
    ev_repr: debug_event_repr }

and debug_event_kind = Debug_event.debug_event_kind =
    Event_before
  | Event_after of Types.type_expr
  | Event_pseudo

and debug_event_info = Debug_event.debug_event_info =
    Event_function
  | Event_return of int
  | Event_other

and debug_event_repr = Debug_event.debug_event_repr =
    Event_none
  | Event_parent of int ref
  | Event_child of int ref

(* Abstract machine instructions *)

type label = int                        (* Symbolic code labels *)

type instruction =
    Klabel of label
  | Kacc of int
  | Kenvacc of int
  | Kpush
  | Kpop of int
  | Kassign of int
  | Kpush_retaddr of label
  | Kapply of int                       (* number of arguments *)
  | Kappterm of int * int               (* number of arguments, slot size *)
  | Kreturn of int                      (* slot size *)
  | Krestart
  | Kgrab of int                        (* number of arguments *)
  | Kclosure of label * int
  | Kclosurerec of label list * int
  | Koffsetclosure of int
  | Kgetglobal of Compilation_unit.t
  | Ksetglobal of Compilation_unit.t
  | Kgetpredef of Ident.t
  | Kconst of structured_constant
  | Kmakeblock of int * int             (* size, tag *)
  | Kmake_faux_mixedblock of int * int  (* size, tag *)
  (* A "faux" mixed block is not actually represented as a mixed block at
     runtime. It just has the top header byte sent to a sentinel value so
     bytecode knows that the block can't be marshaled to native code, where
     mixed records are represented as true mixed blocks.
  *)
  | Kmakefloatblock of int
  | Kgetfield of int
  | Ksetfield of int
  | Kgetfloatfield of int
  | Ksetfloatfield of int
  | Kvectlength
  | Kgetvectitem
  | Ksetvectitem
  | Kgetstringchar
  | Kgetbyteschar
  | Ksetbyteschar
  | Kbranch of label
  | Kbranchif of label
  | Kbranchifnot of label
  | Kstrictbranchif of label
  | Kstrictbranchifnot of label
  | Kswitch of label array * label array
  | Kboolnot
  | Kpushtrap of label
  | Kpoptrap
  | Kraise of raise_kind
  | Kcheck_signals
  | Kccall of string * int
  | Knegint | Kaddint | Ksubint | Kmulint | Kdivint | Kmodint
  | Kandint | Korint | Kxorint | Klslint | Klsrint | Kasrint
  | Kintcomp of comparison
  | Koffsetint of int
  | Koffsetref of int
  | Kisint
  | Kgetmethod
  | Kgetpubmet of int
  | Kgetdynmet
  | Kevent of debug_event
  | Kperform
  | Kresume
  | Kresumeterm of int
  | Kreperformterm of int
  | Kstop

val immed_min: int
val immed_max: int
