(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common subexpression elimination by value numbering over extended
   basic blocks. *)

open Mach
open CSE_utils

module CSE_utils_numbering = Make (struct type t = Mach.operation end)
open CSE_utils_numbering

(* Prepend a set of moves before [i] to assign [srcs] to [dsts].  *)

let insert_single_move i src dst =
  instr_cons_debug (Iop Imove) [|src|] [|dst|] i.dbg i

let insert_move srcs dsts i =
  match Array.length srcs with
  | 0 -> i
  | 1 -> instr_cons_debug (Iop Imove) srcs dsts i.dbg i
  | _ -> (* Parallel move: first copy srcs into tmps one by one,
            then copy tmps into dsts one by one *)
         let tmps = Reg.createv_like srcs in
         let i1 = Misc.Stdlib.Array.fold_left2 insert_single_move i tmps dsts in
         Misc.Stdlib.Array.fold_left2 insert_single_move i1 srcs tmps

let rec split4 = function
    [] -> ([], [], [], [])
  | (x,y,z,w)::l ->
    let (rx, ry, rz, rw) = split4 l in (x::rx, y::ry, z::rz, w::rw)

let rec combine4 l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
    ([], [], [], []) -> []
  | (a1::l1, a2::l2, a3::l3, a4::l4) -> (a1, a2, a3, a4) :: combine4 l1 l2 l3 l4
  | (_, _, _, _) -> invalid_arg "combine4"

class cse_generic = object (self)

(* Default classification of operations.  Can be overridden in
   processor-specific files to classify specific operations better. *)

method class_of_operation op =
  match op with
  | Imove | Ispill | Ireload -> assert false   (* treated specially *)
  | Iconst_int _ | Iconst_float32 _ | Iconst_float _
  | Iconst_symbol _ | Iconst_vec128 _ -> Op_pure
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Iextcall _ | Iprobe _ | Iopaque -> assert false  (* treated specially *)
  | Istackoffset _ -> Op_other
  | Iload { mutability; is_atomic } ->
    (* #12173: disable CSE for atomic loads. *)
    if is_atomic then Op_other
    else Op_load (match mutability with
      | Mutable -> Mutable
      | Immutable -> Immutable)
  | Istore(_,_,asg) -> Op_store asg
  | Ialloc _ | Ipoll _ -> assert false     (* treated specially *)
  | Iintop _ -> Op_pure
  | Iintop_imm(_, _) -> Op_pure
  | Iintop_atomic _ -> Op_store true
  | Ifloatop _
  | Icsel _
  | Istatic_cast _
  | Ireinterpret_cast _ -> Op_pure
  | Ispecific _ -> Op_other
  | Iname_for_debugger _ -> Op_other
  | Iprobe_is_enabled _ -> Op_other
  | Ibeginregion | Iendregion -> Op_other
  | Idls_get -> Op_load Mutable
  | Ireturn_addr -> Op_load Immutable

(* Operations that are so cheap that it isn't worth factoring them. *)

method is_cheap_operation op =
  match op with
  | Iconst_int _ -> true
  | _ -> false

(* Forget all equations involving mutable memory loads.
   Performed after a non-initializing store *)

method private kill_loads n =
  remove_mutable_load_numbering n

(* Perform CSE on the given instruction [i] and its successors.
   [n] is the value numbering current at the beginning of [i]. *)

method private cse n i k =
  match i.desc with
  | Iend | Ireturn _ | Iop(Itailcall_ind) | Iop(Itailcall_imm _)
  | Iexit _ | Iraise _ ->
      k i
  | Iop (Imove | Ispill | Ireload) ->
      (* For moves, we associate the same value number to the result reg
         as to the argument reg. *)
      let n1 = set_move n i.arg.(0) i.res.(0) in
      self#cse n1 i.next (fun next -> k { i with next; })
  | Iop (Icall_ind | Icall_imm _ | Iextcall _ | Iprobe _) ->
      (* For function calls and probes, we should at least forget:
         - equations involving memory loads, since the callee can
           perform arbitrary memory stores;
         - equations involving arithmetic operations that can
           produce [Addr]-typed derived pointers into the heap
           (see below for Ialloc);
         - mappings from hardware registers to value numbers,
           since the callee does not preserve these registers.
         That doesn't leave much usable information: checkbounds
         could be kept, but won't be usable for CSE as one of their
         arguments is always a memory load.  For simplicity, we
         just forget everything. *)
      self#cse empty_numbering i.next (fun next -> k { i with next; })
  | Iop Iopaque ->
      (* Assume arbitrary side effects from Iopaque *)
      self#cse empty_numbering i.next (fun next -> k { i with next; })
  | Iop (Ialloc _) | Iop (Ipoll _) ->
      (* For allocations, we must avoid extending the live range of a
         pseudoregister across the allocation if this pseudoreg
         is a derived heap pointer (a pointer into the heap that does
         not point to the beginning of a Caml block).  PR#6484 is an
         example of this situation.  Such pseudoregs have type [Addr].
         Pseudoregs with types other than [Addr] can be kept.
         Moreover, allocations and polls can trigger the asynchronous execution
         of arbitrary Caml code (finalizer, signal handler, context
         switch), which can contain non-initializing stores.
         Hence, all equations over mutable loads must be removed. *)
       let n1 = kill_addr_regs (self#kill_loads n) in
       let n2 = set_unknown_regs n1 i.res in
       self#cse n2 i.next (fun next -> k { i with next; })
  | Iop op ->
      begin match self#class_of_operation op with
      | (Op_pure | Op_load _) as op_class ->
          let (n1, varg) = valnum_regs n i.arg in
          let n2 = set_unknown_regs n1 (Proc.destroyed_at_oper i.desc) in
          begin match find_equation op_class n1 (op, varg) with
          | Some vres ->
              (* This operation was computed earlier. *)
              (* Are there registers that hold the results computed earlier? *)
              begin match find_regs_containing n1 vres with
              | Some res when (not (self#is_cheap_operation op)) ->
                  (* We can replace res <- op args with r <- move res,
                     provided res are stable (non-volatile) registers.
                     If the operation is very cheap to compute, e.g.
                     an integer constant, don't bother. *)
                  let n3 = set_known_regs n1 i.res vres in
                  (* This is n1 above and not n2 because the move
                     does not destroy any regs *)
                  self#cse n3 i.next (fun next ->
                    k (insert_move res i.res next))
              | _ ->
                  (* We already computed the operation but lost its
                     results.  Associate the result registers to
                     the result valnums of the previous operation. *)
                  let n3 = set_known_regs n2 i.res vres in
                  self#cse n3 i.next (fun next -> k { i with next; })
              end
          | None ->
              (* This operation produces a result we haven't seen earlier. *)
              let n3 = set_fresh_regs n2 i.res (op, varg) op_class in
              self#cse n3 i.next (fun next -> k { i with next; })
          end
      | Op_store false | Op_other ->
          (* An initializing store or an "other" operation do not invalidate
             any equations, but we do not know anything about the results. *)
         let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
         let n2 = set_unknown_regs n1 i.res in
         self#cse n2 i.next (fun next -> k { i with next; })
      | Op_store true ->
          (* A non-initializing store can invalidate
             anything we know about prior mutable loads. *)
         let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
         let n2 = set_unknown_regs n1 i.res in
         let n3 = self#kill_loads n2 in
         self#cse n3 i.next (fun next -> k { i with next; })
      end
  (* For control structures, we set the numbering to empty at every
     join point, but propagate the current numbering across fork points. *)
  | Iifthenelse(test, ifso, ifnot) ->
      let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
      self#cse n1 ifso (fun ifso ->
        self#cse n1 ifnot (fun ifnot ->
          self#cse empty_numbering i.next (fun next ->
            k { i with desc = Iifthenelse(test, ifso, ifnot); next; })))
  | Iswitch(index, cases) ->
      let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
      self#cse_array n1 cases (fun cases ->
        self#cse empty_numbering i.next (fun next ->
          k { i with desc = Iswitch(index, cases); next; }))
  | Icatch(rec_flag, ts, handlers, body) ->
      let nfail, t, handler_code, is_cold = split4 handlers in
      self#cse_list empty_numbering handler_code (fun handler_code ->
        let handlers = combine4 nfail t handler_code is_cold in
        self#cse n body (fun body ->
          self#cse empty_numbering i.next (fun next ->
            k { i with desc = Icatch(rec_flag, ts, handlers, body); next; })))
  | Itrywith(body, kind, (ts, handler)) ->
      self#cse n body (fun body ->
        self#cse empty_numbering handler (fun handler ->
          self#cse empty_numbering i.next (fun next ->
            k { i with desc = Itrywith(body, kind, (ts, handler)); next; })))

method private cse_array n is k =
  self#cse_list n (Array.to_list is) (fun is -> k (Array.of_list is))

method private cse_list0 n is acc k =
  match is with
  | [] -> k acc
  | i::is -> self#cse n i (fun i -> self#cse_list0 n is (i :: acc) k)

method private cse_list n is k =
  self#cse_list0 n is [] (fun is_rev -> k (List.rev is_rev))

method fundecl f =
  (* CSE can trigger bad register allocation behaviors, see MPR#7630 *)
  if List.mem Cmm.No_CSE f.fun_codegen_options then
    f
  else
    { f with fun_body = self#cse empty_numbering f.fun_body Fun.id }

end
