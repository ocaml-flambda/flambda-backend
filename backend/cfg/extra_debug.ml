(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

module CL = Cfg_with_layout
module DLL = Flambda_backend_utils.Doubly_linked_list

let add cl =
  (* Fabricate debug info when missing, because it is required to emit discriminators.
   *    Following the semantics of debug_line, remember the last-seen dbg and attach it
   *    to instructions that do not have any.
   *
   *    The following Linear instructions do not need Fdo info,
   *    because they are emitted as assembly directives and
   *    not as addressible assembly instructions:
   *          Lend
   *          Lreloadretaddr
   *          Llabel
   *          Lentertrap
   *          Ladjust_stack_offset
   *    There is nothing wrong with emitting .loc directives for them,
   *    but its redundant.
   *    Only Lreloadretaddr has a corresponding Cfg instruction.
   *    Others are created during cfg_to_linear with [fdo] set to [none]. *)
  let update_instr prev (i : _ Cfg.instruction) =
    let dbg =
      if (not (Debuginfo.is_none prev)) && Debuginfo.is_none i.dbg
      then prev
      else i.dbg
    in
    i.fdo <- Fdo_info.create ~discriminator:i.id ~dbg;
    dbg
  in
  let cfg = CL.cfg cl in
  let layout = CL.layout cl in
  let update_block prev label =
    let block = Cfg.get_block_exn cfg label in
    let prev = DLL.fold_left ~f:update_instr ~init:prev block.body in
    let prev = update_instr prev block.terminator in
    prev
  in
  ignore (DLL.fold_left ~f:update_block ~init:cfg.fun_dbg layout : Debuginfo.t)
