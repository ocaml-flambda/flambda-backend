[@@@ocaml.warning "+a-30-40-41-42"]

module CL = Cfg_with_layout

let add cl =
  (* Fabricate debug info when missing, because it is required to emit discriminators.
     Following the semantics of debug_line, remember the last-seen dbg and attach it to
     instructions that do not have any.

     The following Linear instructions do not need Fdo info,
     because they are emitted as assembly directives and
     not as addressible assembly instructions:
           Lend
           Lreloadretaddr
           Llabel
           Lentertrap
           Ladjust_trap_depth
     There is nothing wrong with emitting .loc directives for them,
     but its redundant.
     Only Lreloadretaddr has a corresponding Cfg instruction.
     Others are created during cfg_to_linear with [fdo] set to [none].
  *)
  let update_instr prev (i : _ Cfg.instruction) =
    let dbg =
      if not (Debuginfo.is_none prev) && (Debuginfo.is_none i.dbg) then
        prev
      else
        i.dbg
    in
    let fdo = Fdo_info.create ~discriminator:i.id ~dbg in
    dbg, { i with fdo }
  in
  let cfg = CL.cfg cl in
  let layout = CL.layout cl in
  let update_block prev label =
    let block = Cfg.get_block_exn cfg label in
    let prev, new_body = List.fold_left_map update_instr prev block.body in
    block.body <- new_body;
    let prev, new_terminator = update_instr prev block.terminator in
    block.terminator <- new_terminator;
    prev
  in
  ignore (List.fold_left update_block cfg.fun_dbg layout: Debuginfo.t)
