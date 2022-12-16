(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2022-2022 Jane Street Group LLC                                  *
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

module String = Misc.Stdlib.String

type error =
  | Annotation of
      { fun_name : string;
        check : string
      }

exception Error of Location.t * error

module Value = struct
  type t =
    | Pass
    | Fail
    | Unknown

  let to_string = function
    | Pass -> "pass"
    | Fail -> "fail"
    | Unknown -> "unknown"
end

module Func_info = struct
  type t =
    { name : string;  (** function name *)
      mutable loc : Location.t option;
          (** Source location for error messages. *)
      mutable value : Value.t;  (** the result of the check *)
      mutable callers : string list;
          (** unresolved dependencies. if not empty then value is Unknown. *)
      mutable in_current_unit : bool;
          (** is the function known to be defined in the current unit? *)
      mutable annotated : bool  (** is the function annotated *)
    }

  let create name value =
    { name;
      loc = None;
      value;
      callers = [];
      in_current_unit = false;
      annotated = false
    }
end

module type Spec = sig
  (** Which check is it? Used in error messages. *)
  val name : string

  (** Is the check enabled? *)
  val enabled : unit -> bool

  (** Record that the callee passed the check. *)
  val add_callee : string -> Cmx_format.checks -> unit

  (** Does the callee pass the check (i.e., satisfies the conditions such as no
      allocation)? Returns [true] if the callee passed the check or the callee
      is annotated to be skipped by this check. *)
  val check_callee : string -> Cmx_format.checks -> bool

  (** returns true when the check passes. *)
  val check_specific : Arch.specific_operation -> bool

  val annotation : Cmm.property
end
(* CR-someday gyorsh: We may also want annotations on call sites, not only on
   functions. *)

(** Check one function. *)
module Analysis (S : Spec) : sig
  val fundecl : Format.formatter -> Mach.fundecl -> unit

  val reset_unit_info : unit -> unit

  val record_unit_info : Format.formatter -> Cmx_format.checks -> unit
end = struct
  (** Information about functions that we have seen so far for the current
      compilation unit. *)
  module Unit_info : sig
    (** mutable state *)
    type t

    val create : unit -> t

    val reset : t -> unit

    (** Resolve all dependencies, record all names associated with Pass, and
        check all annotated names. *)
    val resolve_all : Format.formatter -> t -> Cmx_format.checks -> unit

    val is_fail : t -> string -> bool

    val get_value : t -> string -> Value.t option

    val add_value : Format.formatter -> t -> string -> Value.t -> unit

    val add_dep : t -> callee:string -> caller:string -> unit

    val in_current_unit : t -> string -> Debuginfo.t -> unit

    val annotated : t -> string -> unit
  end = struct
    (** map function name to the information about it *)
    type t = (string, Func_info.t) Hashtbl.t

    let create () : (string, Func_info.t) Hashtbl.t = Hashtbl.create 17

    let reset t = Hashtbl.reset t

    let is_fail t name =
      match Hashtbl.find_opt t name with
      | None -> false
      | Some (func_info : Func_info.t) -> (
        match func_info.value with Fail -> true | Pass | Unknown -> false)

    let in_current_unit t name dbg =
      let func_info : Func_info.t =
        match Hashtbl.find_opt t name with
        | None ->
          let func_info = Func_info.create name Unknown in
          Hashtbl.replace t name func_info;
          func_info
        | Some func_info -> func_info
      in
      func_info.in_current_unit <- true;
      match func_info.loc with
      | None -> func_info.loc <- Some (Debuginfo.to_location dbg)
      | Some _ -> Misc.fatal_errorf "Duplicate symbol name %s" name

    let record t name value =
      match Hashtbl.find_opt t name with
      | None ->
        let func_info = Func_info.create name value in
        Hashtbl.replace t name func_info
      | Some (old : Func_info.t) ->
        if old.value = value
        then ()
        else if old.value = Value.Unknown
        then old.value <- value
        else
          Misc.fatal_errorf
            "Checkmach %s record: can't set %s to %s, already set to %s" S.name
            name (Value.to_string value)
            (Value.to_string old.value)

    (* Resolve everything that depends on [callee]. *)
    let rec resolve ppf t callee (value : Value.t) =
      record t callee value;
      match Hashtbl.find_opt t callee with
      | None -> assert false
      | Some (func_info : Func_info.t) -> (
        match value with
        | Unknown ->
          Misc.fatal_errorf "Checkmach %s: can't resolve %s to Unknown" S.name
            callee
        | Pass ->
          (* If [callee] passes the check and has a caller that depends on it,
             we can resolve the [caller] to Pass only when it does not depend on
             any other callees, but we don't keep the information about reverse
             dependencies, so we just leave the dependency edge. *)
          func_info.callers <- []
        | Fail ->
          (* If [callee] fails the check, all its callers fail the check. *)
          let callers = func_info.callers in
          func_info.callers <- [];
          List.iter
            (fun caller ->
              if !Flambda_backend_flags.dump_checkmach && not (is_fail t caller)
              then
                Format.fprintf ppf
                  "*** check %s failed in %s: callee %s resolved to fail\n"
                  S.name caller callee;
              resolve ppf t caller Value.Fail)
            callers)

    (* [resolve_all] makes 3 passes *)
    let resolve_all ppf t checks =
      (* Resolve functions not defined in the current unit as [Fail]. If they
         were [Pass], they would not have been recorded. *)
      Hashtbl.iter
        (fun callee (func_info : Func_info.t) ->
          if not func_info.in_current_unit then resolve ppf t callee Fail)
        t;
      (* Record all remaining [Unknown] as [Pass].

         If there is a call from [f] to [g], and we have seen the definitions of
         [f] and [g], then if [g] failed the check, we would have already
         removed the dependency and recorded both [f] and [g] as [Fail].
         Therefore, either [g] passed the check or there is a cycle of
         dependencies involving [f]. For all functions in the dependency cycle,
         we have already determined that they don't have any forbidden
         instructions directly. Therefore the entire cycle passes the check.*)
      Hashtbl.iter
        (fun _callee (func_info : Func_info.t) ->
          match func_info.value with
          | Unknown ->
            func_info.value <- Pass;
            func_info.callers <- []
          | Pass | Fail -> assert (List.length func_info.callers = 0))
        t;
      (* Check annotations. Return all function names associated with Pass. *)
      if S.enabled ()
      then
        Hashtbl.iter
          (fun _callee (func_info : Func_info.t) ->
            if func_info.in_current_unit
            then
              match func_info.value with
              | Unknown -> assert false
              | Pass -> S.add_callee func_info.name checks
              | Fail ->
                if func_info.annotated
                then
                  raise
                    (Error
                       ( Option.get func_info.loc,
                         Annotation
                           { fun_name = func_info.name; check = S.name } )))
          t

    let add_dep t ~callee ~caller =
      record t callee Unknown;
      record t caller Unknown;
      let func_info : Func_info.t = Hashtbl.find t callee in
      if not (List.mem caller func_info.callers)
      then func_info.callers <- caller :: func_info.callers

    let add_value ppf t name value = resolve ppf t name value

    let get_value t name =
      match Hashtbl.find_opt t name with
      | None -> None
      | Some (func_info : Func_info.t) -> Some func_info.value

    let annotated t name =
      let func_info : Func_info.t = Hashtbl.find t name in
      func_info.annotated <- true
  end

  let unit_info = Unit_info.create ()

  let reset_unit_info () = Unit_info.reset unit_info

  let record_unit_info ppf_dump checks =
    Unit_info.resolve_all ppf_dump unit_info checks

  type t =
    { ppf : Format.formatter;
      fun_name : string;
      mutable unresolved_dependencies : bool
    }

  let report t ~msg ~desc dbg =
    if !Flambda_backend_flags.dump_checkmach
    then
      Format.fprintf t.ppf "*** check %s %s in %s: %s %a\n" S.name msg
        t.fun_name desc Debuginfo.print_compact dbg

  exception Bail

  let report_fail t desc dbg =
    report t ~msg:"failed" ~desc dbg;
    Unit_info.add_value t.ppf unit_info t.fun_name Fail;
    if not !Flambda_backend_flags.dump_checkmach then raise_notrace Bail

  let check_call t callee ~desc dbg =
    if String.begins_with ~prefix:"caml_apply" callee
       (* This check is only an optimization, to keep dependencies small. *)
       (* The prefix check is conservative (not everything that begins with
          caml_apply is a compiler-generated symbol). *)
       (* CR-someday gyorsh: propagate information about caml_apply from cmm to
          mach instead of reverse-engineering it from the symbol name. *)
    then report_fail t desc dbg
    else if not (S.check_callee callee Compilenv.cached_checks)
    then
      match Unit_info.get_value unit_info callee with
      | Some Pass -> ()
      | None | Some Unknown ->
        if Unit_info.is_fail unit_info t.fun_name
        then report t ~msg:"verbose" ~desc dbg
        else (
          Unit_info.add_dep unit_info ~callee ~caller:t.fun_name;
          t.unresolved_dependencies <- true;
          report t ~msg:"unknown" ~desc dbg)
      | Some Fail -> report_fail t desc dbg

  (** Returns [false] when the check fails (e.g., allocation or indirect call
      found). *)
  let check_operation t (op : Mach.operation) dbg =
    match op with
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _ | Iconst_symbol _
    | Iload _ | Icompf _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat | Ivalueofint | Iintofvalue
    | Iintop_imm
        ( ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ ),
          _ )
    | Iintop
        ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
        | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ )
    | Icsel _ | Iname_for_debugger _ ->
      assert (Mach.operation_is_pure op)
    | Istackoffset _ | Iprobe_is_enabled _ | Iopaque | Ibeginregion | Iendregion
    | Iintop_atomic _ ->
      ()
    | Istore _ -> ()
    | Iintop Icheckbound | Iintop_imm (Icheckbound, _) ->
      report_fail t "checkbound" dbg
    | Ialloc { mode = Alloc_local; _ } -> ()
    | Ialloc { mode = Alloc_heap; _ } -> report_fail t "allocation" dbg
    | Ipoll _ ->
      (* Polling points may trigger finalisers, signal handlers or memprof
         callbacks, but any allocations done therein don't count towards the
         calling function being "allocating". *)
      ()
    | Iprobe { name; handler_code_sym } ->
      let desc = Printf.sprintf "probe %s handler %s" name handler_code_sym in
      check_call t handler_code_sym ~desc dbg
    | Icall_ind -> report_fail t "indirect call" dbg
    | Itailcall_ind -> report_fail t "indirect tailcall" dbg
    | Icall_imm { func } ->
      check_call t func ~desc:("direct call to " ^ func) dbg
    | Itailcall_imm { func } ->
      check_call t func ~desc:("direct tailcall to " ^ func) dbg
    | Iextcall { alloc = false; _ } -> ()
    | Iextcall { func; alloc = true; _ } ->
      report_fail t ("external call to " ^ func) dbg
    | Ispecific s ->
      if not (S.check_specific s)
      then report_fail t "Arch.specific_operation" dbg

  (** Returns [true] when [i] is post-dominated by a raise. If [i] is not, then
      checks [i] for the property in [S]. *)
  let rec check_instr_exn t (i : Mach.instruction) raise_after =
    match (i.desc : Mach.instruction_desc) with
    | Iend -> raise_after
    | Iop op ->
      let raise_after = check_instr_exn t i.next raise_after in
      if not raise_after then check_operation t op i.dbg;
      raise_after
    | Iifthenelse (_c, ifso, ifnot) ->
      let raise_after = check_instr_exn t i.next raise_after in
      let raise_after_ifso = check_instr_exn t ifso raise_after in
      let raise_after_ifnot = check_instr_exn t ifnot raise_after in
      raise_after_ifso && raise_after_ifnot
    | Iswitch (_index, cases) ->
      let raise_after = check_instr_exn t i.next raise_after in
      Array.for_all (fun case -> check_instr_exn t case raise_after) cases
    | Icatch (_rec, _ts, handlers, body) ->
      (* conservative *)
      let raise_after = check_instr_exn t i.next raise_after in
      let _ = check_instr_exn t body raise_after in
      List.iter
        (fun (_n, _ts, handler) -> check_instr_exn t handler false |> ignore)
        handlers;
      false
    | Itrywith (body, _kind, (_ts, handler)) ->
      (* conservative *)
      let _ = check_instr_exn t i.next raise_after in
      let _ = check_instr_exn t body false in
      let _ = check_instr_exn t handler false in
      false
    | Iraise _ -> false
    | Ireturn _ -> false
    | Iexit _ -> false

  let debug t expected =
    match Unit_info.get_value unit_info t.fun_name with
    | None -> assert false
    | Some v ->
      (* just checking for consistency with the recorded value *)
      if not (v = expected)
      then
        Misc.fatal_errorf
          "Checkmach %s: Wrong result for %s, expected %s, found %s" S.name
          t.fun_name (Value.to_string expected) (Value.to_string v)

  let fundecl ppf (f : Mach.fundecl) =
    if S.enabled ()
    then
      Profile.record_call ~accumulate:true ("check " ^ S.name) (fun () ->
          let fun_name = f.fun_name in
          let t = { ppf; fun_name; unresolved_dependencies = false } in
          Unit_info.in_current_unit unit_info fun_name f.fun_dbg;
          if List.mem (Cmm.Assume S.annotation) f.fun_codegen_options
          then (
            report t ~msg:"assumed" ~desc:"fundecl" f.fun_dbg;
            Unit_info.add_value t.ppf unit_info fun_name Pass)
          else (
            (try
               let _ = check_instr_exn t f.fun_body false in
               if (not t.unresolved_dependencies)
                  && not (Unit_info.is_fail unit_info t.fun_name)
               then (
                 report t ~msg:"passed" ~desc:"fundecl" f.fun_dbg;
                 Unit_info.add_value t.ppf unit_info fun_name Pass)
             with Bail -> debug t Fail);
            if List.mem (Cmm.Assert S.annotation) f.fun_codegen_options
            then Unit_info.annotated unit_info fun_name))
end

module Spec_alloc : Spec = struct
  let name = "noalloc"

  let enabled () = !Flambda_backend_flags.alloc_check

  let check_callee s (checks : Cmx_format.checks) =
    String.Set.mem s checks.ui_noalloc_functions

  let add_callee s (checks : Cmx_format.checks) =
    if not (check_callee s checks)
    then
      checks.ui_noalloc_functions
        <- String.Set.add s checks.ui_noalloc_functions

  let check_specific s = not (Arch.operation_allocates s)

  let annotation = Cmm.Noalloc
end

(***************************************************************************
 *   Statically checks that the input function satisfies the following
 *   - no allocations on the heap (local allocations are ignored)
 *   - no indirect calls (incl. no indirect tailcalls)
 *   - all direct calls (incl. tailcalls and probes) are to functions the
 *     satisfy the same conditions.
 *****************************************************************************)
module Check_alloc = Analysis (Spec_alloc)

let fundecl ppf_dump fd =
  Check_alloc.fundecl ppf_dump fd;
  fd

let reset_unit_info () =
  Check_alloc.reset_unit_info ();
  ()

let record_unit_info ppf_dump =
  let checks = (Compilenv.current_unit_infos ()).ui_checks in
  Check_alloc.record_unit_info ppf_dump checks;
  Compilenv.cache_checks checks

(* Error report *)

let report_error ppf = function
  | Annotation { fun_name; check } ->
    Format.fprintf ppf "Annotation check for %s failed on function %s" check
      fun_name

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
