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

(** Abstract value for each component of the domain. *)
module V : sig
  type t =
    | Top  (** Property may not hold on some paths. *)
    | Safe  (** Property holds on all paths.  *)
    | Bot  (** Not reachable. *)

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val transform : t -> t

  val is_not_safe : t -> bool

  val print : Format.formatter -> t -> unit
end = struct
  type t =
    | Top
    | Safe
    | Bot

  let join c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top, Top -> Top
    | Safe, Bot | Bot, Safe -> Safe
    | Top, Bot | Top, Safe | Bot, Top | Safe, Top -> Top

  let lessequal v1 v2 =
    match v1, v2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top, Top -> true
    | Bot, Safe -> true
    | Bot, Top -> true
    | Safe, Top -> true
    | Top, (Bot | Safe) -> false
    | Safe, Bot -> false

  (** abstract transformer (backward analysis) for a statement that violates the property
      but doesn't alter control flow. *)
  let transform = function
    | Bot ->
      (* if a return is unreachable from the program location immediately after
         the statement, then return is unreachable from the program location
         immediately before the statement. *)
      Bot
    | Safe | Top -> Top

  let is_not_safe = function Top -> true | Safe | Bot -> false

  let print ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Top -> Format.fprintf ppf "top"
    | Safe -> Format.fprintf ppf "safe"
end

(** Abstract value associated with each program location in a function. *)
module Value : sig
  type t =
    { nor : V.t;
          (** Property about
              all paths from this program location that may reach a Normal Return  *)
      exn : V.t;
          (** Property about all paths from this program point that may reach a Return with
          Exception *)
      div : V.t
          (** Property about all paths from this program point that may diverge.  *)
    }

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val is_top : t -> bool

  val top : t

  val bot : t

  val normal_return : t

  val exn_escape : t

  val diverges : t

  val safe : t

  val relaxed : t

  val print : Format.formatter -> t -> unit

  val transform : t -> t
end = struct
  (** Lifts V to triples  *)
  type t =
    { nor : V.t;
      exn : V.t;
      div : V.t
    }

  let bot = { nor = V.Bot; exn = V.Bot; div = V.Bot }

  let lessequal v1 v2 =
    V.lessequal v1.nor v2.nor && V.lessequal v1.exn v2.exn
    && V.lessequal v1.div v2.div

  let join v1 v2 =
    { nor = V.join v1.nor v2.nor;
      exn = V.join v1.exn v2.exn;
      div = V.join v1.div v2.div
    }

  let transform v =
    { nor = V.transform v.nor;
      exn = V.transform v.exn;
      div = V.transform v.div
    }

  let normal_return = { bot with nor = V.Safe }

  let exn_escape = { bot with exn = V.Safe }

  let diverges = { bot with div = V.Safe }

  let safe = { nor = V.Safe; exn = V.Safe; div = V.Safe }

  let top = { nor = V.Top; exn = V.Top; div = V.Top }

  let relaxed = { nor = V.Safe; exn = V.Top; div = V.Top }

  let is_top v = v = top

  let print ppf { nor; exn; div } =
    Format.fprintf ppf "{ nor=%a; exn=%a; div=%a }" V.print nor V.print exn
      V.print div
end

(**  Representation of user-provided annotations as abstract values *)
module Annotation : sig
  type t

  val find : Cmm.codegen_option list -> Cmm.property -> Debuginfo.t -> t option

  val expected_value : t -> Value.t

  val is_assume : t -> bool

  val report_error : exn -> Location.error option

  exception
    Invalid of
      { a : t;
        fun_name : string;
        property : Cmm.property
      }
end = struct
  (**
   ***************************************************************************
   *  [Strict] statically guarantees that all paths through the function satisfy
   *  all of the following conditions:
   *   - property holds on all primitive operations (e.g., no heap allocation)
   *   - no indirect calls (incl. no indirect tailcalls)
   *   - all direct calls (incl. tailcalls and probes) are to functions that
   *     satisfy the same conditions, i.e., they are [Strict].
   *
   *  [Relaxed] is the same as [Strict] on all paths that end in a normal return
   *  from a function, but no restrictions on diverging executions or
   *  on when a function returns with a [raise] with backtrace, which is treated
   *  as an error return (whereas [raise_no_trace] is treated as normal control flow
   *  and is subject to [Strict] requirements).
   *
   *****************************************************************************)

  type t =
    { strict : bool;  (** strict or relaxed? *)
      assume : bool;
      loc : Location.t
          (** Source location of the annotation, used for error messages. *)
    }

  let expected_value t = if t.strict then Value.safe else Value.relaxed

  let is_assume t = t.assume

  let find codegen_options spec dbg =
    let a =
      List.filter_map
        (fun (c : Cmm.codegen_option) ->
          match c with
          | Check { property; strict; assume; loc } when property = spec ->
            Some { strict; assume; loc }
          | Check _ | Reduce_code_size | No_CSE | Use_linscan_regalloc -> None)
        codegen_options
    in
    match a with
    | [] -> None
    | [p] -> Some p
    | _ :: _ ->
      Misc.fatal_errorf "Unexpected duplicate annotation %a"
        Debuginfo.print_compact dbg ()

  exception
    Invalid of
      { a : t;
        fun_name : string;
        property : Cmm.property
      }

  let print_error ppf t ~fun_name ~property =
    Format.fprintf ppf "Annotation check for %s%s failed on function %s"
      (Printcmm.property_to_string property)
      (if t.strict then " strict" else "")
      fun_name

  let report_error = function
    | Invalid { a; fun_name; property } ->
      Some
        (Location.error_of_printer ~loc:a.loc
           (print_error ~fun_name ~property)
           a)
    | _ -> None
end

module Func_info = struct
  (* Fields are mutable: [annotation] changes when a call appears before the
     declaration. [value] and [unresolved_callers] change to track and update
     dependencies as the compilation unit is scanned. *)
  type t =
    { name : string;  (** function name *)
      mutable value : Value.t;  (** the result of the check *)
      mutable annotation : Annotation.t option;
          (** [value] must be lessequal than the expected value
          if there is user-defined annotation on this function. *)
      mutable unresolved_callers : String.Set.t;  (** direct callers  *)
      mutable unresolved_callees : String.Set.t  (** direct callees  *)
    }

  let create name =
    { name;
      value = Value.bot;
      annotation = None;
      unresolved_callers = String.Set.empty;
      unresolved_callees = String.Set.empty
    }

  let is_resolved t = String.Set.is_empty t.unresolved_callees

  let print ~msg ppf t =
    let open Format in
    let print_names ppf set =
      set |> String.Set.to_seq
      |> pp_print_seq
           ~pp_sep:(fun ppf () -> pp_print_char ppf ' ')
           pp_print_string ppf
    in
    fprintf ppf "%s %s %a@,(unresolved callees: %a)@,(unresolved callers: %a)@."
      msg t.name Value.print t.value print_names t.unresolved_callees
      print_names t.unresolved_callers
end

module type Spec = sig
  (** Is the check enabled? *)
  val enabled : unit -> bool

  (** [get_value_opt f] returns the value recorded for function [f] in [Compilenv],
      either because the check passed or because of user-defined "assume" annotation.
      If [f] was compiled with checks disabled, returns None.
  *)
  val get_value_opt : string -> Value.t option

  (** [set_value f v] record the value of the function named [f] in [Compilenv]. *)
  val set_value : string -> Value.t -> unit

  (** Summary of target specific operations. *)
  val transform_specific : Arch.specific_operation -> Value.t

  val property : Cmm.property
end
(* CR-someday gyorsh: We may also want annotations on call sites, not only on
   functions. *)

(** Information about functions that we have seen so far in the current compilation
      unit. *)
module Unit_info : sig
  (** mutable state *)
  type t

  val create : unit -> t

  val reset : t -> unit

  val find_opt : t -> string -> Func_info.t option

  (** [join_value t name v] name must be in the current compilation unit,
      and previously recorded.  *)
  val join_value : t -> string -> Value.t -> unit

  (** [add_value t name v] name must be in the current compilation unit
      and have not been recorded yet.  *)
  val add_value : t -> string -> Value.t -> unit

  (** [record_deps t ~caller ~callees] caller and callees must be in the current
      compilation unit.  *)
  val record_deps : t -> caller:string -> callees:String.Set.t -> unit

  (** [cleanup_deps] remove resolved dependencies starting from [name]. *)
  val cleanup_deps : t -> string -> unit

  val record_annotation : t -> string -> Annotation.t option -> unit

  val resolve_all : t -> unit

  val iter : t -> f:(Func_info.t -> unit) -> unit
end = struct
  (** map function name to the information about it *)
  type t = Func_info.t String.Tbl.t

  let create () = String.Tbl.create 17

  let reset t = String.Tbl.reset t

  let get_exn t name : Func_info.t = String.Tbl.find t name

  let find_opt t name = String.Tbl.find_opt t name

  let get_or_create t name : Func_info.t =
    match String.Tbl.find_opt t name with
    | None ->
      let func_info = Func_info.create name in
      String.Tbl.replace t name func_info;
      func_info
    | Some func_info -> func_info

  (* fixpoint backward propogation of function summaries along the recorded
     dependency edges. *)
  let rec propagate t (func_info : Func_info.t) =
    let unresolved_callers = func_info.unresolved_callers in
    let unresolved_callees = func_info.unresolved_callees in
    let value = func_info.value in
    if Value.is_top value
    then (
      (* optimization: remove incoming and outgoing dependency edges *)
      func_info.unresolved_callers <- String.Set.empty;
      func_info.unresolved_callees <- String.Set.empty;
      String.Set.iter
        (fun callee ->
          let callee_info = get_exn t callee in
          callee_info.unresolved_callers
            <- String.Set.remove func_info.name callee_info.unresolved_callers)
        unresolved_callees);
    let value =
      (* conservative use of summaries for unresolved dependencies *)
      let v = V.join value.nor value.exn in
      { Value.nor = v; exn = v; div = value.div }
    in
    String.Set.iter (join_and_propagate t ~value) unresolved_callers

  and join_and_propagate t ~value name =
    let func_info = get_exn t name in
    let new_value = Value.join func_info.value value in
    if not (Value.lessequal new_value func_info.value)
    then (
      func_info.value <- new_value;
      propagate t func_info)

  let iter t ~f = String.Tbl.iter (fun _ func_info -> f func_info) t

  let resolve_all t = iter t ~f:(propagate t)

  let add_value t name value =
    let (_ : Func_info.t) = get_or_create t name in
    join_and_propagate t name ~value

  let join_value t name value = join_and_propagate t name ~value

  let record_annotation t name annotation =
    let func_info = get_or_create t name in
    if Option.is_some func_info.annotation
    then Misc.fatal_errorf "Duplicate symbol %s" name;
    func_info.annotation <- annotation

  let record_deps t ~caller ~callees =
    let func_info = get_exn t caller in
    if not (String.Set.is_empty func_info.unresolved_callees)
    then Misc.fatal_errorf "Unexpected unresolved callees for %s" caller;
    func_info.unresolved_callees <- callees;
    String.Set.iter
      (fun callee ->
        let func_info = get_or_create t callee in
        func_info.unresolved_callers
          <- String.Set.add caller func_info.unresolved_callers)
      callees

  let rec cleanup_deps t name =
    (* optimization: clean up unresolved *)
    let func_info = get_exn t name in
    let unresolved_callees_except_self =
      String.Set.remove name func_info.unresolved_callees
    in
    if String.Set.is_empty unresolved_callees_except_self
    then (
      (* remove all resolved deps *)
      let unresolved_callers = func_info.unresolved_callers in
      func_info.unresolved_callers <- String.Set.empty;
      let changed_callers =
        String.Set.filter
          (fun caller ->
            let caller_info : Func_info.t = get_exn t caller in
            if String.Set.mem name caller_info.unresolved_callees
            then (
              caller_info.unresolved_callees
                <- String.Set.remove name caller_info.unresolved_callees;
              true)
            else false)
          unresolved_callers
      in
      (* propagate *)
      String.Set.iter (cleanup_deps t) changed_callers)
end

(** Check one function. *)
module Analysis (S : Spec) : sig
  val fundecl :
    Mach.fundecl ->
    future_funcnames:String.Set.t ->
    Unit_info.t ->
    Format.formatter ->
    unit

  val record_unit : Unit_info.t -> Format.formatter -> unit
end = struct
  (** Information about the current function under analysis. *)
  type t =
    { ppf : Format.formatter;
      current_fun_name : string;
      future_funcnames : String.Set.t;
          (** functions defined later in the same compilation unit  *)
      mutable unresolved_deps : String.Set.t;
          (** must be the current compilation unit.  *)
      unit_info : Unit_info.t
    }

  let create ppf current_fun_name future_funcnames unit_info =
    { ppf;
      current_fun_name;
      future_funcnames;
      unresolved_deps = String.Set.empty;
      unit_info
    }

  let analysis_name = Printcmm.property_to_string S.property

  let report' ppf v ~current_fun_name ~msg ~desc dbg =
    if !Flambda_backend_flags.dump_checkmach
    then
      Format.fprintf ppf "*** check %s %s in %s: %s with %a (%a)\n"
        analysis_name msg current_fun_name desc Value.print v
        Debuginfo.print_compact dbg

  let report t v ~msg ~desc dbg =
    report' t.ppf v ~msg ~desc ~current_fun_name:t.current_fun_name dbg

  let report_fail t d desc dbg = report t d ~msg:"failed" ~desc dbg

  let is_future_funcname t callee = String.Set.mem callee t.future_funcnames

  let check t (r : Value.t) msg dbg =
    if V.is_not_safe r.nor then report_fail t r (msg ^ " nor") dbg;
    if V.is_not_safe r.exn then report_fail t r (msg ^ " exn") dbg;
    if V.is_not_safe r.div then report_fail t r (msg ^ " div") dbg;
    r

  let report_unit_info ~msg ppf unit_info =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Unit_info.iter unit_info ~f:(Func_info.print ppf ~msg)

  let report_func_info ~msg ppf func_info =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Func_info.print ppf ~msg func_info

  let record_unit unit_info ppf =
    report_unit_info ppf unit_info ~msg:"before resolve_all";
    Unit_info.resolve_all unit_info;
    let record (func_info : Func_info.t) =
      (match func_info.annotation with
      | None -> ()
      | Some a ->
        if (not (Annotation.is_assume a))
           && not
                (Value.lessequal func_info.value (Annotation.expected_value a))
        then
          (* CR gyorsh: we can add error recovering mode where we sets the
             expected value as the actual value and continue analysis of other
             functions. *)
          raise
            (Annotation.Invalid
               { a; fun_name = func_info.name; property = S.property }));
      report_func_info ~msg:"record" ppf func_info;
      S.set_value func_info.name func_info.value
    in
    Unit_info.iter unit_info ~f:record

  let record_unit unit_info ppf =
    if S.enabled ()
    then
      Profile.record_call ~accumulate:true ("record_unit " ^ analysis_name)
        (fun () -> record_unit unit_info ppf)

  let update_deps t v dep desc dbg =
    match dep with
    | Some callee ->
      t.unresolved_deps <- String.Set.add callee t.unresolved_deps;
      report t v ~msg:"unresolved" ~desc dbg
    | None -> report t v ~msg:"resolved" ~desc dbg

  (* [find_callee] returns the value associated with the callee and whether
     there is a new dependency to record. *)
  let find_callee t callee =
    match Unit_info.find_opt t.unit_info callee with
    | None ->
      if is_future_funcname t callee
      then (
        (* The callee is defined later in the file. We have not seen any calls
           to it yet. *)
        (* CR-soon gyorsh: Returning Safe is sound because the value of the
           callee, when it becomes available, will be joined to the final value
           of the caller (the current function). Analysis result depends on the
           order of functions in the file. To address it, use more precise
           function summaries. *)
        let v = Value.safe in
        Unit_info.add_value t.unit_info callee v;
        v, Some callee)
      else
        (* Callee is not defined in the current compilation unit. *)
        let v =
          match S.get_value_opt callee with
          | None ->
            report t Value.top ~msg:"callee compiled without checks"
              ~desc:callee Debuginfo.none;
            Value.top
          | Some v -> v
        in
        v, None
    | Some callee_info ->
      (* Callee defined earlier in the same compilation unit, or we have already
         seen a call to this callee earlier in the same compilation unit
         (possibly in the same function), but haven't finished analysis of the
         callee's definition yet. *)
      (* If callee is not fully resolved, add it to dependencies. *)
      let dep =
        if is_future_funcname t callee
           || not (Func_info.is_resolved callee_info)
        then Some callee
        else None
      in
      let v =
        if String.equal t.current_fun_name callee
        then (
          (* self-call, conservative *)
          let v = Value.join callee_info.value Value.safe in
          Unit_info.add_value t.unit_info callee v;
          v)
        else callee_info.value
      in
      v, dep

  let transform_return ~(effect : V.t) dst =
    match effect with
    | V.Bot -> Value.bot
    | V.Safe -> dst
    | V.Top -> Value.transform dst

  let transform_diverge ~(effect : V.t) (dst : Value.t) =
    let div = V.join effect dst.div in
    { dst with div }

  let transform t ~next ~exn ~(effect : Value.t) desc dbg =
    let next = transform_return ~effect:effect.nor next in
    let exn = transform_return ~effect:effect.exn exn in
    report t next ~msg:"transform new next" ~desc dbg;
    report t exn ~msg:"transform new exn" ~desc dbg;
    let r = Value.join next exn in
    report t r ~msg:"transform join" ~desc dbg;
    let r = transform_diverge ~effect:effect.div r in
    report t r ~msg:"transform_call result" ~desc dbg;
    check t r desc dbg

  let transform_call t ~next ~exn callee ~desc dbg =
    report t next ~msg:"transform_call next" ~desc dbg;
    report t exn ~msg:"transform_call exn" ~desc dbg;
    let callee_value, new_dep = find_callee t callee in
    update_deps t callee_value new_dep desc dbg;
    transform t ~next ~exn ~effect:callee_value desc dbg

  let transform_operation t (op : Mach.operation) ~next ~exn dbg =
    match op with
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _ | Iconst_symbol _
    | Iload _ | Icompf _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat
    | Iintop_imm
        ( ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ ),
          _ )
    | Iintop
        ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
        | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ )
    | Ivalueofint | Iintofvalue | Icsel _ | Iname_for_debugger _ ->
      assert (Mach.operation_is_pure op);
      assert (not (Mach.operation_can_raise op));
      next
    | Istackoffset _ | Iprobe_is_enabled _ | Iopaque | Ibeginregion | Iendregion
    | Iintop_atomic _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istore _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Iintop Icheckbound | Iintop_imm (Icheckbound, _) ->
      (* does not allocate even when it raises because checkbound exception is
         static. *)
      transform t ~next ~exn ~effect:Value.safe "checkbound" dbg
    | Ipoll _ (* Ignore poll points even though they may trigger an allocations,
                 because otherwise all loops would be considered allocating when
                 poll insertion is enabled. [@poll error] should be used instead. *)
    | Ialloc { mode = Alloc_local; _ } ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ialloc { mode = Alloc_heap; _ } ->
      assert (not (Mach.operation_can_raise op));
      let r = Value.transform next in
      check t r "heap allocation" dbg
    | Iprobe { name; handler_code_sym } ->
      let desc = Printf.sprintf "probe %s handler %s" name handler_code_sym in
      transform_call t ~next ~exn handler_code_sym ~desc dbg
    | Icall_ind -> transform t ~next ~exn ~effect:Value.top "indirect call" dbg
    | Itailcall_ind ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      transform t ~next:Value.normal_return ~exn:Value.exn_escape
        ~effect:Value.top "indirect tailcall" dbg
    | Icall_imm { func } ->
      transform_call t ~next ~exn func ~desc:("direct call to " ^ func) dbg
    | Itailcall_imm { func } ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      transform_call t ~next:Value.normal_return ~exn:Value.exn_escape func
        ~desc:("direct tailcall to " ^ func)
        dbg
    | Iextcall { alloc = false; returns = true; _ } ->
      (* Sound to ignore [exn] because external call marked as noalloc does not
         raise. *)
      next
    | Iextcall { alloc = false; returns = false; _ } ->
      (* Sound to ignore [next] and [exn] because the call never returns or
         raises. *)
      Value.normal_return
    | Iextcall { func; alloc = true; _ } ->
      transform t ~next ~exn ~effect:Value.top ("external call to " ^ func) dbg
    | Ispecific s ->
      transform t ~next ~exn ~effect:(S.transform_specific s)
        "Arch.specific_operation" dbg

  module D = Dataflow.Backward ((Value : Dataflow.DOMAIN))

  let check_instr t body =
    let transfer (i : Mach.instruction) ~next ~exn =
      match i.desc with
      | Ireturn _ -> Value.normal_return
      | Iop op -> transform_operation t op ~next ~exn i.dbg
      | Iraise Raise_notrace ->
        (* [raise_notrace] is typically used for control flow, not for
           indicating an error. Therefore, we do not ignore any allocation on
           paths to it. The following conservatively assumes that normal and exn
           Returns are reachable. *)
        Value.join exn Value.safe
      | Iraise (Raise_reraise | Raise_regular) -> exn
      | Iend -> next
      | Iexit _ ->
        report t next ~msg:"transform" ~desc:"iexit" i.dbg;
        next
      | Iifthenelse _ | Iswitch _ -> next
      | Icatch (_rc, _ts, _, _body) ->
        report t next ~msg:"transform" ~desc:"catch" i.dbg;
        next
      | Itrywith (_body, (Regular | Delayed _), (_trap_stack, _handler)) ->
        report t next ~msg:"transform" ~desc:"try-with" i.dbg;
        next
    in
    (* By default, backward analysis does not check the property on paths that
       diverge (non-terminating loops that do not reach normal or exceptional
       return). All loops must go through an (Iexit label) instruction or a
       recursive function call. If (Iexit label) is not backward reachable from
       the function's Normal or Exceptional Return, either the loop diverges or
       the Iexit instruction is not reachable from function entry.

       To check divergent loops, the initial value of "div" component of all
       Iexit labels is set to "Safe" instead of "Bot". This is conservative with
       respect to non-recursive Icatch and Itrywith handlers. *)
    D.analyze ~exnescape:Value.exn_escape ~init_lbl:Value.diverges ~transfer
      body
    |> fst

  let fundecl (f : Mach.fundecl) ~future_funcnames unit_info ppf =
    let check () =
      let fun_name = f.fun_name in
      let t = create ppf fun_name future_funcnames unit_info in
      let a = Annotation.find f.fun_codegen_options S.property f.fun_dbg in
      Unit_info.record_annotation unit_info fun_name a;
      match a with
      | Some a when Annotation.is_assume a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assumed" ~desc:"fundecl" f.fun_dbg;
        Unit_info.join_value unit_info fun_name expected_value
      | None | Some _ ->
        report t Value.top ~msg:"assert" ~desc:"fundecl" f.fun_dbg;
        let res = check_instr t f.fun_body in
        let msg =
          if String.Set.is_empty t.unresolved_deps
          then "finished"
          else "unresolved deps"
        in
        report t res ~msg ~desc:"fundecl" f.fun_dbg;
        report_unit_info ppf unit_info ~msg:"before record deps";
        Unit_info.record_deps unit_info ~callees:t.unresolved_deps
          ~caller:fun_name;
        report_unit_info ppf unit_info ~msg:"after record deps";
        Unit_info.join_value unit_info fun_name res;
        report_unit_info ppf unit_info ~msg:"after join value";
        Unit_info.cleanup_deps unit_info fun_name;
        report_unit_info ppf unit_info ~msg:"after cleanup_deps"
    in
    if S.enabled ()
    then Profile.record_call ~accumulate:true ("check " ^ analysis_name) check
end

(** Check that functions do not allocate on the heap (local allocations are ignored) *)
module Spec_zero_alloc : Spec = struct
  let property = Cmm.Zero_alloc

  let enabled () = !Flambda_backend_flags.zero_alloc_check

  (* Compact the mapping from function name to Value.t to reduce size of Checks
     in cmx and memory consumption Compilenv. Different components have
     different frequencies of Top/Bot. The most likely value is encoded as None
     (i.e., not stored). *)
  let encode_return (v : V.t) =
    match v with Top -> None | Safe -> Some true | Bot -> Some false

  let decode_return = function
    | None -> V.Top
    | Some true -> V.Safe
    | Some false -> V.Bot

  let encode_diverge (v : V.t) =
    match v with Top -> Some false | Safe -> Some true | Bot -> None

  let decode_diverge = function
    | None -> V.Bot
    | Some true -> V.Safe
    | Some false -> V.Top

  let set_value s (v : Value.t) =
    let checks = (Compilenv.current_unit_infos ()).ui_checks in
    let new_value : Checks.value =
      { nor = encode_return v.nor;
        exn = encode_return v.exn;
        div = encode_diverge v.div
      }
    in
    Checks.set_value checks s new_value

  let get_value_opt s =
    let checks = Compilenv.cached_checks in
    match Checks.get_value checks s with
    | None -> None
    | Some ({ nor; exn; div } : Checks.value) ->
      Some
        { Value.nor = decode_return nor;
          exn = decode_return exn;
          div = decode_diverge div
        }

  let transform_specific s =
    (* Conservatively assume that operation can return normally. *)
    let nor = if Arch.operation_allocates s then V.Top else V.Safe in
    let exn = if Arch.operation_can_raise s then nor else V.Bot in
    (* Assume that the operation does not diverge. *)
    let div = V.Bot in
    { Value.nor; exn; div }
end

module Check_zero_alloc = Analysis (Spec_zero_alloc)

(** Information about the current unit. *)
let unit_info = Unit_info.create ()

let fundecl ppf_dump ~future_funcnames fd =
  Check_zero_alloc.fundecl fd ~future_funcnames unit_info ppf_dump;
  fd

let reset_unit_info () = Unit_info.reset unit_info

let record_unit_info ppf_dump =
  Check_zero_alloc.record_unit unit_info ppf_dump;
  Compilenv.cache_checks (Compilenv.current_unit_infos ()).ui_checks

let () = Location.register_error_of_exn Annotation.report_error
