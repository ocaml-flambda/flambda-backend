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

module Witness = struct
  type kind =
    | Alloc of
        { bytes : int;
          dbginfo : Debuginfo.alloc_dbginfo
        }
    | Indirect_call
    | Indirect_tailcall
    | Direct_call of { callee : string }
    | Direct_tailcall of { callee : string }
    | Missing_summary of { callee : string }
    | Forward_call of { callee : string }
    | Extcall of { callee : string }
    | Arch_specific
    | Probe of
        { name : string;
          handler_code_sym : string
        }

  type t =
    { dbg : Debuginfo.t;
      kind : kind
    }

  let create dbg kind = { dbg; kind }

  let compare { dbg = dbg1; kind = kind1 } { dbg = dbg2; kind = kind2 } =
    (* compare by [dbg] first to print the errors in the order they appear in
       the source file. *)
    let c = Debuginfo.compare dbg1 dbg2 in
    if c <> 0 then c else Stdlib.compare kind1 kind2

  let print_kind ppf kind =
    let open Format in
    match kind with
    | Alloc { bytes; dbginfo = _ } -> fprintf ppf "allocation of %d bytes" bytes
    | Indirect_call -> fprintf ppf "indirect call"
    | Indirect_tailcall -> fprintf ppf "indirect tailcall"
    | Direct_call { callee } -> fprintf ppf "direct call %s" callee
    | Direct_tailcall { callee : string } ->
      fprintf ppf "direct tailcall %s" callee
    | Missing_summary { callee } -> fprintf ppf "missing summary for %s" callee
    | Forward_call { callee } ->
      fprintf ppf "forward call or tailcall (conservatively handled) %s" callee
    | Extcall { callee } -> fprintf ppf "external call to %s" callee
    | Arch_specific -> fprintf ppf "arch specific operation"
    | Probe { name; handler_code_sym } ->
      fprintf ppf "probe %s handler %s" name handler_code_sym

  let print ppf { kind; dbg } =
    Format.fprintf ppf "%a %a" print_kind kind Debuginfo.print_compact dbg
end

module Witnesses : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val iter : t -> f:(Witness.t -> unit) -> unit

  val join : t -> t -> t

  val create : Witness.kind -> Debuginfo.t -> t

  val print : Format.formatter -> t -> unit

  val elements : t -> Witness.t list

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  val simplify : components -> components
end = struct
  include Set.Make (Witness)

  (* CR gyorsh: consider using Flambda_backend_flags.checkmach_details_cutoff to
     limit the size of this set. The downside is that it won't get tested as
     much. Only keep witnesses for functions that need checking. *)
  let join = union

  let create kind dbg = singleton (Witness.create dbg kind)

  let iter t ~f = iter f t

  let print ppf t = Format.pp_print_seq Witness.print ppf (to_seq t)

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  let simplify { nor; exn; div } =
    { div =
        (* don't print diverge witnesses unless they are the only ones. *)
        (if is_empty nor && is_empty exn then div else empty);
      nor;
      (* only print the exn witnesses that are not also nor witnesses. *)
      exn = diff exn nor
    }
end

(** Abstract value for each component of the domain. *)
module V : sig
  type t =
    | Top of Witnesses.t  (** Property may not hold on some paths. *)
    | Safe  (** Property holds on all paths.  *)
    | Bot  (** Not reachable. *)

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val transform : Witnesses.t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val diff_witnesses : expected:t -> actual:t -> Witnesses.t

  val get_witnesses : t -> Witnesses.t

  val is_not_safe : t -> bool

  val print : witnesses:bool -> Format.formatter -> t -> unit
end = struct
  type t =
    | Top of Witnesses.t
    | Safe
    | Bot

  let join c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.join w1 w2)
    | Safe, Bot | Bot, Safe -> Safe
    | Top w1, Bot | Top w1, Safe | Bot, Top w1 | Safe, Top w1 -> Top w1

  let lessequal v1 v2 =
    match v1, v2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top _, Top _ -> true
    | Bot, Safe -> true
    | Bot, Top _ -> true
    | Safe, Top _ -> true
    | Top _, (Bot | Safe) -> false
    | Safe, Bot -> false

  (** abstract transformer (backward analysis) for a statement that violates the property
      but doesn't alter control flow. *)
  let transform w = function
    | Bot ->
      (* if a return is unreachable from the program location immediately after
         the statement, then return is unreachable from the program location
         immediately before the statement. *)
      Bot
    | Safe -> Top w
    | Top w' -> Top (Witnesses.join w w')

  let replace_witnesses w t = match t with Top _ -> Top w | Bot | Safe -> t

  let get_witnesses t =
    match t with Top w -> w | Bot | Safe -> Witnesses.empty

  let diff_witnesses ~expected ~actual =
    if lessequal actual expected
    then Witnesses.empty
    else (
      assert (expected = Safe);
      match actual with Bot | Safe -> assert false | Top w -> w)

  let is_not_safe = function Top _ -> true | Safe | Bot -> false

  let print ~witnesses ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Top w ->
      Format.fprintf ppf "top";
      if witnesses then Format.fprintf ppf " (%a)" Witnesses.print w
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

  val top : Witnesses.t -> t

  val bot : t

  val normal_return : t

  val exn_escape : t

  val diverges : t

  val safe : t

  val relaxed : Witnesses.t -> t

  val print : witnesses:bool -> Format.formatter -> t -> unit

  val transform : Witnesses.t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val get_witnesses : t -> Witnesses.components

  val diff_witnesses : expected:t -> actual:t -> Witnesses.components
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

  let transform w v =
    { nor = V.transform w v.nor;
      exn = V.transform w v.exn;
      div = V.transform w v.div
    }

  let replace_witnesses w t =
    { nor = V.replace_witnesses w t.nor;
      exn = V.replace_witnesses w t.exn;
      div = V.replace_witnesses w t.div
    }

  let diff_witnesses ~expected ~actual =
    { Witnesses.nor = V.diff_witnesses ~expected:expected.nor ~actual:actual.nor;
      Witnesses.exn = V.diff_witnesses ~expected:expected.exn ~actual:actual.exn;
      Witnesses.div = V.diff_witnesses ~expected:expected.div ~actual:actual.div
    }

  let get_witnesses t =
    { Witnesses.nor = V.get_witnesses t.nor;
      Witnesses.exn = V.get_witnesses t.exn;
      Witnesses.div = V.get_witnesses t.div
    }

  let normal_return = { bot with nor = V.Safe }

  let exn_escape = { bot with exn = V.Safe }

  let diverges = { bot with div = V.Safe }

  let safe = { nor = V.Safe; exn = V.Safe; div = V.Safe }

  let top w = { nor = V.Top w; exn = V.Top w; div = V.Top w }

  let relaxed w = { nor = V.Safe; exn = V.Top w; div = V.Top w }

  let print ~witnesses ppf { nor; exn; div } =
    let pp = V.print ~witnesses in
    Format.fprintf ppf "{ nor=%a; exn=%a; div=%a }" pp nor pp exn pp div
end

(**  Representation of user-provided annotations as abstract values *)
module Annotation : sig
  type t

  val get_loc : t -> Location.t

  val find :
    Cmm.codegen_option list -> Cmm.property -> string -> Debuginfo.t -> t option

  val expected_value : t -> Value.t

  val is_assume : t -> bool

  val is_strict : t -> bool

  val is_check_enabled :
    Cmm.codegen_option list -> string -> Debuginfo.t -> bool
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
      never_returns_normally : bool;
      loc : Location.t
          (** Source location of the annotation, used for error messages. *)
    }

  let get_loc t = t.loc

  let expected_value t =
    let res = if t.strict then Value.safe else Value.relaxed Witnesses.empty in
    if t.never_returns_normally then { res with nor = V.Bot } else res

  let is_assume t = t.assume

  let is_strict t = t.strict

  let find codegen_options spec fun_name dbg =
    let ignore_assert_all = ref false in
    let a =
      List.filter_map
        (fun (c : Cmm.codegen_option) ->
          match c with
          | Check { property; strict; loc } when property = spec ->
            Some { strict; assume = false; never_returns_normally = false; loc }
          | Assume { property; strict; never_returns_normally; loc }
            when property = spec ->
            Some { strict; assume = true; never_returns_normally; loc }
          | Ignore_assert_all property when property = spec ->
            ignore_assert_all := true;
            None
          | Ignore_assert_all _ | Check _ | Assume _ | Reduce_code_size | No_CSE
          | Use_linscan_regalloc ->
            None)
        codegen_options
    in
    match a with
    | [] ->
      if !Clflags.zero_alloc_check_assert_all && not !ignore_assert_all
      then
        Some
          { strict = false;
            assume = false;
            never_returns_normally = false;
            loc = Debuginfo.to_location dbg
          }
      else None
    | [p] -> Some p
    | _ :: _ ->
      Misc.fatal_errorf "Unexpected duplicate annotation %a for %s"
        Debuginfo.print_compact dbg fun_name ()

  let is_check_enabled codegen_options fun_name dbg =
    let is_enabled p =
      match find codegen_options p fun_name dbg with
      | None -> false
      | Some { assume; _ } -> not assume
    in
    List.exists is_enabled Cmm.all_properties
end

module Report : sig
  type t =
    { a : Annotation.t;
      fun_name : string;
      fun_dbg : Debuginfo.t;
      witnesses : Witnesses.components
    }

  exception Fail of t list * Cmm.property

  val print : exn -> Location.error option
end = struct
  type t =
    { a : Annotation.t;
      fun_name : string;
      fun_dbg : Debuginfo.t;
      witnesses : Witnesses.components
    }

  exception Fail of t list * Cmm.property

  let annotation_error ~property_name t =
    (* print location of the annotation, print function name as part of the
       message. *)
    let loc = Annotation.get_loc t.a in
    let print_annotated_fun ppf () =
      let scoped_name =
        t.fun_dbg |> Debuginfo.get_dbg |> Debuginfo.Dbg.to_list
        |> List.map (fun dbg ->
               Debuginfo.(Scoped_location.string_of_scopes dbg.dinfo_scopes))
        |> String.concat ","
      in
      Format.fprintf ppf "Annotation check for %s%s failed on function %s (%s)"
        property_name
        (if Annotation.is_strict t.a then " strict" else "")
        scoped_name t.fun_name
    in
    Location.error_of_printer ~loc print_annotated_fun ()

  let error_messages ~property_name t : Location.error list =
    let pp_inlined_dbg ppf dbg =
      (* Show inlined locations, if dbg has more than one item. The first item
         will be shown at the start of the error message. *)
      if Debuginfo.Dbg.length (Debuginfo.get_dbg dbg) > 1
      then Format.fprintf ppf " (%a)" Debuginfo.print_compact dbg
    in
    let print_comballoc dbg =
      match dbg with
      | [] | [_] -> "", []
      | alloc_dbginfo ->
        (* If one Ialloc is a result of combining multiple allocations, print
           details of each location. Currently, this cannot happen because
           checkmach is before comballoc. In the future, this may be done in the
           middle-end. *)
        let msg =
          Printf.sprintf " combining %d allocations below"
            (List.length alloc_dbginfo)
        in
        let details =
          List.map
            (fun (item : Debuginfo.alloc_dbginfo_item) ->
              let pp_alloc ppf =
                Format.fprintf ppf "allocate %d words%a" item.alloc_words
                  pp_inlined_dbg item.alloc_dbg
              in
              let aloc = Debuginfo.to_location item.alloc_dbg in
              Location.mkloc pp_alloc aloc)
            alloc_dbginfo
        in
        msg, details
    in
    let print_witness (w : Witness.t) ~component =
      (* print location of the witness, print witness description. *)
      let loc = Debuginfo.to_location w.dbg in
      let component_msg =
        if String.equal "" component
        then component
        else " on a path to " ^ component
      in
      let print_main_msg, sub =
        match w.kind with
        | Alloc { bytes = _; dbginfo } ->
          let comballoc_msg, sub = print_comballoc dbginfo in
          ( Format.dprintf "%a%s%s" Witness.print_kind w.kind component_msg
              comballoc_msg,
            sub )
        | Indirect_call | Indirect_tailcall | Direct_call _ | Direct_tailcall _
        | Missing_summary _ | Forward_call _ | Extcall _ ->
          ( Format.dprintf "called function may allocate%s (%a)" component_msg
              Witness.print_kind w.kind,
            [] )
        | Arch_specific | Probe _ ->
          ( Format.dprintf "expression may allocate%s@ (%a)" component_msg
              Witness.print_kind w.kind,
            [] )
      in
      let pp ppf () =
        print_main_msg ppf;
        pp_inlined_dbg ppf w.dbg
      in
      Location.error_of_printer ~loc ~sub pp ()
    in
    let print_witnesses ws : Location.error list =
      let { Witnesses.nor; exn; div } = Witnesses.simplify ws in
      let f ws component =
        ws |> Witnesses.elements |> List.map (print_witness ~component)
      in
      List.concat [f div "diverge"; f nor ""; f exn "exceptional return"]
    in
    let details =
      match !Flambda_backend_flags.checkmach_details_cutoff with
      | No_details ->
        (* do not print witnesses. *)
        []
      | Keep_all -> print_witnesses t.witnesses
      | At_most cutoff ->
        let all = print_witnesses t.witnesses in
        if List.compare_length_with all cutoff <= 0
        then all
        else
          let result, _ = Misc.Stdlib.List.split_at cutoff all in
          result
    in
    annotation_error ~property_name t :: details

  let rec print_all msgs =
    (* Print all errors message in a compilation unit as separate messages to
       make editor integration easier. *)
    match msgs with
    | [] -> assert false
    | [last_error] ->
      (* Finally, raise Error with the last function. *)
      Some last_error
    | error :: tl ->
      Location.print_report Format.err_formatter error;
      print_all tl

  let print = function
    | Fail (reports, property) ->
      let property_name = Printcmm.property_to_string property in
      (* Sort by function's location. If debuginfo is missing, keep sorted by
         function name. *)
      let compare t1 t2 =
        let c = Debuginfo.compare t1.fun_dbg t2.fun_dbg in
        if not (Int.equal c 0)
        then c
        else String.compare t1.fun_name t2.fun_name
      in
      reports |> List.stable_sort compare
      |> List.concat_map (error_messages ~property_name)
      |> print_all
    | _ -> None
end

module Func_info = struct
  (* Fields are mutable: [annotation] changes when a call appears before the
     declaration. [value] and [unresolved_callers] change to track and update
     dependencies as the compilation unit is scanned. *)
  type t =
    { name : string;  (** function name *)
      mutable dbg : Debuginfo.t;  (** debug info associated with the function *)
      mutable value : Value.t;  (** the result of the check *)
      mutable annotation : Annotation.t option;
          (** [value] must be lessequal than the expected value
          if there is user-defined annotation on this function. *)
      mutable unresolved_callers : String.Set.t;  (** direct callers  *)
      mutable unresolved_callees : Witnesses.t String.Map.t
          (** direct callees  *)
    }

  let create name =
    { name;
      dbg = Debuginfo.none;
      value = Value.bot;
      annotation = None;
      unresolved_callers = String.Set.empty;
      unresolved_callees = String.Map.empty
    }

  let is_resolved t = String.Map.is_empty t.unresolved_callees

  let print ~witnesses ~msg ppf t =
    let open Format in
    let print_names ppf set =
      pp_print_seq
        ~pp_sep:(fun ppf () -> pp_print_char ppf ' ')
        pp_print_string ppf set
    in
    fprintf ppf "%s %s %a@,(unresolved callees: %a)@,(unresolved callers: %a)@."
      msg t.name (Value.print ~witnesses) t.value print_names
      (t.unresolved_callees |> String.Map.to_seq |> Seq.map (fun (k, _) -> k))
      print_names
      (String.Set.to_seq t.unresolved_callers)
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
  val transform_specific : Witnesses.t -> Arch.specific_operation -> Value.t

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
  val record_deps :
    t -> caller:string -> callees:Witnesses.t String.Map.t -> unit

  (** [cleanup_deps] remove resolved dependencies starting from [name]. *)
  val cleanup_deps : t -> string -> unit

  val record_annotation :
    t -> string -> Debuginfo.t -> Annotation.t option -> unit

  val resolve_all : t -> unit

  val iter : t -> f:(Func_info.t -> unit) -> unit

  val should_keep_witnesses : bool -> bool
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

  let should_keep_witnesses keep =
    match !Flambda_backend_flags.checkmach_details_cutoff with
    | Keep_all -> true
    | No_details -> false
    | At_most _ -> keep

  (* fixpoint backward propogation of function summaries along the recorded
     dependency edges. *)
  let rec propagate t (func_info : Func_info.t) =
    let unresolved_callers = func_info.unresolved_callers in
    let value = func_info.value in
    let value =
      (* conservative use of summaries for unresolved dependencies *)
      let w =
        if should_keep_witnesses true
        then
          Witnesses.create
            (Forward_call { callee = func_info.name })
            Debuginfo.none
        else Witnesses.empty
      in
      let v = V.join value.nor value.exn |> V.replace_witnesses w in
      { Value.nor = v; exn = v; div = value.div }
    in
    String.Set.iter
      (replace_witnesses_and_propagate t ~value ~callee:func_info.name)
      unresolved_callers

  and replace_witnesses_and_propagate t ~(value : Value.t) ~callee name =
    let func_info = get_exn t name in
    match String.Map.find_opt callee func_info.unresolved_callees with
    | None ->
      Misc.fatal_errorf "missing witnesses for unresolved_callee %s of %s"
        callee name
    | Some w ->
      let v = V.replace_witnesses w value.nor in
      let value = { value with nor = v; exn = v } in
      join_and_propagate t ~value name

  and join_and_propagate t ~value name =
    let func_info = get_exn t name in
    let new_value = Value.join func_info.value value in
    let old_value = func_info.value in
    (* propagate witnesses *)
    func_info.value <- new_value;
    if not (Value.lessequal new_value old_value) then propagate t func_info

  let iter t ~f = String.Tbl.iter (fun _ func_info -> f func_info) t

  let resolve_all t = iter t ~f:(propagate t)

  let add_value t name value =
    let (_ : Func_info.t) = get_or_create t name in
    join_and_propagate t name ~value

  let join_value t name value = join_and_propagate t name ~value

  let record_annotation t name dbg annotation =
    let func_info = get_or_create t name in
    if Option.is_some func_info.annotation
    then Misc.fatal_errorf "Duplicate symbol %s" name;
    func_info.dbg <- dbg;
    func_info.annotation <- annotation

  let record_deps t ~caller ~callees =
    let func_info = get_exn t caller in
    if not (String.Map.is_empty func_info.unresolved_callees)
    then Misc.fatal_errorf "Unexpected unresolved callees for %s" caller;
    func_info.unresolved_callees <- callees;
    String.Map.iter
      (fun callee _ ->
        let func_info = get_or_create t callee in
        func_info.unresolved_callers
          <- String.Set.add caller func_info.unresolved_callers)
      callees

  let rec cleanup_deps t name =
    (* optimization: clean up unresolved *)
    let func_info = get_exn t name in
    let unresolved_callees_except_self =
      String.Map.remove name func_info.unresolved_callees
    in
    if String.Map.is_empty unresolved_callees_except_self
    then (
      (* remove all resolved deps *)
      let unresolved_callers = func_info.unresolved_callers in
      func_info.unresolved_callers <- String.Set.empty;
      let changed_callers =
        String.Set.filter
          (fun caller ->
            let caller_info : Func_info.t = get_exn t caller in
            if String.Map.mem name caller_info.unresolved_callees
            then (
              caller_info.unresolved_callees
                <- String.Map.remove name caller_info.unresolved_callees;
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
      mutable unresolved_deps : Witnesses.t String.Map.t;
          (** must be the current compilation unit.  *)
      unit_info : Unit_info.t;
      mutable keep_witnesses : bool
    }

  let create ppf current_fun_name future_funcnames unit_info =
    { ppf;
      current_fun_name;
      future_funcnames;
      unresolved_deps = String.Map.empty;
      unit_info;
      keep_witnesses = false
    }

  let analysis_name = Printcmm.property_to_string S.property

  let report' ppf v ~current_fun_name ~msg ~desc dbg =
    if !Flambda_backend_flags.dump_checkmach
    then
      Format.fprintf ppf "*** check %s %s in %s: %s with %a (%a)\n"
        analysis_name msg current_fun_name desc
        (Value.print ~witnesses:true)
        v Debuginfo.print_compact dbg

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
      Unit_info.iter unit_info ~f:(Func_info.print ~witnesses:true ppf ~msg)

  let report_func_info ~msg ppf func_info =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Func_info.print ~witnesses:true ppf ~msg func_info

  let record_unit unit_info ppf =
    report_unit_info ppf unit_info ~msg:"before resolve_all";
    Unit_info.resolve_all unit_info;
    let errors = ref [] in
    let record (func_info : Func_info.t) =
      (match func_info.annotation with
      | None -> ()
      | Some a ->
        Builtin_attributes.mark_property_checked analysis_name
          (Annotation.get_loc a);
        let expected_value = Annotation.expected_value a in
        if (not (Annotation.is_assume a))
           && S.enabled ()
           && not (Value.lessequal func_info.value expected_value)
        then
          (* CR-soon gyorsh: keeping track of all the witnesses until the end of
             the compilation unit will be expensive. For functions that do not
             have any dependencies, we can check annotations earlier, as soon as
             the function is analyzed, or as soon as its dependencies are
             resolved, print the error, and remove the witnesses from the stored
             values. *)
          (* CR gyorsh: we can add error recovering mode where we sets the
             expected value as the actual value and continue analysis of other
             functions. *)
          let witnesses =
            Value.diff_witnesses ~expected:expected_value
              ~actual:func_info.value
          in
          errors
            := { Report.a;
                 fun_name = func_info.name;
                 fun_dbg = func_info.dbg;
                 witnesses
               }
               :: !errors);
      report_func_info ~msg:"record" ppf func_info;
      S.set_value func_info.name func_info.value
    in
    Unit_info.iter unit_info ~f:record;
    match !errors with
    | [] -> ()
    | errors -> raise (Report.Fail (errors, S.property))

  let record_unit unit_info ppf =
    Profile.record_call ~accumulate:true ("record_unit " ^ analysis_name)
      (fun () -> record_unit unit_info ppf)

  let update_deps t v dep w desc dbg =
    match dep with
    | Some callee ->
      let f old =
        match old with
        | None -> Some w
        | Some old_w -> Some (Witnesses.join w old_w)
      in
      t.unresolved_deps <- String.Map.update callee f t.unresolved_deps;
      report t v ~msg:"unresolved" ~desc dbg
    | None -> report t v ~msg:"resolved" ~desc dbg

  let[@inline always] create_witnesses t kind dbg =
    if t.keep_witnesses then Witnesses.create kind dbg else Witnesses.empty

  (* [find_callee] returns the value associated with the callee and a new
     dependency to record if there is one. *)
  let find_callee t callee dbg =
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
            let w = create_witnesses t (Missing_summary { callee }) dbg in
            let v = Value.top w in
            report t v ~msg:"callee compiled without checks" ~desc:callee
              Debuginfo.none;
            v
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
    | V.Top w -> Value.transform w dst

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
    report t r ~msg:"transform result" ~desc dbg;
    check t r desc dbg

  let transform_top t ~next ~exn w desc dbg =
    let effect =
      if Debuginfo.assume_zero_alloc dbg then Value.relaxed w else Value.top w
    in
    transform t ~next ~exn ~effect desc dbg

  let transform_call t ~next ~exn callee w ~desc dbg =
    report t next ~msg:"transform_call next" ~desc dbg;
    report t exn ~msg:"transform_call exn" ~desc dbg;
    let effect =
      if Debuginfo.assume_zero_alloc dbg
      then Value.relaxed w
      else
        let callee_value, new_dep = find_callee t callee dbg in
        update_deps t callee_value new_dep w desc dbg;
        (* Abstract witnesses of a call to the single witness for the callee
           name. Summary of tailcall self won't be affected because it is set to
           Safe not Top by [find_callee]. *)
        Value.replace_witnesses w callee_value
    in
    transform t ~next ~exn ~effect desc dbg

  let transform_operation t (op : Mach.operation) ~next ~exn dbg =
    match op with
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _ | Iconst_symbol _
    | Iconst_vec128 _ | Iload _ | Icompf _ | Inegf | Iabsf | Iaddf | Isubf
    | Imulf | Idivf | Ifloatofint | Iintoffloat | Ivectorcast _ | Iscalarcast _
    | Iintop_imm
        ( ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ ),
          _ )
    | Iintop
        ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
        | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ )
    | Icsel _ ->
      assert (Mach.operation_is_pure op);
      assert (not (Mach.operation_can_raise op));
      next
    | Iname_for_debugger _ | Ivalueofint | Iintofvalue ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istackoffset _ | Iprobe_is_enabled _ | Iopaque | Ibeginregion | Iendregion
    | Iintop_atomic _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istore _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ipoll _
    (* Ignore poll points even though they may trigger an allocations, because
       otherwise all loops would be considered allocating when poll insertion is
       enabled. [@poll error] should be used instead. *)
    | Ialloc { mode = Alloc_local; _ } ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ialloc { mode = Alloc_heap; bytes; dbginfo } ->
      assert (not (Mach.operation_can_raise op));
      if Debuginfo.assume_zero_alloc dbg
      then next
      else
        let w = create_witnesses t (Alloc { bytes; dbginfo }) dbg in
        let r = Value.transform w next in
        check t r "heap allocation" dbg
    | Iprobe { name; handler_code_sym; enabled_at_init = __ } ->
      let desc = Printf.sprintf "probe %s handler %s" name handler_code_sym in
      let w = create_witnesses t (Probe { name; handler_code_sym }) dbg in
      transform_call t ~next ~exn handler_code_sym w ~desc dbg
    | Icall_ind ->
      let w = create_witnesses t Indirect_call dbg in
      transform_top t ~next ~exn w "indirect call" dbg
    | Itailcall_ind ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      let w = create_witnesses t Indirect_tailcall dbg in
      transform_top t ~next:Value.normal_return ~exn:Value.exn_escape w
        "indirect tailcall" dbg
    | Icall_imm { func = { sym_name = func; _ } } ->
      let w = create_witnesses t (Direct_call { callee = func }) dbg in
      transform_call t ~next ~exn func w ~desc:("direct call to " ^ func) dbg
    | Itailcall_imm { func = { sym_name = func; _ } } ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      let w = create_witnesses t (Direct_tailcall { callee = func }) dbg in
      transform_call t ~next:Value.normal_return ~exn:Value.exn_escape func w
        ~desc:("direct tailcall to " ^ func)
        dbg
    | Iextcall { alloc = false; returns = true; _ } ->
      (* Sound to ignore [exn] because external call marked as noalloc does not
         raise. *)
      next
    | Iextcall { alloc = false; returns = false; _ } ->
      (* Sound to ignore [next] and [exn] because the call never returns or
         raises. *)
      Value.bot
    | Iextcall { func; alloc = true; _ } ->
      let w = create_witnesses t (Extcall { callee = func }) dbg in
      transform_top t ~next ~exn w ("external call to " ^ func) dbg
    | Ispecific s ->
      let effect =
        let w = create_witnesses t Arch_specific dbg in
        if Debuginfo.assume_zero_alloc dbg
        then
          (* Conservatively assume that operation can return normally. *)
          let nor = V.Safe in
          let exn = if Arch.operation_can_raise s then V.Top w else V.Bot in
          (* Assume that the operation does not diverge. *)
          let div = V.Bot in
          { Value.nor; exn; div }
        else S.transform_specific w s
      in
      transform t ~next ~exn ~effect "Arch.specific_operation" dbg
    | Idls_get -> Misc.fatal_error "Idls_get not supported"

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
      | Itrywith (_body, _, (_trap_stack, _handler)) ->
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
       Iexit labels of recurisve Icatch handlers is set to "Safe" instead of
       "Bot". *)
    D.analyze ~exnescape:Value.exn_escape ~init_rc_lbl:Value.diverges ~transfer
      body
    |> fst

  let fundecl (f : Mach.fundecl) ~future_funcnames unit_info ppf =
    let check () =
      let fun_name = f.fun_name in
      let t = create ppf fun_name future_funcnames unit_info in
      let a =
        Annotation.find f.fun_codegen_options S.property fun_name f.fun_dbg
      in
      Unit_info.record_annotation unit_info fun_name f.fun_dbg a;
      let really_check ~keep_witnesses =
        t.keep_witnesses <- Unit_info.should_keep_witnesses keep_witnesses;
        let res = check_instr t f.fun_body in
        let msg =
          if String.Map.is_empty t.unresolved_deps
          then "finished"
          else "unresolved deps"
        in
        report t res ~msg ~desc:"fundecl" f.fun_dbg;
        if not t.keep_witnesses
        then (
          let { Witnesses.nor; exn; div } = Value.get_witnesses res in
          assert (Witnesses.is_empty nor);
          assert (Witnesses.is_empty exn);
          assert (Witnesses.is_empty div));
        report_unit_info ppf unit_info ~msg:"before record deps";
        Unit_info.record_deps unit_info ~callees:t.unresolved_deps
          ~caller:fun_name;
        report_unit_info ppf unit_info ~msg:"after record deps";
        Unit_info.join_value unit_info fun_name res;
        report_unit_info ppf unit_info ~msg:"after join value";
        Unit_info.cleanup_deps unit_info fun_name;
        report_unit_info ppf unit_info ~msg:"after cleanup_deps"
      in
      let really_check ~keep_witnesses =
        if !Flambda_backend_flags.disable_checkmach
        then
          (* Do not analyze the body of the function, conservatively assume that
             the summary is top. *)
          Unit_info.join_value unit_info fun_name (Value.top Witnesses.empty)
        else really_check ~keep_witnesses
      in
      match a with
      | Some a when Annotation.is_assume a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assumed" ~desc:"fundecl" f.fun_dbg;
        Unit_info.join_value unit_info fun_name expected_value
      | None -> really_check ~keep_witnesses:false
      | Some a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assert" ~desc:"fundecl" f.fun_dbg;
        (* Only keep witnesses for functions that need checking. *)
        really_check ~keep_witnesses:true
    in
    Profile.record_call ~accumulate:true ("check " ^ analysis_name) check
end

(** Check that functions do not allocate on the heap (local allocations are ignored) *)
module Spec_zero_alloc : Spec = struct
  let property = Cmm.Zero_alloc

  let enabled () =
    (* Checkmach no longer distinguishes between opt and default checks. *)
    match !Clflags.zero_alloc_check with
    | No_check -> false
    | Check_default -> true
    | Check_all -> true
    | Check_opt_only -> true

  (* Compact the mapping from function name to Value.t to reduce size of Checks
     in cmx and memory consumption Compilenv. Different components have
     different frequencies of Top/Bot. The most likely value is encoded as None
     (i.e., not stored). *)
  let encode (v : V.t) = match v with Top _ -> 0 | Safe -> 1 | Bot -> 2

  (* Witnesses are not used across functions and not stored in cmx. Witnesses
     that appear in a function's summary are only used for error messages about
     that function, not about its callers. Witnesses from the summary of a
     callee are ignored, and replaced by the name of the callee. *)
  let decoded_witness = Witnesses.empty

  let decode = function
    | 0 -> V.Top decoded_witness
    | 1 -> V.Safe
    | 2 -> V.Bot
    | n -> Misc.fatal_errorf "Checkmach cannot decode %d" n

  let encode (v : Value.t) : Checks.value =
    let c = (encode v.div lsl 4) lor (encode v.exn lsl 2) lor encode v.nor in
    if c = 0 then None else Some c

  let decode : Checks.value -> Value.t = function
    | None -> Value.top decoded_witness
    | Some d ->
      if d = 0 then Misc.fatal_error "Checkmach unexpected 0 encoding";
      let nor = decode (d land 3) in
      let exn = decode ((d lsr 2) land 3) in
      let div = decode ((d lsr 4) land 3) in
      { nor; exn; div }

  let set_value s (v : Value.t) =
    let checks = (Compilenv.current_unit_infos ()).ui_checks in
    Checks.set_value checks s (encode v)

  let get_value_opt s =
    let checks = Compilenv.cached_checks in
    match Checks.get_value checks s with
    | None -> None
    | Some (c : Checks.value) -> Some (decode c)

  let transform_specific w s =
    (* Conservatively assume that operation can return normally. *)
    let nor = if Arch.operation_allocates s then V.Top w else V.Safe in
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

type iter_witnesses = (string -> Witnesses.components -> unit) -> unit

let iter_witnesses f =
  Unit_info.iter unit_info ~f:(fun func_info ->
      f func_info.name
        (Value.get_witnesses func_info.value |> Witnesses.simplify))

let () = Location.register_error_of_exn Report.print

let is_check_enabled codegen_options fun_name dbg =
  Annotation.is_check_enabled codegen_options fun_name dbg
