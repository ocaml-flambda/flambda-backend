(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020--2021 OCamlPro SAS                                    *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inlining reports for Flambda2.
 *
 *   Given the history of decisions made by the inliner, reconstructing the
 *   implied tree of decisions made by the inliner is feasible.
 *
 *    Let's take the following bit of code:
 *    ```
 *    (* Module A*)
 *    let foo arg =
 *       let[@inline never] bar arg = .... in
 *       bar x
 *
 *    let baz_1 x = foo x
 *    let baz_2 x = (foo [@@inlined never]) x
 *    ```
 *
 *    Let's assume that simplify inlined the call to [foo]
 *    in [baz_1] and doesn't inline the call to [foo].
 *
 *    Then the inliner will have taken the following decisions:
 *    - Mark the function [foo] as inlinable
 *    - Mark the function [foo.bar] as non inlinable
 *    - Mark the function [baz_1] as inlinable
 *    - Inline the call to [baz_1.foo]
 *    - Do not inline the call to [bar] that was created as a
 *      consequence of inlining [foo.baz_1]
 *    - Mark the call to [baz_2.foo] as non inlinable
 *
 *    All of this can also be represented into the following tree:
 *    Module A
 *    ├─► Declaration of Function [foo]
 *    │   │ Deemed inlinable
 *    │   ├─► Declaration of function [bar]
 *    │   │   │ Deemed as non inlinable
 *    │   └─► Call to [bar]
 *    │       │ Non inlined because the callee was deemed to be non inlinable
 *    └─► Definition of [baz_1]
 *        │ Deemed inlinable
 *        ├─► Call to [foo]
 *        │   │ Inlined
 *        │   ├─► Declaration of function [bar]
 *        │   │   │ Deemed as non inlinable
 *        │   └─► Call to [bar] that was first defined at [foo.bar]
 *        │       │ Non inlined because the callee was deemed to be non inlinable
 *        └─► Call to [foo]
 *            │ Not inlined because of an annotation
 *
 *    These inlining reports are:
 *    - Recording all decisions made during [simplify]
 *    - Reconstructing the inlining tree from these decisions
 *    - Printing this tree to a human-readable file compatible with org mode
 *)

[@@@ocaml.warning "+a-30-40-41-42"]

type pass =
  | After_closure_conversion
  | Before_simplify
  | After_simplify

module Table : sig
  (* Pretty print a table of values.

     The width of cells containing a float or an int is determined using the
     width of the string returned by [string_of_float] and [string_of_int]
     respectively. *)
  type t

  val create :
    [< `Float of float | `Int of int | `String of string] list list -> t

  val print : Format.formatter -> t -> unit
end = struct
  type t = int list * string list list

  let create cells =
    let cells_with_size =
      List.map
        (fun row ->
          List.map
            (fun cell ->
              let s_cell =
                match cell with
                | `Float f -> string_of_float f
                | `Int d -> string_of_int d
                | `String s -> s
              in
              s_cell, String.length s_cell)
            row)
        cells
    in
    let sizes =
      List.fold_left
        (fun sizes row_sizes ->
          List.map2
            (fun s1 (_, s2) -> if s1 < s2 then s2 else s1)
            sizes row_sizes)
        (List.hd cells_with_size |> List.map snd)
        cells_with_size
    in
    let cells = List.map (List.map fst) cells_with_size in
    sizes, cells

  let table_line (sizes, _) =
    let dashes = List.map (fun s -> String.make s '-') sizes in
    "|-" ^ String.concat "-+-" dashes ^ "-|"

  let print_row ppf (size, cells) =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf "|"
      | (width, c) :: rest -> Format.fprintf ppf "| %*s %a" width c loop rest
    in
    loop ppf (List.combine size cells)

  let print_table_values ~table_line ppf (size, cells) =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf "@[<h>%s@]" table_line
      | row :: rest ->
        Format.fprintf ppf "@[<h>%s@]@;@[<h>%a@]@;%a" table_line print_row
          (size, row) loop rest
    in
    loop ppf cells

  let print ppf t =
    let table_line = table_line t in
    Format.fprintf ppf "@[<v>%a@]" (print_table_values ~table_line) t
end

module Context = struct
  (* Represents the context under which an inlining decision was taken. *)
  type t =
    { args : Inlining_arguments.t;
      cost_metrics : Cost_metrics.t option;
      depth : int option;
      unrolling_depth : int option option;
      are_rebuilding_terms : Are_rebuilding_terms.t
    }

  let create ?depth ?unrolling_depth ?cost_metrics ~are_rebuilding_terms ~args
      () =
    { args; depth; unrolling_depth; cost_metrics; are_rebuilding_terms }

  let print ppf
      { args; cost_metrics; depth; unrolling_depth; are_rebuilding_terms } =
    let print_unrolling_depth ppf = function
      | None -> ()
      | Some (Some unroll) ->
        Format.fprintf ppf "@[<h>Unrolling@ depth:@ %d@]@.@." unroll
      | Some None -> Format.fprintf ppf "@[<h>Unrolling@ depth unknown@]@.@."
    in
    let print_args ppf args =
      let table =
        [ [ `String "max inlining depth";
            `Int (Inlining_arguments.max_inlining_depth args);
            `String "call cost";
            `Float (Inlining_arguments.call_cost args) ];
          [ `String "small function size";
            `Int (Inlining_arguments.small_function_size args);
            `String "alloc cost";
            `Float (Inlining_arguments.alloc_cost args) ];
          [ `String "large function size";
            `Int (Inlining_arguments.large_function_size args);
            `String "branch cost";
            `Float (Inlining_arguments.branch_cost args) ];
          [ `String "threshold";
            `Float (Inlining_arguments.threshold args);
            `String "indirect call cost";
            `Float (Inlining_arguments.indirect_call_cost args) ];
          [ `String "prim cost";
            `Float (Inlining_arguments.prim_cost args);
            `String "poly compare cost";
            `Float (Inlining_arguments.poly_compare_cost args) ] ]
        |> Table.create
      in
      Format.fprintf ppf "@[<h>Inlining@ arguments:@;@]@.@[<h>%a]@.@."
        Table.print table
    in
    let print_cost_metrics ppf = function
      | None -> ()
      | Some c ->
        let Removed_operations.
              { call;
                alloc;
                prim;
                branch;
                direct_call_of_indirect;
                specialized_poly_compare;
                requested_inline
              } =
          Cost_metrics.removed c
        in
        let table =
          [ [ `String "Call";
              `String "Alloc";
              `String "Prim";
              `String "Branch";
              `String "Direct call of indirect";
              `String "Specialized poly compare";
              `String "Requested inline" ];
            [ `Int call;
              `Int alloc;
              `Int prim;
              `Int branch;
              `Int direct_call_of_indirect;
              `Int specialized_poly_compare;
              `Int requested_inline ] ]
          |> Table.create
        in

        Format.fprintf ppf
          "@[<v>@[<h>Code@ size@ was@ estimated@ to@ be@ %a@]@.@[<h>Benefits@ \
           of@ inlining@ this@ call:@;\
           @]@.@[<h>%a@]@]@.@."
          Code_size.print (Cost_metrics.size c) Table.print table
    in
    let print_depth ppf = function
      | None -> ()
      | Some c ->
        Format.fprintf ppf "@[<h>Considered@ at@ inlining@ depth@ of@ %d@]@.@."
          c
    in
    let print_are_rebuilding_terms ppf t =
      Format.fprintf ppf
        "@[<h>Considered@ with@ are_rebuilding_terms@ =@ %a@]@.@."
        Are_rebuilding_terms.print t
    in
    Format.fprintf ppf "@[";
    print_are_rebuilding_terms ppf are_rebuilding_terms;
    print_args ppf args;
    print_cost_metrics ppf cost_metrics;
    print_depth ppf depth;
    print_unrolling_depth ppf unrolling_depth;
    Format.fprintf ppf "@]"
end

module Decision_with_context = struct
  type decision =
    | Call of Call_site_inlining_decision_type.t
    | Fundecl of Function_decl_inlining_decision_type.t

  type t =
    { context : Context.t;
      decision : decision
    }

  let print_decision ppf = function
    | Call c -> Call_site_inlining_decision_type.report ppf c
    | Fundecl c -> Function_decl_inlining_decision_type.report ppf c

  let print ppf { context; decision } =
    Format.fprintf ppf "@[<v>@[<h>%a@]@.@[<h>%a@]@.@]" Context.print context
      print_decision decision
end

type raw_decision =
  { path : Inlining_history.Absolute.t;
    dbg : Debuginfo.t;
    decision_with_context : Decision_with_context.t option;
    pass : pass
  }

(* Record all raw decisions inside a list. Decisions are appended as they are
   recorded. The ordering in which they are recorded doesn't matter. *)
let log : raw_decision list ref = ref []

let record_decision_at_call_site_for_known_function ~tracker ~unrolling_depth
    ~apply ~pass ~callee ~are_rebuilding_terms decision =
  if Flambda_features.inlining_report ()
     || Flambda_features.inlining_report_bin ()
  then
    let dbg = Apply_expr.dbg apply in
    let path =
      Inlining_history.Tracker.call ~dbg ~callee
        ~relative:(Apply_expr.relative_history apply)
        tracker
    in
    let state = Apply_expr.inlining_state apply in
    let context =
      Context.create
        ~depth:(Inlining_state.depth state)
        ~args:(Inlining_state.arguments state)
        ~unrolling_depth ~are_rebuilding_terms ()
    in
    log
      := { pass;
           decision_with_context = Some { decision = Call decision; context };
           path;
           dbg
         }
         :: !log
  else ()

let record_decision_at_call_site_for_unknown_function ~tracker ~apply ~pass () =
  if Flambda_features.inlining_report ()
     || Flambda_features.inlining_report_bin ()
  then
    let dbg = Apply_expr.dbg apply in
    let path =
      Inlining_history.Tracker.unknown_call ~dbg
        ~relative:(Apply_expr.relative_history apply)
        tracker
    in
    log := { pass; decision_with_context = None; path; dbg } :: !log
  else ()

let record_decision_at_function_definition ~absolute_history ~code_metadata
    ~pass ~are_rebuilding_terms decision =
  if Flambda_features.inlining_report ()
     || Flambda_features.inlining_report_bin ()
  then
    let dbg = Code_metadata.dbg code_metadata in
    let args = Code_metadata.inlining_arguments code_metadata in
    let cost_metrics = Code_metadata.cost_metrics code_metadata in
    let context = Context.create ~args ~cost_metrics ~are_rebuilding_terms () in
    log
      := { pass;
           decision_with_context = Some { decision = Fundecl decision; context };
           path = absolute_history;
           dbg
         }
         :: !log
  else ()

module Inlining_tree = struct
  module Key = struct
    type scope_type =
      | Module
      | Class
      | Unknown

    let compare_scope_type a b =
      match a, b with
      | Module, Module | Class, Class | Unknown, Unknown -> 0
      | Module, (Class | Unknown) | Class, Unknown -> 1
      | (Class | Unknown), Module | Unknown, Class -> -1

    type element =
      | Fundecl of string
      | Scope of scope_type * string
      | Call of Inlining_history.Absolute.t

    type t = Debuginfo.t * element

    let compare_element a b =
      match a, b with
      | Fundecl s1, Fundecl s2 -> String.compare s1 s2
      | Call s1, Call s2 -> Inlining_history.Absolute.compare s1 s2
      | Scope (t1, s1), Scope (t2, s2) ->
        let c = compare_scope_type t1 t2 in
        if c <> 0 then c else String.compare s1 s2
      | Fundecl _, (Call _ | Scope _) | Call _, Scope _ -> 1
      | (Call _ | Scope _), Fundecl _ | Scope _, Call _ -> -1

    let compare (dbg1, e1) (dbg2, e2) =
      let c = Debuginfo.compare dbg1 dbg2 in
      if c <> 0 then c else compare_element e1 e2
  end

  module Map = Map.Make (Key)

  type item =
    | Fundecl of fundecl
    | Call of call
    | Scope of t

  and t = item Map.t

  and fundecl_passes =
    { after_closure : Decision_with_context.t option;
      before_simplify : Decision_with_context.t option;
      after_simplify : Decision_with_context.t option
    }

  and fundecl =
    { passes : fundecl_passes;
      body : t
    }

  and call =
    { decision_with_context : Decision_with_context.t option;
      inlined : t
    }

  let empty = Map.empty

  let empty_fundecl_passes =
    { after_closure = None; before_simplify = None; after_simplify = None }

  let insert_or_update_scope ~scope_type ~name ~cont t =
    let key = Debuginfo.none, Key.Scope (scope_type, name) in
    let m =
      if not (Map.mem key t)
      then Map.empty
      else
        match Map.find key t with
        | Scope m -> m
        | Call _ | Fundecl _ -> assert false
    in
    Map.add key (Scope (cont m)) t

  let join_decision ~error:_ old next =
    match old, next with
    | Some _old, Some next ->
      (* CR poechsel: This case should be invalid (hitting it would mean that we
         took several decisions for the same exact path, which either indicates
         that the inlining history is buggy or that we are simplifying the same
         exact term several times in a row) but it seems that it can be
         currently reached. I should investigate it a bit further.

         Misc.fatal_errorf error *)
      Some next
    | None, next -> next
    | old, None -> old

  let join_fundecl_passes (a : fundecl_passes) (b : fundecl_passes) =
    let after_closure =
      join_decision a.after_closure b.after_closure
        ~error:
          "Each function can have at most one associated decision for the \
           after closure pass."
    in
    let before_simplify =
      join_decision a.before_simplify b.before_simplify
        ~error:
          "Each function can have at most one associated decision for the \
           before simplify pass."
    in
    let after_simplify =
      join_decision a.after_simplify b.after_simplify
        ~error:
          "Each function can have at most one associated decision for the \
           after simplify pass."
    in
    { after_closure; before_simplify; after_simplify }

  let insert_or_update_call ?decision_with_context ~dbg ~callee
      ~(cont : item Map.t -> item Map.t) t =
    let key = dbg, Key.Call callee in
    Map.update key
      (function
        | None ->
          Some (Call { inlined = cont Map.empty; decision_with_context })
        | Some (Call t) ->
          let decision_with_context =
            join_decision t.decision_with_context decision_with_context
              ~error:"Each call can have at most one associated decision"
          in

          Some (Call { inlined = cont t.inlined; decision_with_context })
        | Some (Fundecl _ | Scope _) ->
          Misc.fatal_errorf "%s"
            "A key of type call should be associated with an item of type call")
      t

  let insert_or_update_fundecl ~passes ~dbg ~name
      ~(cont : item Map.t -> item Map.t) t =
    let key = dbg, Key.Fundecl name in
    Map.update key
      (function
        | None -> Some (Fundecl { body = cont Map.empty; passes })
        | Some (Fundecl t) ->
          let passes = join_fundecl_passes t.passes passes in
          Some (Fundecl { body = cont t.body; passes })
        | Some (Call _ | Scope _) ->
          Misc.fatal_errorf
            "A key of type call should be associated with an item of type call")
      t

  (* Insert a decision into an inlining tree [t].

     Only decisions rooted in [compilation_unit] are kept. *)
  let insert ~compilation_unit t (decision : raw_decision) =
    let rec aux ~cont path =
      match (path : Inlining_history.Absolute.path) with
      | Empty -> cont t
      | Unknown { prev } ->
        aux prev ~cont:(fun m ->
            insert_or_update_scope ~scope_type:Unknown ~name:"" ~cont m)
      | Module { name; prev } ->
        aux prev ~cont:(fun m ->
            insert_or_update_scope ~scope_type:Module ~name ~cont m)
      | Class { name; prev } ->
        aux prev ~cont:(fun m ->
            insert_or_update_scope ~scope_type:Class ~name ~cont m)
      | Function { dbg; name; prev } ->
        aux prev ~cont:(fun m ->
            insert_or_update_fundecl ~passes:empty_fundecl_passes ~dbg ~name
              ~cont m)
      | Call { dbg; callee; prev } ->
        aux prev ~cont:(fun m -> insert_or_update_call ~dbg ~callee ~cont m)
      | Inline { prev } -> aux prev ~cont
    in

    let { path; dbg; pass; decision_with_context } = decision in
    if Compilation_unit.equal compilation_unit
         (Inlining_history.Absolute.compilation_unit path)
    then
      match Inlining_history.Absolute.path path with
      | Unknown _ -> t
      | Call { callee; prev; _ } ->
        aux
          ~cont:(fun m ->
            insert_or_update_call ?decision_with_context ~dbg ~callee
              ~cont:(fun x -> x)
              m)
          prev
      | Function { name; prev; _ } ->
        let passes =
          match pass with
          | After_closure_conversion ->
            { empty_fundecl_passes with after_closure = decision_with_context }
          | Before_simplify ->
            { empty_fundecl_passes with
              before_simplify = decision_with_context
            }
          | After_simplify ->
            { empty_fundecl_passes with after_simplify = decision_with_context }
        in
        aux
          ~cont:(insert_or_update_fundecl ~passes ~dbg ~name ~cont:(fun x -> x))
          prev
      | Module _ | Class _ | Inline _ | Empty ->
        Misc.fatal_errorf
          "Only decisions for closures and calls can be recorded."
    else t

  let stars ppf n = Format.fprintf ppf "%s" (String.make n '*')

  let print_decision ~callee ~compilation_unit ppf = function
    | None -> begin
      match callee with
      | None -> Format.fprintf ppf "Unknown"
      | Some def ->
        let defined_in = Inlining_history.Absolute.compilation_unit def in
        if Compilation_unit.equal defined_in compilation_unit
        then Format.fprintf ppf "Unknown"
        else
          Format.fprintf ppf
            "@[<h>This@ decision@ was@ taken@ while@ compiling@ %s.@]"
            (Compilation_unit.string_for_printing defined_in)
    end
    | Some decision -> Decision_with_context.print ppf decision

  let print_fundecl_decision = print_decision ~callee:None

  let print_call_decision ~callee = print_decision ~callee:(Some callee)

  let print_title ?(dbg = Debuginfo.none) ~depth ~label ~f ppf w =
    let print_dbg ppf dbg =
      if Debuginfo.is_none dbg
      then ()
      else Format.fprintf ppf "@ at@ %a" Debuginfo.print_compact dbg
    in
    Format.fprintf ppf "@[<h>%a@ %s@ %a%a@]@." stars depth label f w print_dbg
      dbg

  let rec print ~compilation_unit ~depth ppf t =
    Map.iter
      (fun (dbg, key) (v : item) ->
        match key, v with
        | Scope (Unknown, _), _ ->
          print_title ~depth ~label:"Unknown" ~f:Format.pp_print_text ppf "";
          print ~compilation_unit ppf ~depth:(depth + 1) t
        | Scope (Module, name), Scope t ->
          print_title ~depth ~label:"Module" ~f:Format.pp_print_text ppf name;
          print ~compilation_unit ppf ~depth:(depth + 1) t
        | Scope (Class, name), Scope t ->
          print_title ~depth ~label:"Class" ~f:Format.pp_print_text ppf name;
          print ~compilation_unit ppf ~depth:(depth + 1) t
        | Call callee, Call { decision_with_context; inlined } ->
          Format.fprintf ppf "%a@.@[<v>%a@]@.@.%a"
            (print_title ~dbg ~depth ~label:"Application of"
               ~f:Inlining_history.Absolute.print)
            (Inlining_history.Absolute.shorten_to_definition callee)
            (print_call_decision ~callee ~compilation_unit)
            decision_with_context
            (print ~compilation_unit ~depth:(depth + 1))
            inlined
        | ( Fundecl fundecl,
            Fundecl
              { passes = { after_closure; before_simplify; after_simplify };
                body
              } ) ->
          Format.fprintf ppf
            "@[<v>%a@. @[<h>%a@ After@ closure@ conversion:@]@.@[<v>%a@]@.@. \
             @[<h>%a@ Before@ simplify:@]@.@[<v>%a@] @.@.%a@.@. @[<h>%a@ \
             After@ simplify:@]@.@[<v>%a@]@.@]"
            (print_title ~dbg ~depth ~label:"Definition of"
               ~f:Format.pp_print_text)
            fundecl stars (depth + 1)
            (print_fundecl_decision ~compilation_unit)
            after_closure stars (depth + 1)
            (print_fundecl_decision ~compilation_unit)
            before_simplify
            (print ~compilation_unit ~depth:(depth + 1))
            body stars (depth + 1)
            (print_fundecl_decision ~compilation_unit)
            after_simplify
        | Scope _, (Fundecl _ | Call _)
        | Call _, (Fundecl _ | Scope _)
        | Fundecl _, (Call _ | Scope _) ->
          assert false)
      t

  let print ~compilation_unit ppf t = print ~compilation_unit ~depth:0 ppf t
end

type metadata = { compilation_unit : Compilation_unit.t }

type report = [`Flambda2_1_0_0 of metadata * Inlining_tree.t]

let output_then_forget_decisions ~output_prefix =
  Misc.try_finally
    ~always:(fun () -> log := [])
    ~exceptionally:(fun () ->
      (* CR gbury: Is there a more appropriate function to report a warning
         (that is not one of the numbered warnings)? *)
      Format.eprintf "WARNING: inlining report output failed@.")
    (fun () ->
      let compilation_unit = Compilation_unit.get_current_exn () in
      let tree =
        lazy
          (List.fold_left
             (Inlining_tree.insert ~compilation_unit)
             Inlining_tree.empty !log)
      in
      if Flambda_features.inlining_report ()
      then (
        let filename = output_prefix ^ ".inlining.org" in
        let out_channel = open_out filename in
        let fmt = Format.formatter_of_out_channel out_channel in
        Format.fprintf fmt "@[<v>%a@]"
          (Inlining_tree.print ~compilation_unit)
          (Lazy.force tree);
        close_out out_channel);
      if Flambda_features.inlining_report_bin ()
      then begin
        let ch = open_out_bin (output_prefix ^ ".inlining") in
        let metadata = { compilation_unit } in
        let report : report = `Flambda2_1_0_0 (metadata, Lazy.force tree) in
        Marshal.to_channel ch report [];
        close_out ch
      end)
