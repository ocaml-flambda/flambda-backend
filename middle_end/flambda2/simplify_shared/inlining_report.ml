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

module IHA = Inlining_history.Absolute

module Pass = struct
  type t =
    | After_closure_conversion
    | Before_simplify
    | After_simplify

  let print ppf pass =
    match pass with
    | After_closure_conversion ->
      Format.fprintf ppf "afte@r closure@ conversion"
    | Before_simplify -> Format.fprintf ppf "before@ simplify"
    | After_simplify -> Format.fprintf ppf "after@ simplify"
end

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
      are_rebuilding_terms : Are_rebuilding_terms.t;
      pass : Pass.t
    }

  let create ?depth ?unrolling_depth ?cost_metrics ~are_rebuilding_terms ~args
      ~pass () =
    { args; depth; unrolling_depth; cost_metrics; are_rebuilding_terms; pass }

  let print ppf
      { args;
        cost_metrics;
        depth;
        unrolling_depth;
        are_rebuilding_terms;
        pass = _
      } =
    let print_unrolling_depth ppf = function
      | None -> ()
      | Some (Some unroll) ->
        Format.fprintf ppf "@[<h>Unrolling@ depth:@ %d@]@,@," unroll
      | Some None -> Format.fprintf ppf "@[<h>Unrolling@ depth unknown@]@,@,"
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
      Format.fprintf ppf "@[<h>Inlining@ arguments:@;@]@,@[<h>%a@]@,@,"
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
          "@[<v>@[<h>Code@ size@ was@ estimated@ to@ be@ %a@]@,\
           @,\
           @[<h>Benefits@ of@ inlining@ this@ call:@;\
           @]@,\
           @[<h>%a@]@]@,\
           @,"
          Code_size.print (Cost_metrics.size c) Table.print table
    in
    let print_depth ppf = function
      | None -> ()
      | Some c ->
        Format.fprintf ppf "@[<h>Considered@ at@ inlining@ depth@ of@ %d@]@,@,"
          c
    in
    let print_are_rebuilding_terms ppf t =
      Format.fprintf ppf
        "@[<h>Considered@ with@ are_rebuilding_terms@ =@ %a@]@,@,"
        Are_rebuilding_terms.print t
    in
    print_are_rebuilding_terms ppf are_rebuilding_terms;
    print_args ppf args;
    print_cost_metrics ppf cost_metrics;
    print_depth ppf depth;
    print_unrolling_depth ppf unrolling_depth
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
    Format.fprintf ppf "@[<v>@[<h>Decision@ taken@ *%a*@]@," Pass.print
      context.pass;
    Format.fprintf ppf "@[<v 2>@,";
    Format.fprintf ppf "%a@,@[<h>%a@]@," Context.print context print_decision
      decision;
    Format.fprintf ppf "@]@]"
end

type raw_decision =
  { path : IHA.t;
    dbg : Debuginfo.t;
    decision_with_context : Decision_with_context.t option
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
        ~unrolling_depth ~are_rebuilding_terms ~pass ()
    in
    log
      := { decision_with_context = Some { decision = Call decision; context };
           path;
           dbg
         }
         :: !log
  else ()

let record_decision_at_call_site_for_unknown_function ~tracker ~apply ~pass:_ ()
    =
  if Flambda_features.inlining_report ()
     || Flambda_features.inlining_report_bin ()
  then
    let dbg = Apply_expr.dbg apply in
    let path =
      Inlining_history.Tracker.unknown_call ~dbg
        ~relative:(Apply_expr.relative_history apply)
        tracker
    in
    log := { decision_with_context = None; path; dbg } :: !log
  else ()

let record_decision_at_function_definition ~absolute_history ~code_metadata
    ~pass ~are_rebuilding_terms decision =
  if Flambda_features.inlining_report ()
     || Flambda_features.inlining_report_bin ()
  then
    let dbg = Code_metadata.dbg code_metadata in
    let args = Code_metadata.inlining_arguments code_metadata in
    let cost_metrics = Code_metadata.cost_metrics code_metadata in
    let context =
      Context.create ~args ~cost_metrics ~are_rebuilding_terms ~pass ()
    in
    log
      := { decision_with_context = Some { decision = Fundecl decision; context };
           path = absolute_history;
           dbg
         }
         :: !log
  else ()

module Uid = struct
  type t =
    { compilation_unit : Compilation_unit.t;
      t : string
    }

  let create ~compilation_unit path =
    { compilation_unit; t = IHA.uid_path path }

  let print_anchor ppf { compilation_unit = _; t } =
    Format.fprintf ppf " <<%s>>" t

  let print_link_hum ~compilation_unit ppf { compilation_unit = cu_uid; t } =
    if Compilation_unit.equal compilation_unit cu_uid
    then Format.fprintf ppf "[[%s][here]]" t
    else
      let external_reports =
        Format.asprintf "%a.0.inlining.org" Compilation_unit.print_name cu_uid
      in
      try
        let file = Load_path.find_normalized external_reports in
        Format.fprintf ppf "[[file:%s::%s][in compilation unit %a]]" t file
          Compilation_unit.print cu_uid
      with Not_found ->
        Format.fprintf ppf
          "in@ compilation@ unit@ %a@ but@ no@ inlining@ reports@ were@ \
           generated@ for@ this@ unit."
          Compilation_unit.print cu_uid
end

module Inlining_tree = struct
  module Key = struct
    type scope =
      | Module
      | Class
      | Unknown

    let compare_scope a b =
      match a, b with
      | Module, Module | Class, Class | Unknown, Unknown -> 0
      | Module, (Class | Unknown) | Class, Unknown -> 1
      | (Class | Unknown), Module | Unknown, Class -> -1

    type element =
      | Fundecl of string
      | Scope of scope * string
      | Call of IHA.t

    let compare_element a b =
      match a, b with
      | Fundecl s1, Fundecl s2 -> String.compare s1 s2
      | Call s1, Call s2 -> IHA.compare s1 s2
      | Scope (t1, s1), Scope (t2, s2) ->
        let c = compare_scope t1 t2 in
        if c <> 0 then c else String.compare s1 s2
      | Fundecl _, (Call _ | Scope _) | Call _, Scope _ -> 1
      | (Call _ | Scope _), Fundecl _ | Scope _, Call _ -> -1

    type t = Debuginfo.t * element

    let compare (dbg1, e1) (dbg2, e2) =
      let c = Debuginfo.compare dbg1 dbg2 in
      if c <> 0 then c else compare_element e1 e2
  end

  module Map = Map.Make (Key)

  type decision_or_reference =
    | Decision of Decision_with_context.t
    | Reference of IHA.t
    | Unavailable

  let merge_decision_or_reference old new_ =
    match old, new_ with
    | Reference _, ((Decision _ | Unavailable) as d)
    | ((Decision _ | Unavailable) as d), Reference _ ->
      d
    | Unavailable, (Decision _ as d) | (Decision _ as d), Unavailable -> d
    | Reference _, Reference _
    | Decision _, Decision _
    | Unavailable, Unavailable ->
      new_

  type item =
    | Call of
        { decision : decision_or_reference;
          tree : t
        }
    | Fundecl of
        { decisions : decisions;
          body : t
        }
    | Scope of t

  and t = item Map.t

  and decisions = Decision_with_context.t list

  let empty = Map.empty

  let insert_or_update_scope ~scope_type ~name ~apply_to_child t =
    let key = Debuginfo.none, Key.Scope (scope_type, name) in
    let m =
      if not (Map.mem key t)
      then Map.empty
      else
        match Map.find key t with
        | Scope m -> m
        | Call _ | Fundecl _ -> assert false
    in
    Map.add key (Scope (apply_to_child m)) t

  let insert_or_update_call ~decision ~dbg ~callee ~(apply_to_child : t -> t) t
      =
    let key = dbg, Key.Call callee in
    Map.update key
      (function
        | None -> Some (Call { tree = apply_to_child Map.empty; decision })
        | Some (Call t) ->
          let decision = merge_decision_or_reference t.decision decision in
          Some (Call { tree = apply_to_child t.tree; decision })
        | Some (Scope _ | Fundecl _) ->
          Misc.fatal_errorf
            "A key of type call or fundecl should be associated with an item \
             of type Call")
      t

  let insert_or_update_fundecl ?decision_with_context ~dbg ~name
      ~(apply_to_child : t -> t) t =
    let key = dbg, Key.Fundecl name in
    Map.update key
      (function
        | None ->
          Some
            (Fundecl
               { body = apply_to_child Map.empty;
                 decisions = Option.to_list decision_with_context
               })
        | Some (Fundecl t) ->
          let decisions =
            match decision_with_context with
            | Some decision -> decision :: t.decisions
            | None -> t.decisions
          in
          Some (Fundecl { body = apply_to_child t.body; decisions })
        | Some (Scope _ | Call _) ->
          Misc.fatal_errorf
            "A key of type fundecl should be associated with an item of type \
             Fundecl")
      t

  (* Insert a decision into an inlining tree [t].

     Only decisions rooted in [compilation_unit] are kept. *)
  let insert ~compilation_unit t (decision : raw_decision) =
    (* Paths are stored as a linked list, meaning that the innermost atoms of
       the paths are also the one at the start of it. For example, the path:
       Empty -> fundecl(foo) -> apply(bar) -> inlined -> apply(baz) Is
       represented as Apply(baz, Inlined(Apply(bar, Fundecl(foo, Empty))))

       As such path can only traversed from top to bottom. However, [insert]
       will insert a decision in a inlining tree rooted in [t] and inside an
       inlining [tree] nodes are traversed from bottom to tree.

       To avoid having to manually reverse the path each call to
       [insert_or_update_descendant] constructs a [apply_to_child] function that
       will replay the traversal in reversed order and build the inlining tree
       [t] where nodes corresponding to [decision] are present. *)
    let rec insert_or_update_descendant ~apply_to_child path =
      match (path : IHA.path) with
      | Empty -> apply_to_child t
      | Unknown { prev } ->
        insert_or_update_descendant prev ~apply_to_child:(fun m ->
            insert_or_update_scope ~scope_type:Unknown ~name:"" ~apply_to_child
              m)
      | Module { name; prev } ->
        insert_or_update_descendant prev ~apply_to_child:(fun m ->
            insert_or_update_scope ~scope_type:Module ~name ~apply_to_child m)
      | Class { name; prev } ->
        insert_or_update_descendant prev ~apply_to_child:(fun m ->
            insert_or_update_scope ~scope_type:Class ~name ~apply_to_child m)
      | Function { dbg; name; prev } ->
        insert_or_update_descendant prev ~apply_to_child:(fun m ->
            insert_or_update_fundecl ~dbg ~name ~apply_to_child m)
      | Call { dbg; callee; prev } ->
        insert_or_update_descendant prev ~apply_to_child:(fun m ->
            insert_or_update_call ~decision:(Reference callee) ~dbg ~callee
              ~apply_to_child m)
      | Inline { prev } -> insert_or_update_descendant prev ~apply_to_child
    in
    let { path; dbg; decision_with_context } = decision in
    if Compilation_unit.equal compilation_unit (IHA.compilation_unit path)
    then
      let path = IHA.path path in
      match path with
      | Unknown _ -> t
      | Call { callee; prev; _ } ->
        insert_or_update_descendant
          ~apply_to_child:(fun m ->
            let decision =
              match decision_with_context with
              | Some decision -> Decision decision
              | None -> Unavailable
            in
            insert_or_update_call ~decision ~dbg ~callee
              ~apply_to_child:(fun x -> x)
              m)
          prev
      | Function { name; prev; _ } ->
        insert_or_update_descendant
          ~apply_to_child:
            (insert_or_update_fundecl ?decision_with_context ~dbg ~name
               ~apply_to_child:(fun x -> x))
          prev
      | Module _ | Class _ | Inline _ | Empty ->
        Misc.fatal_errorf
          "Only decisions for closures and calls can be recorded."
    else t

  let stars ppf n = Format.fprintf ppf "%s" (String.make n '*')

  let print_title ?uid ?(dbg = Debuginfo.none) ~depth ~label ~f ppf w =
    let print_uid ppf uid =
      match uid with None -> () | Some uid -> Uid.print_anchor ppf uid
    in
    let print_dbg ppf dbg =
      if Debuginfo.is_none dbg
      then ()
      else Format.fprintf ppf "@ at@ %a" Debuginfo.print_compact dbg
    in
    Format.fprintf ppf "@[<h>%a@ %s@ %a%a%a@]@,@," stars depth label f w
      print_dbg dbg print_uid uid

  let print_decision_with_context ppf decision_with_context =
    Format.fprintf ppf "@[%a@]@," Decision_with_context.print
      decision_with_context

  let print_decisions ppf decisions =
    match decisions with
    | [] -> assert false
    | decisions ->
      let decisions = List.rev decisions in
      Format.fprintf ppf "@[<v>";
      List.iter (print_decision_with_context ppf) decisions;
      Format.fprintf ppf "@]"

  let rebuild_path ~path ~dbg (key : Key.element) =
    match key with
    | Scope (Unknown, _) -> IHA.Unknown { prev = path }
    | Scope (Module, name) -> IHA.Module { name; prev = path }
    | Scope (Class, name) -> IHA.Class { name; prev = path }
    | Call callee -> IHA.Call { callee; dbg; prev = path }
    | Fundecl fundecl -> IHA.Function { name = fundecl; dbg; prev = path }

  let print_reference ~compilation_unit ppf to_ =
    let cu ppf () =
      let defined_in = IHA.compilation_unit to_ in
      if Compilation_unit.equal defined_in compilation_unit
      then Format.fprintf ppf "this compilation unit"
      else Format.fprintf ppf "%a" Compilation_unit.print_name defined_in
    in
    Format.fprintf ppf
      "@[<hov>The@ decision@ to@ inline@ this@ call@ was@ taken@ in@ %a@ at@ \
       %a.@]"
      cu () IHA.print to_

  let print_unavailable ppf () =
    Format.fprintf ppf
      "@[<hov>Flambda2@ was@ unavailable@ to@ do@ a@ direct@ call@ to@ this@ \
       function.@]"

  let rec print ~compilation_unit ~depth ~path ppf t =
    Map.iter
      (fun (dbg, key) (v : item) ->
        let path = rebuild_path ~path ~dbg key in
        let uid = Uid.create ~compilation_unit path in
        match key, v with
        | Scope (Unknown, _), _ ->
          print_title ~uid ~depth ~label:"Unknown" ~f:Format.pp_print_text ppf
            ""
        | Scope (Module, name), Scope t ->
          print_title ~uid ~depth ~label:"Module" ~f:Format.pp_print_text ppf
            name;
          print ~compilation_unit ppf ~depth:(depth + 1) ~path t
        | Scope (Class, name), Scope t ->
          print_title ~uid ~depth ~label:"Class" ~f:Format.pp_print_text ppf
            name;
          print ~compilation_unit ppf ~depth:(depth + 1) ~path t
        | Call callee, Call { decision; tree } ->
          print_title ppf ?uid:None ~dbg ~depth ~label:"Application of"
            ~f:IHA.print callee;
          Format.fprintf ppf "@[<v>";
          Format.fprintf ppf "@[<hov>Defined@ %a@]@,@,"
            (Uid.print_link_hum ~compilation_unit)
            (Uid.create ~compilation_unit (IHA.path callee));
          (match decision with
          | Decision decision_with_context ->
            Format.fprintf ppf "@[<v>%a@]" print_decision_with_context
              decision_with_context
          | Reference path -> print_reference ~compilation_unit ppf path
          | Unavailable -> print_unavailable ppf ());
          Format.fprintf ppf "@]@,@,";
          print ppf ~compilation_unit ~depth:(depth + 1)
            ~path:(IHA.Inline { prev = path })
            tree
        | Fundecl fundecl, Fundecl { decisions; body } ->
          print_title ppf ~uid ~dbg ~depth ~label:"Definition of"
            ~f:Format.pp_print_text fundecl;
          Format.fprintf ppf "@[<v>%a@]@,@," print_decisions decisions;
          print ppf ~compilation_unit ~depth:(depth + 1) ~path body
        | Scope _, (Call _ | Fundecl _)
        | Call _, Scope _
        | Fundecl _, Scope _
        | Fundecl _, Call _
        | Call _, Fundecl _ ->
          assert false)
      t

  let print ~compilation_unit ppf t =
    print ~compilation_unit ~depth:0 ~path:IHA.Empty ppf t
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
             Inlining_tree.empty (List.rev !log))
      in
      if Flambda_features.inlining_report ()
      then (
        let filename = output_prefix ^ ".inlining.org" in
        let out_channel = open_out filename in
        let fmt = Format.formatter_of_out_channel out_channel in
        Format.fprintf fmt "@[<v>%a@]@."
          (Inlining_tree.print ~compilation_unit)
          (Lazy.force tree);
        close_out out_channel);
      if Flambda_features.inlining_report_bin ()
      then (
        let ch = open_out_bin (output_prefix ^ ".inlining") in
        let metadata = { compilation_unit } in
        let report : report = `Flambda2_1_0_0 (metadata, Lazy.force tree) in
        Marshal.to_channel ch report [];
        close_out ch);
      Lazy.force tree)
