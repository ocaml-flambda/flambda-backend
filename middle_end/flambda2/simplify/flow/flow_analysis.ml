(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module T = Flow_types

let debug = Sys.getenv_opt "DF" <> None
let ref_to_var_debug = Sys.getenv_opt "RTV" <> None

let r = ref ~-1

let dominator_graph_ppf =
  lazy
    (match Sys.getenv_opt "DOM_GRAPH" with
    | None -> None
    | Some filename ->
      let ch = open_out filename in
      let ppf = Format.formatter_of_out_channel ch in
      Format.fprintf ppf "digraph g {@\n";
      at_exit (fun () ->
          Format.fprintf ppf "@\n}@.";
          close_out ch);
      Some ppf)

let control_flow_graph_ppf =
  lazy
    (match Sys.getenv_opt "FLOW_GRAPH" with
    | None -> None
    | Some filename ->
      let ch = open_out filename in
      let ppf = Format.formatter_of_out_channel ch in
      Format.fprintf ppf "digraph g {@\n";
      at_exit (fun () ->
          Format.fprintf ppf "@\n}@.";
          close_out ch);
      Some ppf)

let analyze ?(speculative=false) ?print_name ~return_continuation ~exn_continuation
    ~code_age_relation ~used_value_slots t : T.Flow_result.t =
  Profile.record_call ~accumulate:true "data_flow" (fun () ->
      if debug then Format.eprintf "PRESOURCE:@\n%a@\n@." T.Acc.print t;
      let ({ T.Acc.stack; map; extra = _; dummy_toplevel_cont } as t) =
        Flow_acc.extend_args_with_extra_args t
      in
      assert (stack = []);
      assert (
        not
          (Continuation.name dummy_toplevel_cont
          = Flow_acc.wrong_dummy_toplevel_cont_name));
      if debug then Format.eprintf "SOURCE:@\n%a@\n@." T.Acc.print t;
      (* Dead variable analysis *)
      let deps =
        Data_flow_graph.create map ~return_continuation ~exn_continuation
          ~code_age_relation ~used_value_slots
      in
      if debug
      then Format.eprintf "/// graph@\n%a@\n@." Data_flow_graph.print deps;
      let dead_variable_result = Data_flow_graph.required_names deps in
      (* Aliases analysis *)
      let dom_graph =
        Dominator_graph.create map ~return_continuation ~exn_continuation
          ~required_names:dead_variable_result.required_names
      in
      let aliases = Dominator_graph.dominator_analysis dom_graph in
      let aliases_kind = Dominator_graph.aliases_kind dom_graph aliases in
      (match print_name with
      | None -> ()
      | Some print_name ->
        Option.iter
          (fun ppf ->
            incr r;
            Dominator_graph.Dot.print ~print_name ~ctx:!r ~doms:aliases ppf
              dom_graph)
          (Lazy.force dominator_graph_ppf));
      let control = Control_flow_graph.create ~dummy_toplevel_cont t in
      let reference_analysis =
        Mutable_unboxing.create ~dom:aliases ~dom_graph ~source_info:t
          ~callers:control.callers ~return_continuation ~exn_continuation
      in
      let pp_node = Mutable_unboxing.pp_node reference_analysis in
      let continuation_parameters =
        Control_flow_graph.compute_continuation_extra_args_for_aliases
          ~speculative ~source_info:t aliases control
          ~required_names:dead_variable_result.required_names
      in
      (match print_name with
      | None -> ()
      | Some print_name ->
        Option.iter
          (fun ppf ->
            incr r;
            Control_flow_graph.Dot.print ~df:t ~print_name ~ctx:!r ppf
              ~return_continuation ~exn_continuation ~continuation_parameters
              ~pp_node control)
          (Lazy.force control_flow_graph_ppf));
      let reference_result =
        Mutable_unboxing.make_result reference_analysis
      in
      let required_names_after_ref_reference_analysis =
        (* CR pchambart/gbury: this is an overapproximation of actually used new
           parameters. We might want to filter this using another round of
           dead_analysis *)
        Continuation.Map.fold
          (fun _cont epa required_names ->
            let params = Bound_parameters.var_set (Continuation_extra_params_and_args.extra_params epa) in
            Name.Set.union required_names (Name.set_of_var_set params))
          reference_result.T.Mutable_unboxing_result.additionnal_epa dead_variable_result.required_names
      in
      (* Return *)
      let result =
        T.Flow_result.{ data_flow_result =
            { dead_variable_result with
              required_names = required_names_after_ref_reference_analysis
            };
          aliases_result = { aliases_kind; continuation_parameters };
          mutable_unboxing_result = reference_result;
        }
      in
      if (not
            (Named_rewrite_id.Map.is_empty result.mutable_unboxing_result.let_rewrites))
         && ref_to_var_debug
      then
        Format.printf "let_rewrites %a@."
          (Named_rewrite_id.Map.print Named_rewrite.print)
          result.mutable_unboxing_result.let_rewrites;
      if debug then Format.eprintf "/// result@\n%a@\n@." T.Flow_result.print result;
      result)
