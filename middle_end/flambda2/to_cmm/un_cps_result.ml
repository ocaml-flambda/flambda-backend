(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = struct
  include Cmm_helpers
  include Un_cps_helper
end

type t = {
  gc_roots : Symbol.t list;
  data_list : Cmm.phrase list;
  functions : Cmm.fundecl list;
  current_data : Cmm.data_item list;
}

let empty = {
  gc_roots = [];
  data_list = [];
  functions = [];
  current_data = [];
}

let defines_a_symbol data =
  match (data : Cmm.data_item) with
  | Cdefine_symbol _
  | Cglobal_symbol _ -> true
  | _ -> false

let add_to_data_list x l =
  match x with
  | [] -> l
  | _ :: _ ->
    if not (List.exists defines_a_symbol x) then
      Misc.fatal_errorf "data list does not define any symbol, \
                         its elements will be unusable: %a"
        Printcmm.data x;
    C.cdata x :: l

let archive_data r =
  { r with current_data = [];
           data_list = add_to_data_list r.current_data r.data_list; }

let update_data r f =
  { r with current_data = f r.current_data; }

let set_data r l =
  update_data r (function
      | [] -> l
      | _ ->
        Misc.fatal_errorf
          "Un_cps_result.set_data: %s"
          "about to lose some translated static data items"
    )

let add_gc_roots r l =
  { r with gc_roots = l @ r.gc_roots; }

let add_function r f =
  { r with functions = f :: r.functions; }

let to_cmm r =
  (* Make sure we do not forget any current data *)
  let r = archive_data r in
  (* Sort functions according to debuginfo *)
  let sorted_functions =
    List.sort (fun (f1: Cmm.fundecl) (f2: Cmm.fundecl) ->
      Debuginfo.compare f1.fun_dbg f2.fun_dbg
    ) r.functions
  in
  let functions_phrases =
    List.map (fun f -> Cmm.Cfunction f) sorted_functions
  in
  (* Return the data list, gc roots and function declarations *)
  r.data_list, r.gc_roots, functions_phrases

