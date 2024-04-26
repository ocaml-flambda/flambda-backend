(******************************************************************************
 *                             flambda-backend                                *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* In classic mode and when running Simplify on classic-mode-generated code, we
   may encounter multiple [Enter_inlined_apply] primitives, before reaching any
   particular [Debuginfo.t]. As such we need to keep a stack of these so we
   don't lose information. *)
module One_step = struct
  type t =
    { dbg : Debuginfo.t;
      function_symbol : Linkage_name.t;
      uid : string
    }

  (* CR mshinwell/poechsel: if this becomes the flambda2 replacement for
     [Debuginfo.t] we could maybe avoid making the uids in the same way, and add
     a "below" constructor or something to identify different instances of
     inlining. *)

  (* CR mshinwell/poechsel: maybe this could be integrated with the inlining
     histories *)

  let print_debuginfo ppf dbg =
    if Debuginfo.is_none dbg
    then Format.pp_print_string ppf "None"
    else Debuginfo.print_compact ppf dbg

  let print ppf { dbg; function_symbol; uid } =
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(dbg@ %a)@]@ @[<hov 1>(function_symbol@ %a)@]@ \
       @[<hov 1>(uid@ %a)@])@]"
      print_debuginfo dbg Linkage_name.print function_symbol
      Format.pp_print_string uid

  let compare t1 t2 =
    let c = Debuginfo.compare t1.dbg t2.dbg in
    if c <> 0
    then c
    else
      let c = Linkage_name.compare t1.function_symbol t2.function_symbol in
      if c <> 0 then c else String.compare t1.uid t2.uid

  let rewrite t dbg_from_inlined_body =
    if Debuginfo.is_none t.dbg
    then dbg_from_inlined_body
    else
      let dbg_from_inlined_body =
        (* uids of _all_ [Debuginfo.t] values in the body of the function being
           inlined get freshened (consistently, for any given inlining).

           See the doc comment in the .mli for further details. *)
        Debuginfo.mapi_items dbg_from_inlined_body
          ~f:(fun n ({ dinfo_function_symbol; _ } as item : Debuginfo.item) ->
            let dinfo_function_symbol =
              if n = 0
              then Some (Linkage_name.to_string t.function_symbol)
              else dinfo_function_symbol
            in
            Debuginfo.item_with_uid_and_function_symbol item
              ~dinfo_uid:(Some t.uid) ~dinfo_function_symbol)
      in
      (* Note that [t.dbg] might not be a singleton -- however everything should
         be ok in terms of each frame (except the first) being annotated with a
         uid and function symbol in [t.dbg]. *)
      Debuginfo.inline t.dbg dbg_from_inlined_body
end

type t = One_step.t list

let none = []

let is_none t = match t with [] -> true | _ :: _ -> false

let print ppf t =
  Format.fprintf ppf "@[<hov 1>%a@]" (Format.pp_print_list One_step.print) t

let inlining_counter = ref 0

let create ~called_code_id ~apply_dbg =
  let function_symbol = Code_id.linkage_name called_code_id in
  let uid =
    incr inlining_counter;
    (* CR mshinwell: consider improving this *)
    Hashtbl.hash (apply_dbg, function_symbol, !inlining_counter)
    |> string_of_int
  in
  [{ One_step.dbg = apply_dbg; function_symbol; uid }]

let merge t ~from_apply_expr = from_apply_expr @ t

let compare t1 t2 = List.compare One_step.compare t1 t2

let rewrite t dbg =
  (* This could be optimized in terms of freshening uids, but for the moment use
     a more obviously-correct implementation. *)
  List.fold_left (fun dbg one_step -> One_step.rewrite one_step dbg) dbg t
