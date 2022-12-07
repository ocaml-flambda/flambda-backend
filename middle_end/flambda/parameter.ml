(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+9"]
(* Warning 9 is enabled to ensure correct update of each function when
   a field is added to type parameter *)

type parameter = {
  var : Variable.t;
  mode : Lambda.alloc_mode;
  kind : Lambda.value_kind;
}

let wrap var mode kind = { var; mode; kind }

let var p = p.var
let alloc_mode p = p.mode
let kind p = p.kind

module M =
  Identifiable.Make (struct
    type t = parameter

    let compare
        { var = var1; mode = _ ; kind = _ }
        { var = var2; mode = _ ; kind = _ } =
      Variable.compare var1 var2

    let equal
        { var = var1; mode = _ ; kind = _ }
        { var = var2; mode = _ ; kind = _ } =
      Variable.equal var1 var2

    let hash { var; mode = _ ; kind = _ } =
      Variable.hash var

    let print ppf { var; mode ; kind } =
      let mode = match mode with
        | Lambda.Alloc_heap -> ""
        | Lambda.Alloc_local -> "[->L]" in
      Format.fprintf ppf "%a%s[%a]"
        Variable.print var mode Printlambda.value_kind kind

    let output o { var; mode = _ ; kind = _ } =
      Variable.output o var
  end)

module T = M.T
include T

module Map = M.Map
module Tbl = M.Tbl
module Set = struct
  include M.Set
  let vars l = Variable.Set.of_list (List.map var l)
end

let rename ?current_compilation_unit p =
  { p with var = Variable.rename ?current_compilation_unit p.var }

let map_var f { var ; mode ; kind } = { var = f var; mode; kind }

module List = struct
  let vars params = List.map (fun { var ; mode = _ ; kind = _ } -> var) params
end
