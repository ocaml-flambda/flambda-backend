(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module P = Flambda_primitive

type comparison_kind =
  | Int of { kind : Flambda_kind.Standard_int.t; sign : P.signed_or_unsigned }
  | Float

type t =
{ lhs : Simple.t;
  rhs : Simple.t;
  comp : comparison_kind;
}

let create ~(prim : P.t) : t option =
  match[@warning "-fragile-match"] prim with
  | Binary (Int_comp (kind, Yielding_int_like_compare_functions sign),
     lhs, rhs)->
    Some { lhs; rhs; comp = Int { kind; sign } }
  | Binary (Float_comp (Yielding_int_like_compare_functions ()),
     lhs, rhs)->
    Some { lhs; rhs; comp = Float }
  | _ -> None

let print_kind ppf ckind =
  match ckind with
  | Float -> Format.fprintf ppf "float"
  | Int { kind; sign } ->
    let prefix = match sign with Signed -> "" | Unsigned -> "u" in
    Format.fprintf ppf "%s%a"
      prefix
      Flambda_kind.Standard_int.print_lowercase kind

let [@ocamlformat "disable"] print ppf { lhs; rhs; comp } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(lhs@ %a)@]@ \
      @[<hov 1>(rhs@ %a)@]@ \
      @[<hov 1>(kind@ %a)@]@ \
      )@]"
    Simple.print lhs
    Simple.print rhs
    print_kind comp

let convert_result_compared_to_zero { lhs; rhs; comp } (op : _ P.comparison) : P.t =
  let fix_sign new_val : _ P.comparison =
    match op with
    | Eq -> Eq
    | Neq -> Neq
    | Lt _ -> Lt new_val
    | Gt _ -> Gt new_val
    | Le _ -> Le new_val
    | Ge _ -> Ge new_val
  in
  let prim : P.binary_primitive=
    match comp with
    | Float -> Float_comp (Yielding_bool (fix_sign ()))
    | Int { kind; sign } -> Int_comp (kind, Yielding_bool (fix_sign sign))
  in
  Binary (prim, lhs, rhs)
