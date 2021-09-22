(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Not_yet_decided
  | Never_inline_attribute
  | Function_body_too_large of Code_size.t
  | Stub
  | Attribute_inline
  | Small_function of
      { size : Code_size.t;
        small_function_size : Code_size.t
      }
  | Speculatively_inlinable of
      { size : Code_size.t;
        small_function_size : Code_size.t;
        large_function_size : Code_size.t
      }
  | Functor of { size : Code_size.t }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Not_yet_decided -> Format.fprintf ppf "Not_yet_decided"
  | Never_inline_attribute ->
    Format.fprintf ppf "Never_inline_attribute"
  | Function_body_too_large large_function_size ->
    Format.fprintf ppf
      "@[<hov 1>(Function_body_too_large@ %a)@]"
      Code_size.print large_function_size
  | Stub ->
    Format.fprintf ppf "Stub"
  | Attribute_inline ->
    Format.fprintf ppf "Attribute_inline"
  | Small_function {size; small_function_size} ->
    Format.fprintf ppf
      "@[<hov 1>(Small_function@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_function_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_function_size
  | Speculatively_inlinable {size;
                              small_function_size;
                              large_function_size} ->
    Format.fprintf ppf
      "@[<hov 1>(Speculatively_inlinable@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_function_size@ %a)@]@ \
        @[<hov 1>(large_function_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_function_size
      Code_size.print large_function_size
  | Functor { size } ->
    Format.fprintf ppf
      "@[<hov 1>(Functor@ \
        @[<hov 1>(size@ %a)@]\
        )@]"
      Code_size.print size

let report_decision ppf t =
  match t with
  | Not_yet_decided -> Format.fprintf ppf "no decision has yet been made"
  | Never_inline_attribute ->
    Format.fprintf ppf "%a" Format.pp_print_text
      "the function has an attribute preventing its inlining"
  | Function_body_too_large large_function_size ->
    Format.fprintf ppf
      "the@ function's@ body@ is@ too@ large,@ more@ specifically,@ it@ is@ \
       larger@ than@ the@ large@ function@ size:@ %a"
      Code_size.print large_function_size
  | Stub -> Format.fprintf ppf "the@ function@ is@ a@ stub"
  | Attribute_inline ->
    Format.fprintf ppf
      "the@ function@ has@ an@ attribute@ forcing@ its@ inlining"
  | Small_function { size; small_function_size } ->
    Format.fprintf ppf
      "the@ function's@ body@ is@ smaller@ than@ the@ threshold@ size@ for@ \
       small@ functions: size=%a <= large@ function@ size=%a"
      Code_size.print size Code_size.print small_function_size
  | Speculatively_inlinable { size; small_function_size; large_function_size }
    ->
    Format.fprintf ppf
      "the@ function's@ body@ is@ between@ the@ threshold@ size@ for@ small@ \
       functions and the@ threshold@ size@ for@ large@ functions: small@ \
       function@ size=%a < size=%a < large@ function@ size=%a"
      Code_size.print small_function_size Code_size.print size Code_size.print
      large_function_size
  | Functor _ ->
    Format.fprintf ppf
      "this@ function@ is@ a@ functor@ (so@ the@ large@ function@ threshold@ \
       was@ not@ applied)."

type inlining_behaviour =
  | Cannot_be_inlined
  | Must_be_inlined
  | Could_possibly_be_inlined

let behaviour t =
  match t with
  | Not_yet_decided | Never_inline_attribute | Function_body_too_large _ ->
    Cannot_be_inlined
  | Stub | Attribute_inline | Small_function _ -> Must_be_inlined
  | Functor _ | Speculatively_inlinable _ -> Could_possibly_be_inlined

let report fmt t =
  Format.fprintf fmt
    "@[<v>The function %s be inlined at its use-sites@ because @[<hov>%a@]@]"
    (match behaviour t with
    | Cannot_be_inlined -> "cannot"
    | Could_possibly_be_inlined -> "could"
    | Must_be_inlined -> "must")
    report_decision t
