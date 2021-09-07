(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

type t =
  | Never_inline_attribute
  | Function_body_too_large of Code_size.t
  | Stub
  | Attribute_inline
  | Small_function of { size : Code_size.t; small_function_size : Code_size.t }
  | Speculatively_inlinable of
      { size : Code_size.t;
        small_function_size : Code_size.t;
        large_function_size : Code_size.t
      }
  | Functor of { size : Code_size.t }

type inlining_behaviour =
  | Cannot_be_inlined
  | Must_be_inlined
  | Could_possibly_be_inlined

let behaviour t =
  match t with
  | Never_inline_attribute | Function_body_too_large _ -> Cannot_be_inlined
  | Stub | Attribute_inline | Small_function _ -> Must_be_inlined
  | Functor _ | Speculatively_inlinable _ -> Could_possibly_be_inlined

let [@ocamlformat "disable"] print fmt = function
  | Never_inline_attribute ->
    Format.fprintf fmt "Never_inline_attribute"
  | Function_body_too_large large_function_size ->
    Format.fprintf fmt
      "@[<hov 1>(Function_body_too_large@ %a)@]"
      Code_size.print large_function_size
  | Stub ->
    Format.fprintf fmt "Stub"
  | Attribute_inline ->
    Format.fprintf fmt "Attribute_inline"
  | Small_function {size; small_function_size} ->
    Format.fprintf fmt
      "@[<hov 1>(Small_function@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_function_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_function_size
  | Speculatively_inlinable {size;
                              small_function_size;
                              large_function_size} ->
    Format.fprintf fmt
      "@[<hov 1>(Speculatively_inlinable@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_function_size@ %a)@]@ \
        @[<hov 1>(large_function_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_function_size
      Code_size.print large_function_size
  | Functor { size } ->
    Format.fprintf fmt
      "@[<hov 1>(Functor@ \
        @[<hov 1>(size@ %a)@]\
        )@]"
      Code_size.print size

let report_reason fmt = function
  | Never_inline_attribute ->
    Format.fprintf fmt "%a" Format.pp_print_text
      "the function has an attribute preventing its inlining"
  | Function_body_too_large large_function_size ->
    Format.fprintf fmt
      "the@ function's@ body@ is@ too@ large,@ more@ specifically,@ it@ is@ \
       larger@ than@ the@ large@ function@ size:@ %a"
      Code_size.print large_function_size
  | Stub -> Format.fprintf fmt "the@ function@ is@ a@ stub"
  | Attribute_inline ->
    Format.fprintf fmt
      "the@ function@ has@ an@ attribute@ forcing@ its@ inlining"
  | Small_function { size; small_function_size } ->
    Format.fprintf fmt
      "the@ function's@ body@ is@ smaller@ than@ the@ threshold@ size@ for@ \
       small@ functions: size=%a <= large@ function@ size=%a"
      Code_size.print size Code_size.print small_function_size
  | Speculatively_inlinable { size; small_function_size; large_function_size }
    ->
    Format.fprintf fmt
      "the@ function's@ body@ is@ between@ the@ threshold@ size@ for@ small@ \
       functions and the@ threshold@ size@ for@ large@ functions: small@ \
       function@ size=%a < size=%a < large@ function@ size=%a"
      Code_size.print small_function_size Code_size.print size Code_size.print
      large_function_size
  | Functor _ ->
    Format.fprintf fmt
      "this@ function@ is@ a@ functor@ (so@ the@ large@ function@ threshold@ \
       was@ not@ applied)."

let report fmt t =
  Format.fprintf fmt
    "@[<v>The function %s be inlined at its use-sites@ because @[<hov>%a@]@]"
    (match behaviour t with
    | Cannot_be_inlined -> "cannot"
    | Could_possibly_be_inlined -> "could"
    | Must_be_inlined -> "must")
    report_reason t

let make_decision code : t =
  (* At present, we follow Closure, taking inlining decisions without first
     examining call sites. *)
  let args = Code.inlining_arguments code in
  match Code.inline code with
  | Never_inline -> Never_inline_attribute
  | Hint_inline | Always_inline -> Attribute_inline
  | Default_inline | Unroll _ ->
    if Code.stub code
    then Stub
    else
      let metrics = Code.cost_metrics code in
      let large_function_size =
        Inlining_arguments.large_function_size args |> Code_size.of_int
      in
      let small_function_size =
        Inlining_arguments.small_function_size args |> Code_size.of_int
      in
      let size = Cost_metrics.size metrics in
      let is_small = Code_size.( <= ) size small_function_size in
      let is_large = Code_size.( <= ) large_function_size size in
      if Code.is_a_functor code
      then Functor { size }
      else if is_large
      then Function_body_too_large large_function_size
      else if is_small
      then
        Small_function { size = Cost_metrics.size metrics; small_function_size }
      else
        Speculatively_inlinable
          { size = Cost_metrics.size metrics;
            small_function_size;
            large_function_size
          }
