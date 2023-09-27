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
  | Recursive

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
  | Recursive ->
    Format.fprintf ppf "Recursive"

type inlining_behaviour =
  | Cannot_be_inlined
  | Must_be_inlined
  | Could_possibly_be_inlined

let behaviour t =
  match t with
  | Not_yet_decided | Never_inline_attribute | Function_body_too_large _
  | Recursive ->
    Cannot_be_inlined
  | Stub | Attribute_inline | Small_function _ -> Must_be_inlined
  | Functor _ | Speculatively_inlinable _ -> Could_possibly_be_inlined

let must_be_inlined t =
  match behaviour t with
  | Must_be_inlined -> true
  | Cannot_be_inlined | Could_possibly_be_inlined -> false

let has_attribute_inline t =
  match t with
  | Attribute_inline -> true
  | Not_yet_decided | Never_inline_attribute | Function_body_too_large _ | Stub
  | Small_function _ | Speculatively_inlinable _ | Functor _ | Recursive ->
    false

let cannot_be_inlined t =
  match behaviour t with
  | Cannot_be_inlined -> true
  | Must_be_inlined | Could_possibly_be_inlined -> false

let equal t1 t2 =
  match t1, t2 with
  | Not_yet_decided, Not_yet_decided
  | Never_inline_attribute, Never_inline_attribute
  | Stub, Stub
  | Attribute_inline, Attribute_inline ->
    true
  | Function_body_too_large size1, Function_body_too_large size2 ->
    Code_size.equal size1 size2
  | ( Small_function { size = size1; small_function_size = small_function_size1 },
      Small_function
        { size = size2; small_function_size = small_function_size2 } ) ->
    Code_size.equal size1 size2
    && Code_size.equal small_function_size1 small_function_size2
  | ( Speculatively_inlinable
        { size = size1;
          small_function_size = small_function_size1;
          large_function_size = large_function_size1
        },
      Speculatively_inlinable
        { size = size2;
          small_function_size = small_function_size2;
          large_function_size = large_function_size2
        } ) ->
    Code_size.equal size1 size2
    && Code_size.equal small_function_size1 small_function_size2
    && Code_size.equal large_function_size1 large_function_size2
  | Functor { size = size1 }, Functor { size = size2 } ->
    Code_size.equal size1 size2
  | Recursive, Recursive -> true
  | ( ( Not_yet_decided | Never_inline_attribute | Function_body_too_large _
      | Stub | Attribute_inline | Small_function _ | Speculatively_inlinable _
      | Functor _ | Recursive ),
      _ ) ->
    false
