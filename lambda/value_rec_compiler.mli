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

(** Overview of the compilation scheme

    Given a set of recursive definitions, we sort them into four categories:

    - Non-recursive definitions (more precisely, definitions classified as
      [Dynamic] by [Value_rec_check]). These are compiled as regular
      let-bindings, inserted before the rest.
    - Constant definitions. These are definitions that have been classified
      as [Static], but cannot be pre-allocated and cannot end up actually using
      recursive values (i.e. let rec x = let _ = x in 0). These are compiled to
      regular bindings too, modulo some renaming to remove references to
      recursive variables.
    - Blocks of static size. These are pre-allocated, new values are computed
      from the definitions, and the contents of the new values is copied back
      to the original allocation.
    - Functional values. These are defined together in a single recursive block
      full of functions. The definition occurs after the pre-allocation
      (so the functions can refer to the recursive blocks) but before the
      back-patching (so the block definitions can refer to the functions).

    More detailed comments are available in the implementation.
*)

val compile_letrec :
  (Ident.t * Value_rec_types.recursive_binding_kind * Lambda.lambda) list ->
  Lambda.lambda ->
  Lambda.lambda
