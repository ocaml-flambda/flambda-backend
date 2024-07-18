(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2023 OCamlPro SAS                                    *)
(*   Copyright 2016--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

type region_stack_element = Ident.t

val same_region : region_stack_element -> region_stack_element -> bool

val create :
  current_unit:Compilation_unit.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  my_region:Ident.t ->
  t

val current_unit : t -> Compilation_unit.t

val ident_stamp_upon_starting : t -> int

val is_mutable : t -> Ident.t -> bool

val register_mutable_variable :
  t -> Ident.t -> Flambda_kind.With_subkind.t -> t * Ident.t

val update_mutable_variable : t -> Ident.t -> t * Ident.t

val register_unboxed_product :
  t ->
  unboxed_product:Ident.t ->
  before_unarization:[`Complex] Flambda_arity.Component_for_creation.t ->
  fields:(Ident.t * Flambda_kind.With_subkind.t) list ->
  t

val get_unboxed_product_fields :
  t ->
  Ident.t ->
  ([`Complex] Flambda_arity.Component_for_creation.t * Ident.t list) option

type add_continuation_result = private
  { body_env : t;
    handler_env : t;
    extra_params : (Ident.t * Flambda_kind.With_subkind.t) list
  }

val add_continuation :
  t ->
  Continuation.t ->
  push_to_try_stack:bool ->
  pop_region:bool ->
  Asttypes.rec_flag ->
  add_continuation_result

val add_static_exn_continuation :
  t -> int -> pop_region:bool -> Continuation.t -> add_continuation_result

val get_static_exn_continuation : t -> int -> Continuation.t

val mark_as_recursive_static_catch : t -> int -> t

val is_static_exn_recursive : t -> int -> bool

val get_try_stack : t -> Continuation.t list

val get_try_stack_at_handler : t -> Continuation.t -> Continuation.t list

val extra_args_for_continuation : t -> Continuation.t -> Ident.t list

val extra_args_for_continuation_with_kinds :
  t -> Continuation.t -> (Ident.t * Flambda_kind.With_subkind.t) list

val get_mutable_variable_with_kind :
  t -> Ident.t -> Ident.t * Flambda_kind.With_subkind.t

(** About local allocation regions:

      In this pass, we have to transform [Lregion] expressions in Lambda to
      primitives that mark the opening and closing of stack regions. We need to
      ensure regions are always closed so as to not leak out of their scope.
      They must also never be closed twice.

      Several nested regions can be closed with one primitive as [End_region id]
      which will close [id] and every other region opened in its scope. As such,
      the transformation doesn't need to generate strict pairings of
      [Begin_region] and [End_region] in every case. We may jump out of the
      scope of several regions at once, in particular with exception raises from
      [Lstaticraise].

      Another case requiring attention is function calls in tail position for
      which we may need to add an [End_region] before the jump.

      This implementation works as follows.

      For normal control flow, following the block structure of Lambda
      expressions, we insert a new continuation (called the "region closure
      continuation") upon encountering [Begin_region]; then at every leaf we
      cause the control flow to jump via that continuation. The region closure
      continuation closes the relevant region before jumping to what would have
      been the "real" continuation of the leaf expressions in question. The
      insertion of the continuation avoids duplication of the [End_region]
      constructs. (We only need one [Begin_region] per region, but potentially
      as many [End_region]s as there are leaves in the subsequent term.)

      For exceptional control flow, the region closure continuation is not used;
      instead, a region is opened before the beginning of a Trywith, so that we
      can use this region to close every subsequent regions opened in its scope
      at the beginning of the handler.

      Likewise, when regions must be closed explicitly prior to tail calls to
      avoid leaking memory on the local allocation stack, the closure
      continuation is also not used in favour of explicit insertion of
      [End_region] operations.

      Region closure continuations are created alongside corresponding
      [Begin_region]s in the [Lregion] cases of [cps_non_tail] and [cps_tail].
      The decision as to calling a closure continuation or adding explicit
      [End_region]s is done in [restore_continuation_context] and
      [wrap_return_continuation]. Exceptional control flow cases are handled by
      the [compile_staticfail] and [Ltrywith] cases of the main transformation
      functions. *)

val entering_region :
  t ->
  Ident.t ->
  continuation_closing_region:Continuation.t ->
  continuation_after_closing_region:Continuation.t ->
  t

val leaving_region : t -> t

val current_region : t -> Ident.t

val parent_region : t -> Ident.t

val my_region : t -> Ident.t

(** The innermost (newest) region is first in the list. *)
val region_stack : t -> region_stack_element list

val region_stack_in_cont_scope :
  t -> Continuation.t -> region_stack_element list

val pop_one_region : t -> t * region_stack_element

(** Hack for staticfail (which should eventually use
      [pop_regions_up_to_context]) *)
val pop_region :
  region_stack_element list ->
  (region_stack_element * region_stack_element list) option

val pop_regions_up_to_context : t -> Continuation.t -> Ident.t option

type region_closure_continuation =
  { continuation_closing_region : Continuation.t;
    continuation_after_closing_region : Continuation.t
  }

val region_closure_continuation : t -> Ident.t -> region_closure_continuation
