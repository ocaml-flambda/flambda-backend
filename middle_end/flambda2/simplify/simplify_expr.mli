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

(** Simplification proceeds in two passes: the downwards pass and the upwards
    pass.

    The downwards pass proceeds following the scoping of the terms. It collects
    usage information and exploits the Flambda 2 type system to identify
    simplifications to be performed. It also does inlining and unboxing
    calculations.

    The upwards pass rebuilds the simplified expressions based on information
    collected in the downwards pass. This includes placing let-symbol bindings
    for statically-allocated values.

    In general the whole of an expression will be traversed downwards before the
    upwards pass starts. There is one exception: the right-hand sides of
    [Let]-expressions binding [Set_of_closures] expressions are simplified fully
    (down and up) during the downwards pass of the surrounding expression. This
    is so that the Flambda type system can have the necessary simplified
    function bodies available, enabling inlining of those bodies later on. (In
    effect we have made the decision to prioritise availability of simplified
    function bodies over usage information of functions. Having the latter would
    permit a more extensive data flow analysis whose results could be used
    before function bodies were rebuilt.)

    Data flow equations are solved in the transition from the downwards to the
    upwards pass.

    Also see comments in [Simplify_common]. *)

(** Simplify an expression in a given context. *)
val simplify_expr : Flambda.Expr.t Simplify_common.expr_simplifier

(** Simplify a whole program body or a whole function body. This deals with the
    setting up of the [Data_flow] module and the return/exception
    continuations. *)
val simplify_toplevel : Simplify_common.simplify_toplevel
