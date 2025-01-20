(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type tail_expr =
  | Invalid of { message : string }
  | Apply_cont of Apply_cont_expr.t
  | Switch of Switch_expr.t
  | Apply of Apply_expr.t

type rev_expr_holed =
  | Hole
  | Let of
      { bound_pattern : Bound_pattern.t;
        defining_expr : rev_named;
        parent : rev_expr_holed
      }
  | Let_cont of
      { cont : Continuation.t;
        handler : cont_handler;
        parent : rev_expr_holed
      }
  | Let_cont_rec of
      { invariant_params : Bound_parameters.t;
        handlers : cont_handler Continuation.Lmap.t;
        parent : rev_expr_holed
      }

and rev_named =
  | Named of Flambda.named
  | Set_of_closures of rev_set_of_closures
  | Static_consts of rev_static_const_or_code list

and rev_static_const_or_code =
  | Code of rev_code
  | Deleted_code
  | Static_const of rev_static_const

and rev_static_const =
  | Set_of_closures of rev_set_of_closures
  | Other of Static_const.t

and rev_code =
  { params_and_body : rev_params_and_body;
    free_names_of_params_and_body : Name_occurrences.t;
    code_metadata : Code_metadata.t
  }

and rev_params_and_body =
  { return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    params : Bound_parameters.t;
    body : rev_expr;
    my_closure : Variable.t;
    my_region : Variable.t option;
    my_ghost_region : Variable.t option;
    my_depth : Variable.t
  }

and rev_set_of_closures =
  { value_slots : Simple.t Value_slot.Map.t;
    function_decls : Function_declarations.t;
    alloc_mode : Alloc_mode.For_allocations.t
  }

and cont_handler =
  { bound_parameters : Bound_parameters.t;
    is_exn_handler : bool;
    is_cold : bool;
    expr : rev_expr
  }

and rev_expr =
  { expr : tail_expr;
    holed_expr : rev_expr_holed
  }

type t = rev_expr
