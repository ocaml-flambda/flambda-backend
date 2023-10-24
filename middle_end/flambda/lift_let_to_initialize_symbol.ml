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
module Layouts = Flambda.Layouts

type ('a, 'b) kind =
  | Initialisation of (Symbol.t * Tag.t * Flambda.t list)
  | Effect of 'b

let should_copy (named:Flambda.named) =
  match named with
  | Symbol _ | Read_symbol_field _ | Const _ -> true
  | _ -> false

type access =
  | Field of int
  | Project_var of {
      closure_id : Closure_id.t;
      var : Var_within_closure.t;
      kind : Lambda.layout;
    }

type projection = access list

type extracted =
  | Expr of Variable.t * projection * Flambda.t
  | Exprs of (Variable.t * projection) list * Flambda.t
  | Block of Variable.t * Tag.t * Variable.t list

type accumulated = {
  copied_lets : (Variable.t * Flambda.named) list;
  extracted_lets : extracted list;
  terminator : Flambda.expr;
}

(* Values of layout not letrec cannot be stored in any kind of symbol bound
   values. Currently the only kind of values that can store any layout are
   the closures.

   To box a value, we create a dummy closure (with no code) and store the value
   as a free var. Unboxing is done with a projection of the free var.

   The Var_within_closure id is fresh.
*)
let boxing_closure var kind : Flambda.t * access =
  let inner_var = Variable.rename var in
  let closure_id_var =
    Variable.create Internal_variable_names.boxing_set_of_closures
      ~debug_info:Debuginfo.none
  in
  let closure_id = Closure_id.wrap closure_id_var in
  let closure_origin = Closure_origin.create closure_id in
  let function_decl =
    Flambda.create_function_declaration ~params:[]
      ~alloc_mode:Lambda.alloc_heap
      ~region:false
      ~stub:false
      ~return_layout:Lambda.layout_bottom
      ~specialise:Default_specialise
      ~check:Default_check
      ~is_a_functor:false
      ~poll:Default_poll
      ~inline:Default_inline
      ~closure_origin
      ~body:Proved_unreachable
  in
  let function_decls =
    Flambda.create_function_declarations ~is_classic_mode:true
      ~funs:(Variable.Map.singleton closure_id_var function_decl)
  in
  let free_var : Flambda.specialised_to =
    { var; projection = None; kind }
  in
  let set_of_closures =
    Flambda.create_set_of_closures ~function_decls
      ~specialised_args:Variable.Map.empty
      ~direct_call_surrogates:Variable.Map.empty
      ~free_vars:(Variable.Map.singleton inner_var free_var)
  in
  let project_var : access =
    Project_var { closure_id; var = Var_within_closure.wrap inner_var; kind }
  in
  let closure =
    let name = Internal_variable_names.boxing_closure in
    let set_var = Variable.create name in
    let closure_var = Variable.create name in
    Flambda.create_let set_var (Set_of_closures set_of_closures)
      (Flambda.create_let closure_var
        (Project_closure { set_of_closures = set_var; closure_id })
        (Var closure_var))
  in
  closure, project_var

let pack_expr ~layouts (expr : Flambda.t) =
  let layout = Flambda.result_layout ~layouts expr in
  match layout with
  | Ptop -> assert false
  | Pbottom
  | Pvalue _ -> expr, []
  | Punboxed_float
  | Punboxed_int _
  | Punboxed_vector _
  | Punboxed_product _ ->
    (* Unboxed float/int/vector could be boxed in simpler constructions.
       This can be changed when all the unboxed types and have been
       introduced *)
    let var = Variable.create Internal_variable_names.boxed_in_closure in
    let closure_var = Variable.create Internal_variable_names.boxing_closure in
    let closure, access = boxing_closure var layout in
    Flambda.create_let var (Expr expr)
     (Flambda.create_let closure_var (Expr closure) (Var closure_var)),
     [access]

let rec accumulate ~(layouts : Layouts.t) ~substitution ~copied_lets ~extracted_lets
      (expr : Flambda.t) =
  match expr with
  | Let { var; body = Var var'; _ } | Let_rec ([var, _], Var var')
    when Variable.equal var var' ->
    { copied_lets; extracted_lets;
      terminator = Flambda_utils.toplevel_substitution substitution expr;
    }
  (* If the pattern is what lifting let_rec generates, prevent it from being
     lifted again. *)
  | Let_rec (defs,
             Let { var; body = Var var';
                   defining_expr = Prim (Pmakeblock _, fields, _); })
    when
      Variable.equal var var'
      && List.for_all (fun field ->
          List.exists (fun (def_var, _) -> Variable.equal def_var field) defs)
      fields ->
    { copied_lets; extracted_lets;
      terminator = Flambda_utils.toplevel_substitution substitution expr;
    }
  | Let { var; defining_expr = Expr (Var alias); body; _ }
  | Let_rec ([var, Expr (Var alias)], body) ->
    let layouts =
      Layouts.add layouts var (Layouts.find layouts alias)
    in
    let alias =
      match Variable.Map.find alias substitution with
      | exception Not_found -> alias
      | original_alias -> original_alias
    in
    accumulate
      ~layouts
      ~substitution:(Variable.Map.add var alias substitution)
      ~copied_lets
      ~extracted_lets
      body
  | Let { var; defining_expr = named; body; _ }
  | Let_rec ([var, named], body)
    when should_copy named ->
      let layout = Flambda.result_layout_named ~layouts named in
      let layouts = Layouts.add layouts var layout in
      accumulate body
        ~layouts
        ~substitution
        ~copied_lets:((var, named)::copied_lets)
        ~extracted_lets
  | Let { var; defining_expr = named; body; _ } ->
    let layout = Flambda.result_layout_named ~layouts named in
    let layouts = Layouts.add layouts var layout in
    let extracted =
      let renamed = Variable.rename var in
      match named with
      | Prim (Pmakeblock (tag, (Immutable | Immutable_unique),
                          _value_kind, Alloc_heap),
              args, _dbg) ->
        let tag = Tag.create_exn tag in
        let args =
          List.map (fun v ->
              try Variable.Map.find v substitution
              with Not_found -> v)
            args
        in
        Block (var, tag, args)
      | named ->
        let expr =
          Flambda_utils.toplevel_substitution substitution
            (Flambda.create_let renamed named (Var renamed))
        in
        let expr, additionnal_path = pack_expr ~layouts expr in
        Expr (var, additionnal_path @ [Field 0], expr)
    in
    accumulate body
      ~layouts
      ~substitution
      ~copied_lets
      ~extracted_lets:(extracted::extracted_lets)
  | Let_rec ([var, named], body) ->
    let renamed = Variable.rename var in
    let def_substitution = Variable.Map.add var renamed substitution in
    let layout = Lambda.layout_letrec in
    let layouts = Layouts.add layouts var layout in
    let layouts = Layouts.add layouts renamed layout in
    let expr =
      Flambda_utils.toplevel_substitution def_substitution
        (Let_rec ([renamed, named], Var renamed))
    in
    let extracted = Expr (var, [Field 0], expr) in
    accumulate body
      ~layouts
      ~substitution
      ~copied_lets
      ~extracted_lets:(extracted::extracted_lets)
  | Let_rec (defs, body) ->
    let renamed_defs, def_substitution =
      List.fold_right (fun (var, def) (acc, substitution) ->
          let new_var = Variable.rename var in
          (new_var, def) :: acc,
          Variable.Map.add var new_var substitution)
        defs ([], substitution)
    in
    let layouts =
      List.fold_left (fun layouts (var, _) ->
          Layouts.add layouts var Lambda.layout_letrec)
        layouts defs
    in
    let layouts =
      List.fold_left (fun layouts (var, _) ->
          Layouts.add layouts var Lambda.layout_letrec)
        layouts renamed_defs
    in
    let extracted =
      let expr =
        let name = Internal_variable_names.lifted_let_rec_block in
        Flambda_utils.toplevel_substitution def_substitution
          (Let_rec (renamed_defs,
                    Flambda_utils.name_expr ~name
                      (Prim (Pmakeblock (0, Immutable, None, Lambda.alloc_heap),
                             List.map fst renamed_defs,
                             Debuginfo.none))))
      in
      let vars =
        List.mapi (fun i (field, _) ->
            field, [Field i; Field 0])
          defs
      in
      Exprs (vars, expr)
    in
    accumulate body
      ~layouts
      ~substitution
      ~copied_lets
      ~extracted_lets:(extracted::extracted_lets)
  | _ ->
  { copied_lets;
    extracted_lets;
    terminator = Flambda_utils.toplevel_substitution substitution expr;
  }

let rec make_named (symbol, (path:access list)) : Flambda.named =
  match path with
  | [] -> Symbol symbol
  | [Field i] -> Read_symbol_field (symbol, i)
  | Field h :: t ->
    let block_name = Internal_variable_names.symbol_field_block in
    let block = Variable.create block_name in
    let field_name = Internal_variable_names.get_symbol_field in
    let field = Variable.create field_name in
    Expr (
      Flambda.create_let block (make_named (symbol, t))
        (Flambda.create_let field
           (* CR mshinwell: double-check this change *)
           (Prim (Pfield (h, Pvalue Pgenval, Pointer, Mutable), [block],
             Debuginfo.none))
           (Var field)))
  | Project_var { var; kind; closure_id } :: t ->
    let closure_name = Internal_variable_names.symbol_field_closure in
    let closure = Variable.create closure_name in
    let field_name = Internal_variable_names.get_symbol_field in
    let field = Variable.create field_name in
    Expr (
      Flambda.create_let closure (make_named (symbol, t))
        (Flambda.create_let field
           (Project_var ({ closure; var; kind; closure_id}))
           (Var field)))

let rebuild_expr
      ~(extracted_definitions : (Symbol.t * projection) Variable.Map.t)
      ~(copied_definitions : Flambda.named Variable.Map.t)
      ~(substitute : bool)
      (expr : Flambda.t) =
  let expr_with_read_symbols =
    let named = Variable.Map.map make_named extracted_definitions in
    Flambda_utils.substitute_named_for_variables
      named expr
  in
  let free_variables = Flambda.free_variables expr_with_read_symbols in
  let substitution =
    if substitute then
      Variable.Map.of_set (fun x -> Variable.rename x) free_variables
    else
      Variable.Map.of_set (fun x -> x) free_variables
  in
  let expr_with_read_symbols =
    Flambda_utils.toplevel_substitution substitution
      expr_with_read_symbols
  in
  Variable.Map.fold (fun var declaration body ->
      let definition = Variable.Map.find var copied_definitions in
      Flambda.create_let declaration definition body)
    substitution expr_with_read_symbols

let rebuild (used_variables:Variable.Set.t) (accumulated:accumulated) =
  let copied_definitions = Variable.Map.of_list accumulated.copied_lets in
  let accumulated_extracted_lets =
    List.map (fun decl ->
        match decl with
        | Block (var, _, _) | Expr (var, _, _) ->
          Symbol_utils.Flambda.for_variable (Variable.rename var), decl
        | Exprs _ ->
          let name = Internal_variable_names.lifted_let_rec_block in
          let var = Variable.create name in
          Symbol_utils.Flambda.for_variable var, decl)
      accumulated.extracted_lets
  in
  let extracted_definitions =
    (* Blocks are lifted to direct top-level Initialize_block:
         accessing the value be done directly through the symbol.
       Other let bound variables are initialized inside a size
       one static block:
         accessing the value is done directly through the field 0
         of the symbol.
       let rec of size more than one is represented as a block of
       all the bound variables allocated inside a size one static
       block:
         accessing the value is done directly through the right
         field of the field 0 of the symbol. *)
    List.fold_left (fun map (symbol, decl) ->
        match decl with
        | Block (var, _tag, _fields) ->
          Variable.Map.add var (symbol, []) map
        | Expr (var, projection, _expr) ->
          Variable.Map.add var (symbol, projection) map
        | Exprs (vars, _expr) ->
          let map =
            List.fold_left (fun map (var, projection) ->
                Variable.Map.add var (symbol, projection) map)
              map vars
          in
          map)
      Variable.Map.empty accumulated_extracted_lets
  in
  let extracted =
    List.map (fun (symbol, decl) ->
        match decl with
        | Expr (var, _, decl) ->
          let expr =
            rebuild_expr ~extracted_definitions ~copied_definitions
              ~substitute:true decl
          in
          if Variable.Set.mem var used_variables then
            Initialisation
              (symbol,
               Tag.create_exn 0,
               [expr])
          else
            Effect expr
        | Exprs (_vars, decl) ->
          let expr =
            rebuild_expr ~extracted_definitions ~copied_definitions
              ~substitute:true decl
          in
          Initialisation (symbol, Tag.create_exn 0, [expr])
        | Block (_var, tag, fields) ->
          let fields =
            List.map (fun var ->
                rebuild_expr ~extracted_definitions ~copied_definitions
                  ~substitute:true (Var var))
              fields
          in
          Initialisation (symbol, tag, fields))
      accumulated_extracted_lets
  in
  let terminator =
    (* We don't need to substitute the variables in the terminator, we
       suppose that we did for every other occurrence.  Avoiding this
       substitution allows this transformation to be idempotent. *)
    rebuild_expr ~extracted_definitions ~copied_definitions
      ~substitute:false accumulated.terminator
  in
  List.rev extracted, terminator

let introduce_symbols expr =
  let accumulated =
    accumulate expr
      ~layouts:Layouts.empty
      ~substitution:Variable.Map.empty
      ~copied_lets:[] ~extracted_lets:[]
  in
  let used_variables = Flambda.used_variables expr in
  let extracted, terminator = rebuild used_variables accumulated in
  extracted, terminator

let add_extracted introduced program =
  List.fold_right (fun extracted program ->
      match extracted with
      | Initialisation (symbol, tag, def) ->
        Flambda.Initialize_symbol (symbol, tag, def, program)
      | Effect effect ->
        Flambda.Effect (effect, program))
    introduced program

let rec split_program (program : Flambda.program_body) : Flambda.program_body =
  match program with
  | End s -> End s
  | Let_symbol (s, def, program) ->
    Let_symbol (s, def, split_program program)
  | Let_rec_symbol (defs, program) ->
    Let_rec_symbol (defs, split_program program)
  | Effect (expr, program) ->
    let program = split_program program in
    let introduced, expr = introduce_symbols expr in
    add_extracted introduced (Flambda.Effect (expr, program))
  | Initialize_symbol (symbol, tag, ((_::_::_) as fields), program) ->
    (* CR-someday pchambart: currently the only initialize_symbol with more
       than 1 field is the module block. This could evolve, in that case
       this pattern should be handled properly. *)
    Initialize_symbol (symbol, tag, fields, split_program program)
  | Initialize_symbol (sym, tag, [], program) ->
    Let_symbol (sym, Block (tag, []), split_program program)
  | Initialize_symbol (symbol, tag, [field], program) ->
    let program = split_program program in
    let introduced, field = introduce_symbols field in
    add_extracted introduced
      (Flambda.Initialize_symbol (symbol, tag, [field], program))

let lift ~backend:_ (program : Flambda.program) =
  { program with
    program_body = split_program program.program_body;
  }
