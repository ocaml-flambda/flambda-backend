(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types
open Typedtree
open Lambda
open Translobj
open Translcore
open Debuginfo.Scoped_location

(* XXX Rajouter des evenements... | Add more events... *)

type error = Tags of label * label

exception Error of Location.t * error

(* Layouts for types defined in camlinternalOO.ml *)
let layout_label = layout_int
let layout_label_array = layout_array Pintarray
let layout_t = layout_any_value
let layout_obj = layout_array Pgenarray
let layout_table = layout_block
let layout_meth = layout_any_value
let layout_tables = layout_any_value


let lfunction ?(kind=Curried {nlocal=0}) ?(ret_mode=alloc_heap) return_layout params body =
  if params = [] then body else
  match kind, body with
  | Curried {nlocal=0},
    Lfunction {kind = Curried _ as kind; params = params';
               body = body'; attr; loc; mode = Alloc_heap; ret_mode }
    when attr.may_fuse_arity &&
         List.length params + List.length params' <= Lambda.max_arity() ->
      lfunction ~kind ~params:(params @ params')
                ~return:return_layout
                ~body:body'
                ~attr
                ~loc
                ~mode:alloc_heap
                ~ret_mode
  |  _ ->
      lfunction ~kind ~params ~return:return_layout
                ~body
                ~attr:default_function_attribute
                ~loc:Loc_unknown
                ~mode:alloc_heap
                ~ret_mode

let lapply ap =
  match ap.ap_func with
    Lapply ap' ->
      Lapply {ap with ap_func = ap'.ap_func; ap_args = ap'.ap_args @ ap.ap_args}
  | _ ->
      Lapply ap

let lparam name debug_uid layout : Lambda.lparam =
  { name; debug_uid; layout;
    attributes = Lambda.default_param_attribute; mode = alloc_heap }

let mkappl (func, args, layout) =
  Lprim
    (Popaque layout,
     [Lapply {
         ap_loc=Loc_unknown;
         ap_func=func;
         ap_result_layout=layout;
         ap_args=args;
         ap_region_close=Rc_normal;
         ap_mode=alloc_heap;
         ap_tailcall=Default_tailcall;
         ap_inlined=Default_inlined;
         ap_specialised=Default_specialise;
         ap_probe=None;
       }],
     Loc_unknown)

let lsequence l1 l2 =
  if l2 = lambda_unit then l1 else Lsequence(l1, l2)

let lfield v i =
  Lprim(Pfield (i, Pointer, Reads_vary), [Lvar v], Loc_unknown)

let transl_label l = share (Const_immstring l)

let transl_meth_list lst =
  if lst = [] then Lconst (const_int 0) else
  share (Const_block
            (0, List.map (fun lab -> Const_immstring lab) lst))

let set_inst_var ~scopes obj id expr =
  let ptr_or_imm, _ = Typeopt.maybe_pointer expr in
  Lprim(Psetfield_computed (ptr_or_imm, Assignment modify_heap),
    [Lvar obj; Lvar id; transl_exp ~scopes Jkind.Sort.Const.for_instance_var expr],
        Loc_unknown)

let transl_val tbl create name =
  mkappl (oo_prim (if create then "new_variable" else "get_variable"),
          [Lvar tbl; transl_label name], layout_int)

let transl_vals tbl create strict vals rem =
  List.fold_right
    (fun (name, id) rem ->
      Llet(strict, layout_int, id, Lambda.debug_uid_none,
           transl_val tbl create name, rem))
    vals rem

let meths_super tbl meths inh_meths =
  List.fold_right
    (fun (nm, id) rem ->
       try
         (nm, id,
          mkappl(oo_prim "get_method", [Lvar tbl; Lvar (Meths.find nm meths)], layout_meth))
         :: rem
       with Not_found -> rem)
    inh_meths []

let bind_super tbl (vals, meths) cl_init =
  transl_vals tbl false StrictOpt vals
    (List.fold_right (fun (_nm, id, def) rem ->
         Llet(StrictOpt, layout_meth, id, Lambda.debug_uid_none, def, rem))
       meths cl_init)

let create_object cl obj init =
  let obj' = Ident.create_local "self" in
  let obj'_duid = Lambda.debug_uid_none in
  let (inh_init, obj_init, has_init) = init obj' in
  if obj_init = lambda_unit then
    (inh_init,
     mkappl (oo_prim (if has_init then "create_object_and_run_initializers"
                      else"create_object_opt"),
             [obj; Lvar cl], layout_obj))
  else begin
   (inh_init,
    Llet(Strict, layout_obj, obj', obj'_duid,
            mkappl (oo_prim "create_object_opt", [obj; Lvar cl], layout_obj),
         Lsequence(obj_init,
                   if not has_init then Lvar obj' else
                   mkappl (oo_prim "run_initializers_opt",
                           [obj; Lvar obj'; Lvar cl], layout_obj))))
  end

let name_pattern default p =
  match p.pat_desc with
  | Tpat_var (id, _, _, _) -> id
  | Tpat_alias(_, id, _, _, _, _) -> id
  | _ -> Ident.create_local default

let rec build_object_init ~scopes cl_table obj params inh_init obj_init cl =
  match cl.cl_desc with
    Tcl_ident (path, _, _) ->
      let obj_init = Ident.create_local "obj_init" in
      let envs, inh_init = inh_init in
      let env =
        match envs with None -> []
        | Some envs ->
            [Lprim(Pfield (List.length inh_init + 1, Pointer, Reads_vary),
                   [Lvar envs],
                   Loc_unknown)]
      in
      let loc = of_location ~scopes cl.cl_loc in
      let path_lam = transl_class_path loc cl.cl_env path in
      ((envs, (path, path_lam, obj_init) :: inh_init),
       mkappl(Lvar obj_init, env @ [obj], layout_obj))
  | Tcl_structure str ->
      create_object cl_table obj (fun obj ->
        let (inh_init, obj_init, has_init) =
          List.fold_right
            (fun field (inh_init, obj_init, has_init) ->
               match field.cf_desc with
                 Tcf_inherit (_, cl, _, _, _) ->
                   let (inh_init, obj_init') =
                     build_object_init ~scopes cl_table (Lvar obj) [] inh_init
                       (fun _ -> lambda_unit) cl
                   in
                   (inh_init, lsequence obj_init' obj_init, true)
               | Tcf_val (_, _, id, Tcfk_concrete (_, exp), _) ->
                   (inh_init,
                    lsequence (set_inst_var ~scopes obj id exp) obj_init,
                    has_init)
               | Tcf_method _ | Tcf_val _ | Tcf_constraint _ | Tcf_attribute _->
                   (inh_init, obj_init, has_init)
               | Tcf_initializer _ ->
                   (inh_init, obj_init, true)
            )
            str.cstr_fields
            (inh_init, obj_init obj, false)
        in
        (inh_init,
         List.fold_right
           (fun (id, expr) rem ->
              lsequence (Lifused (id, set_inst_var ~scopes obj id expr)) rem)
           params obj_init,
         has_init))
  | Tcl_fun (_, pat, vals, cl, partial) ->
      let (inh_init, obj_init) =
        build_object_init ~scopes cl_table obj (vals @ params)
          inh_init obj_init cl
      in
      (inh_init,
       let build params rem =
         let param = name_pattern "param" pat in
         let param_duid = Lambda.debug_uid_none in
         let arg_sort = Jkind.Sort.Const.for_class_arg in
         let arg_layout =
           Typeopt.layout pat.pat_env pat.pat_loc arg_sort pat.pat_type
         in
         let body =
           Matching.for_function ~scopes ~arg_sort ~arg_layout
             ~return_layout:layout_obj pat.pat_loc None (Lvar param) [pat, rem]
             partial
         in
         Lambda.lfunction
                   ~kind:(Curried {nlocal=0})
                   ~params:(lparam param param_duid arg_layout::params)
                   ~return:layout_obj
                   ~attr:default_function_attribute
                   ~loc:(of_location ~scopes pat.pat_loc)
                   ~body
                   ~mode:alloc_heap
                   ~ret_mode:alloc_heap
       in
       begin match obj_init with
         Lfunction {kind = Curried {nlocal=0}; params; body = rem} ->
          build params rem
       | rem ->
          build [] rem
       end)
  | Tcl_apply (cl, oexprs) ->
      let (inh_init, obj_init) =
        build_object_init ~scopes cl_table obj params inh_init obj_init cl
      in
      (inh_init, transl_apply ~scopes ~result_layout:layout_object obj_init oexprs Loc_unknown)
  | Tcl_let (rec_flag, defs, vals, cl) ->
      let (inh_init, obj_init) =
        build_object_init ~scopes cl_table obj (vals @ params)
          inh_init obj_init cl
      in
      (inh_init, Translcore.transl_let ~return_layout:layout_obj ~scopes
                   rec_flag defs obj_init)
  | Tcl_open (_, cl)
  | Tcl_constraint (cl, _, _, _, _) ->
      build_object_init ~scopes cl_table obj params inh_init obj_init cl

let rec build_object_init_0
          ~scopes cl_table params cl copy_env subst_env top ids =
  match cl.cl_desc with
    Tcl_let (_rec_flag, _defs, vals, cl) ->
      build_object_init_0
        ~scopes cl_table (vals@params) cl copy_env subst_env top ids
  | _ ->
      let self = Ident.create_local "self" in
      let self_duid = Lambda.debug_uid_none in
      let env = Ident.create_local "env" in
      let env_duid = Lambda.debug_uid_none in
      let obj = if ids = [] then lambda_unit else Lvar self in
      let envs = if top then None else Some env in
      let ((_,inh_init), obj_init) =
        build_object_init ~scopes cl_table obj params (envs,[]) copy_env cl in
      let obj_init =
        if ids = []
        then obj_init
        else lfunction layout_obj [lparam self self_duid layout_obj] obj_init
      in
      (inh_init, lfunction (if ids = [] then layout_obj else layout_function)
         [lparam env env_duid layout_block] (subst_env env inh_init obj_init))


let bind_method tbl lab id cl_init =
  (* CR sspies: Can we get a better debugging uid here? *)
  Llet(Strict, layout_label, id, Lambda.debug_uid_none,
                mkappl (oo_prim "get_method_label",
                           [Lvar tbl; transl_label lab], layout_label),
       cl_init)

let bind_methods tbl meths vals cl_init =
  let methl = Meths.fold (fun lab id tl -> (lab,id) :: tl) meths [] in
  let len = List.length methl and nvals = List.length vals in
  if len < 2 && nvals = 0 then Meths.fold (bind_method tbl) meths cl_init else
  if len = 0 && nvals < 2 then transl_vals tbl true Strict vals cl_init else
  let ids = Ident.create_local "ids" in
  let ids_duid = Lambda.debug_uid_none in
  let i = ref (len + nvals) in
  let getter, names =
    if nvals = 0 then "get_method_labels", [] else
    "new_methods_variables",
    [transl_meth_list (List.map fst vals)]
  in
  Llet(Strict, layout_label_array, ids, ids_duid,
       mkappl (oo_prim getter,
               [Lvar tbl;
                transl_meth_list (List.map fst methl)] @ names,
              layout_label_array),
       List.fold_right
         (* CR sspies: Can we get a better debugging uid here? *)
         (fun (_lab, id) lam -> decr i; Llet(StrictOpt, layout_label, id,
                                           Lambda.debug_uid_none,
                                           lfield ids !i, lam))
         (methl @ vals) cl_init)

let output_methods tbl methods lam =
  match methods with
    [] -> lam
  | [lab; code] ->
      lsequence (mkappl(oo_prim "set_method", [Lvar tbl; lab; code], layout_unit)) lam
  | _ ->
      let methods =
        Lprim(Pmakeblock(0,Immutable,None,alloc_heap), methods, Loc_unknown)
      in
      lsequence (mkappl(oo_prim "set_methods",
                        [Lvar tbl; Lprim (Popaque layout_block,
                                          [methods], Loc_unknown)], layout_unit))
        lam

let rec ignore_cstrs cl =
  match cl.cl_desc with
    Tcl_constraint (cl, _, _, _, _) -> ignore_cstrs cl
  | Tcl_apply (cl, _) -> ignore_cstrs cl
  | _ -> cl

let rec index a = function
    [] -> raise Not_found
  | b :: l ->
      if b = a then 0 else 1 + index a l

let bind_id_as_val (id, _) = ("", id)

let class_field i = Pfield (i, Pointer, Reads_vary)

let rec build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl =
  match cl.cl_desc with
  | Tcl_ident _ ->
      begin match inh_init with
      | (_, path_lam, obj_init)::inh_init ->
          (inh_init,
           Llet (Strict, layout_t, obj_init, Lambda.debug_uid_none,
                 mkappl(Lprim(class_field 1, [path_lam], Loc_unknown), (Lvar cla ::
                        if top then [Lprim(class_field 3, [path_lam], Loc_unknown)]
                        else []), layout_t),
                 bind_super cla super cl_init))
      | _ ->
          assert false
      end
  | Tcl_structure str ->
      let cl_init = bind_super cla super cl_init in
      let (inh_init, cl_init, methods, values) =
        List.fold_right
          (fun field (inh_init, cl_init, methods, values) ->
            match field.cf_desc with
              Tcf_inherit (_, cl, _, vals, meths) ->
                let cl_init = output_methods cla methods cl_init in
                let inh_init, cl_init =
                  build_class_init ~scopes cla false
                    (vals, meths_super cla str.cstr_meths meths)
                    inh_init cl_init msubst top cl in
                (inh_init, cl_init, [], values)
            | Tcf_val (name, _, id, _, over) ->
                let values =
                  if over then values else (name.txt, id) :: values
                in
                (inh_init, cl_init, methods, values)
            | Tcf_method (_, _, Tcfk_virtual _)
            | Tcf_constraint _
              ->
                (inh_init, cl_init, methods, values)
            | Tcf_method (name, _, Tcfk_concrete (_, exp)) ->
                let scopes = enter_method_definition ~scopes name.txt in
                let met_code =
                  msubst true (transl_scoped_exp ~scopes Jkind.Sort.Const.for_method exp)
                in
                let met_code =
                  if !Clflags.native_code && List.length met_code = 1 then
                    (* Force correct naming of method for profiles *)
                    let met = Ident.create_local ("method_" ^ name.txt) in
                    [Llet(Strict, layout_meth, met, Lambda.debug_uid_none,
                          List.hd met_code, Lvar met)]
                  else met_code
                in
                (inh_init, cl_init,
                 Lvar(Meths.find name.txt str.cstr_meths) :: met_code @ methods,
                 values)
            | Tcf_initializer exp ->
                (inh_init,
                 Lsequence(mkappl (oo_prim "add_initializer",
                                   Lvar cla :: msubst false
                                                 (transl_exp ~scopes
                                                    Jkind.Sort.Const.for_initializer exp),
                                   layout_unit),
                           cl_init),
                 methods, values)
            | Tcf_attribute _ ->
                (inh_init, cl_init, methods, values))
          str.cstr_fields
          (inh_init, cl_init, [], [])
      in
      let cl_init = output_methods cla methods cl_init in
      (inh_init, bind_methods cla str.cstr_meths values cl_init)
  | Tcl_fun (_, _pat, vals, cl, _) ->
      let (inh_init, cl_init) =
        build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl
      in
      let vals = List.map bind_id_as_val vals in
      (inh_init, transl_vals cla true StrictOpt vals cl_init)
  | Tcl_apply (cl, _exprs) ->
      build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl
  | Tcl_let (_rec_flag, _defs, vals, cl) ->
      let (inh_init, cl_init) =
        build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl
      in
      let vals = List.map bind_id_as_val vals in
      (inh_init, transl_vals cla true StrictOpt vals cl_init)
  | Tcl_constraint (cl, _, vals, meths, concr_meths) ->
      let virt_meths =
        List.filter (fun lab -> not (MethSet.mem lab concr_meths)) meths in
      let concr_meths = MethSet.elements concr_meths in
      let narrow_args =
        [Lvar cla;
         transl_meth_list vals;
         transl_meth_list virt_meths;
         transl_meth_list concr_meths] in
      let cl = ignore_cstrs cl in
      begin match cl.cl_desc, inh_init with
      | Tcl_ident (path, _, _), (path', path_lam, obj_init)::inh_init ->
          assert (Path.same path path');
          let inh = Ident.create_local "inh"
          and inh_duid = Lambda.debug_uid_none
          and ofs = List.length vals + 1
          and valids, methids = super in
          let cl_init =
            List.fold_left
              (fun init (nm, id, _) ->
                Llet(StrictOpt, layout_meth, id, Lambda.debug_uid_none,
                     lfield inh (index nm concr_meths + ofs),
                     init))
              cl_init methids in
          let cl_init =
            List.fold_left
              (fun init (nm, id) ->
                Llet(StrictOpt, layout_meth, id, Lambda.debug_uid_none,
                     lfield inh (index nm vals + 1), init))
              cl_init valids in
          (inh_init,
           Llet (Strict, layout_array Pgenarray, inh, inh_duid,
                 mkappl(oo_prim "inherits", narrow_args @
                        [path_lam;
                         Lconst(const_int (if top then 1 else 0))],
                       layout_array Pgenarray),
                 Llet(StrictOpt, layout_t, obj_init, Lambda.debug_uid_none,
                  lfield inh 0, cl_init)))
      | _ ->
          let core cl_init =
            build_class_init
              ~scopes cla true super inh_init cl_init msubst top cl
          in
          if cstr then core cl_init else
          let (inh_init, cl_init) =
            core (Lsequence (mkappl (oo_prim "widen", [Lvar cla], layout_unit), cl_init))
          in
          (inh_init,
           Lsequence(mkappl (oo_prim "narrow", narrow_args, layout_unit),
                     cl_init))
      end
  | Tcl_open (_, cl) ->
      build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl

let rec build_class_lets ~scopes cl =
  match cl.cl_desc with
    Tcl_let (rec_flag, defs, _vals, cl') ->
      let env, wrap = build_class_lets ~scopes cl' in
      (env, fun return_layout lam_and_kind ->
          let lam, rkind = wrap return_layout lam_and_kind in
          Translcore.transl_let ~scopes ~return_layout rec_flag defs lam,
          rkind)
  | _ ->
      (cl.cl_env, fun _ lam_and_kind -> lam_and_kind)

let rec get_class_meths cl =
  match cl.cl_desc with
    Tcl_structure cl ->
      Meths.fold (fun _ -> Ident.Set.add) cl.cstr_meths Ident.Set.empty
  | Tcl_ident _ -> Ident.Set.empty
  | Tcl_fun (_, _, _, cl, _)
  | Tcl_let (_, _, _, cl)
  | Tcl_apply (cl, _)
  | Tcl_open (_, cl)
  | Tcl_constraint (cl, _, _, _, _) -> get_class_meths cl

(*
   XXX Il devrait etre peu couteux d'ecrire des classes :
   |   Writing classes should be cheap
     class c x y = d e f
*)
let rec transl_class_rebind ~scopes obj_init cl vf =
  match cl.cl_desc with
    Tcl_ident (path, _, _) ->
      if vf = Concrete then begin
        try if (Env.find_class path cl.cl_env).cty_new = None then raise Exit
        with Not_found -> raise Exit
      end;
      let cl_loc = of_location ~scopes cl.cl_loc in
      let path_lam = transl_class_path cl_loc cl.cl_env path in
      (path, path_lam, obj_init)
  | Tcl_fun (_, pat, _, cl, partial) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      let build params rem =
        let param = name_pattern "param" pat in
        let param_duid = Lambda.debug_uid_none in
        let arg_sort = Jkind.Sort.Const.for_class_arg in
        let arg_layout =
          Typeopt.layout pat.pat_env pat.pat_loc arg_sort pat.pat_type
        in
        let return_layout = layout_class in
        let body =
          Matching.for_function ~scopes ~arg_sort ~arg_layout ~return_layout pat.pat_loc
            None (Lvar param) [pat, rem] partial
        in
        Lambda.lfunction
                  ~kind:(Curried {nlocal=0})
                  ~params:(lparam param param_duid arg_layout :: params)
                  ~return:return_layout
                  ~attr:default_function_attribute
                  ~loc:(of_location ~scopes pat.pat_loc)
                  ~body
                  ~mode:alloc_heap
                  ~ret_mode:alloc_heap
      in
      (path, path_lam,
       match obj_init with
         Lfunction {kind = Curried {nlocal=0}; params; body} ->
          build params body
       | rem ->
          build [] rem)
  | Tcl_apply (cl, oexprs) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      (path, path_lam, transl_apply ~scopes ~result_layout:layout_class obj_init oexprs Loc_unknown)
  | Tcl_let (rec_flag, defs, _vals, cl) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      (path, path_lam,
       Translcore.transl_let ~scopes ~return_layout:layout_obj rec_flag defs
         obj_init)
  | Tcl_structure _ -> raise Exit
  | Tcl_constraint (cl', _, _, _, _) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl' vf in
      let rec check_constraint = function
          Cty_constr(path', _, _) when Path.same path path' -> ()
        | Cty_arrow (_, _, cty) -> check_constraint cty
        | _ -> raise Exit
      in
      check_constraint cl.cl_type;
      (path, path_lam, obj_init)
  | Tcl_open (_, cl) ->
      transl_class_rebind ~scopes obj_init cl vf

let rec transl_class_rebind_0 ~scopes (self:Ident.t) self_debug_uid obj_init
  cl vf =
  match cl.cl_desc with
    Tcl_let (rec_flag, defs, _vals, cl) ->
      let path, path_lam, obj_init =
        transl_class_rebind_0 ~scopes self self_debug_uid obj_init cl vf
      in
      (path, path_lam,
       Translcore.transl_let ~scopes ~return_layout:layout_obj rec_flag defs
         obj_init)
  | _ ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      (path, path_lam,
       lfunction layout_obj [lparam self self_debug_uid layout_obj] obj_init)

let transl_class_rebind ~scopes cl vf =
  try
    let obj_init = Ident.create_local "obj_init"
    and obj_init_duid = Lambda.debug_uid_none
    and self = Ident.create_local "self"
    and self_debug_uid = Lambda.debug_uid_none in
    let obj_init0 =
      lapply {
        ap_loc=Loc_unknown;
        ap_func=Lvar obj_init;
        ap_args=[Lvar self];
        ap_result_layout=layout_obj;
        ap_region_close=Rc_normal;
        ap_mode=alloc_heap;
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inlined;
        ap_specialised=Default_specialise;
        ap_probe=None;
      }
    in
    let _, path_lam, obj_init' =
      transl_class_rebind_0 ~scopes self self_debug_uid obj_init0 cl vf in
    let id = (obj_init' = lfunction layout_obj
                            [lparam self self_debug_uid layout_obj] obj_init0)
    in
    if id then path_lam else

    let cla = Ident.create_local "class"
    and cla_duid = Lambda.debug_uid_none
    and new_init = Ident.create_local "new_init"
    and new_init_duid = Lambda.debug_uid_none
    and env_init = Ident.create_local "env_init"
    and env_init_duid = Lambda.debug_uid_none
    and table = Ident.create_local "table"
    and table_duid = Lambda.debug_uid_none
    and envs = Ident.create_local "envs"
    and envs_duid = Lambda.debug_uid_none in
    Llet(
    Strict, layout_function, new_init, new_init_duid, lfunction layout_function
            [lparam obj_init obj_init_duid layout_function] obj_init',
    Llet(
    Alias, layout_block, cla, cla_duid, path_lam,
    Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
          [mkappl(Lvar new_init, [lfield cla 0], layout_function);
           lfunction layout_function [lparam table table_duid layout_table]
             (Llet(Strict, layout_function, env_init, env_init_duid,
                   mkappl(lfield cla 1, [Lvar table], layout_function),
                   lfunction layout_function
                     [lparam envs envs_duid layout_block]
                     (mkappl(Lvar new_init,
                             [mkappl(Lvar env_init, [Lvar envs], layout_obj)], layout_function))));
           lfield cla 2;
           lfield cla 3],
          Loc_unknown)))
  with Exit ->
    lambda_unit

(* Rewrite a closure using builtins. Improves native code size. *)

let const_path local = function
    Lvar id -> not (List.mem id local)
  | Lconst _ -> true
  | Lfunction {kind = Curried _; body} ->
      let fv = free_variables body in
      List.for_all (fun x -> not (Ident.Set.mem x fv)) local
  | _ -> false

let rec builtin_meths self env env2 body =
  let const_path = const_path (env::self) in
  let conv = function
    (* Lvar s when List.mem s self ->  "_self", [] *)
    | p when const_path p -> "const", [p]
    | Lprim(Parrayrefu _, [Lvar s; Lvar n], _) when List.mem s self ->
        "var", [Lvar n]
    | Lprim(Pfield(n, _, _), [Lvar e], _) when Ident.same e env ->
        "env", [Lvar env2; Lconst(const_int n)]
    | Lsend(Self, met, Lvar s, [], _, _, _, _) when List.mem s self ->
        "meth", [met]
    | _ -> raise Not_found
  in
  match body with
  | Llet(_str, _k, s', _duid, Lvar s, body) when List.mem s self ->
      builtin_meths (s'::self) env env2 body
  | Lapply{ap_func = f; ap_args = [arg]} when const_path f ->
      let s, args = conv arg in ("app_"^s, f :: args)
  | Lapply{ap_func = f; ap_args = [arg; p]} when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_"^s^"_const", f :: args @ [p])
  | Lapply{ap_func = f; ap_args = [p; arg]} when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_const_"^s, f :: p :: args)
  | Lsend(Self, Lvar n, Lvar s, [arg], _, _, _, _) when List.mem s self ->
      let s, args = conv arg in
      ("meth_app_"^s, Lvar n :: args)
  | Lsend(Self, met, Lvar s, [], _, _, _, _) when List.mem s self ->
      ("get_meth", [met])
  | Lsend(Public, met, arg, [], _, _, _, _) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lsend(Cached, met, arg, [_;_], _, _, _, _) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lfunction {kind = Curried _; params = [{name = x; _}]; body} ->
      let rec enter self = function
        | Lprim(Parraysetu _, [Lvar s; Lvar n; Lvar x'], _)
          when Ident.same x x' && List.mem s self ->
            ("set_var", [Lvar n])
        | Llet(_str, _k, s', _duid, Lvar s, body) when List.mem s self ->
            enter (s'::self) body
        | _ -> raise Not_found
      in enter self body
  | Lfunction _ -> raise Not_found
  | _ ->
      let s, args = conv body in ("get_"^s, args)

module M = struct
  open CamlinternalOO
  let builtin_meths self env env2 body =
    let builtin, args = builtin_meths self env env2 body in
    (* if not arr then [mkappl(oo_prim builtin, args)] else *)
    let tag = match builtin with
      "get_const" -> GetConst
    | "get_var"   -> GetVar
    | "get_env"   -> GetEnv
    | "get_meth"  -> GetMeth
    | "set_var"   -> SetVar
    | "app_const" -> AppConst
    | "app_var"   -> AppVar
    | "app_env"   -> AppEnv
    | "app_meth"  -> AppMeth
    | "app_const_const" -> AppConstConst
    | "app_const_var"   -> AppConstVar
    | "app_const_env"   -> AppConstEnv
    | "app_const_meth"  -> AppConstMeth
    | "app_var_const"   -> AppVarConst
    | "app_env_const"   -> AppEnvConst
    | "app_meth_const"  -> AppMethConst
    | "meth_app_const"  -> MethAppConst
    | "meth_app_var"    -> MethAppVar
    | "meth_app_env"    -> MethAppEnv
    | "meth_app_meth"   -> MethAppMeth
    | "send_const" -> SendConst
    | "send_var"   -> SendVar
    | "send_env"   -> SendEnv
    | "send_meth"  -> SendMeth
    | _ -> assert false
    in Lconst(const_int (Obj.magic tag)) :: args
end
open M


(*
   Class translation.
   Three subcases:
    * reapplication of a known class -> transl_class_rebind
    * class without local dependencies -> direct translation
    * with local dependencies -> generate a stubs tree,
      with a node for every local classes inherited
   A class is a 4-tuple:
    (obj_init, class_init, env_init, env)
    obj_init: creation function (unit -> obj)
    class_init: inheritance function (table -> env_init)
      (one by source code)
    env_init: parameterisation by the local environment
      (env -> params -> obj_init)
      (one for each combination of inherited class_init )
    env: local environment
   If ids=0 (immediate object), then only env_init is conserved.
*)

(*
let prerr_ids msg ids =
  let names = List.map Ident.unique_toplevel_name ids in
  prerr_endline (String.concat " " (msg :: names))
*)

let free_methods l =
  let fv = ref Ident.Set.empty in
  let rec free l =
    Lambda.iter_head_constructor free l;
    match l with
    | Lsend(Self, Lvar meth, _, _, _, _, _, _) ->
        fv := Ident.Set.add meth !fv
    | Lsend _ -> ()
    | Lfunction{params} ->
        List.iter (fun p -> fv := Ident.Set.remove p.name !fv) params
    | Llet(_, _k, id, _duid, _arg, _body)
    | Lmutlet(_k, id, _duid, _arg, _body) ->
        fv := Ident.Set.remove id !fv
    | Lletrec(decl, _body) ->
        List.iter (fun { id } -> fv := Ident.Set.remove id !fv) decl
    | Lstaticcatch(_e1, (_,vars), _e2, _, _kind) ->
        List.iter (fun (id, _, _) -> fv := Ident.Set.remove id !fv) vars
    | Ltrywith(_e1, exn, _duid, _e2, _k) ->
        fv := Ident.Set.remove exn !fv
    | Lfor {for_id} ->
        fv := Ident.Set.remove for_id !fv
    | Lassign _
    | Lvar _ | Lmutvar _ | Lconst _ | Lapply _
    | Lprim _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
    | Lifthenelse _ | Lsequence _ | Lwhile _
    | Levent _ | Lifused _ | Lregion _ | Lexclave _ -> ()
  in free l; !fv

let transl_class ~scopes ids cl_id pub_meths cl vflag =
  let open Value_rec_types in
  (* First check if it is not only a rebind *)
  let rebind = transl_class_rebind ~scopes cl vflag in
  if rebind <> lambda_unit then rebind, Dynamic else

  (* Prepare for heavy environment handling *)
  let scopes = enter_class_definition ~scopes cl_id in
  let tables = Ident.create_local (Ident.name cl_id ^ "_tables") in
  let (top_env, req) = oo_add_class tables in
  let top = not req in
  let cl_env, llets = build_class_lets ~scopes cl in
  let new_ids = if top then [] else Env.diff top_env cl_env in
  let env2 = Ident.create_local "env"
  and env2_duid = Lambda.debug_uid_none in
  let meth_ids = get_class_meths cl in
  let subst env lam i0 new_ids' =
    let fv = free_variables lam in
    (* prerr_ids "cl_id =" [cl_id]; prerr_ids "fv =" (Ident.Set.elements fv); *)
    let fv = List.fold_right Ident.Set.remove !new_ids' fv in
    (* We need to handle method ids specially, as they do not appear
       in the typing environment (PR#3576, PR#4560) *)
    (* very hacky: we add and remove free method ids on the fly,
       depending on the visit order... *)
    method_ids :=
      Ident.Set.diff (Ident.Set.union (free_methods lam) !method_ids) meth_ids;
    (* prerr_ids "meth_ids =" (Ident.Set.elements meth_ids);
       prerr_ids "method_ids =" (Ident.Set.elements !method_ids); *)
    let new_ids = List.fold_right Ident.Set.add new_ids !method_ids in
    let fv = Ident.Set.inter fv new_ids in
    new_ids' := !new_ids' @ Ident.Set.elements fv;
    (* prerr_ids "new_ids' =" !new_ids'; *)
    let i = ref (i0-1) in
    List.fold_left
      (fun subst id ->
        incr i; Ident.Map.add id (lfield env !i)  subst)
      Ident.Map.empty !new_ids'
  in
  let new_ids_meths = ref [] in
  let no_env_update _ _ env = env in
  let msubst arr = function
      Lfunction {kind = Curried _ as kind; ret_mode;
                 params = self :: args; return; body} ->
        let env = Ident.create_local "env" in
        let env_duid = Lambda.debug_uid_none in
        let body' =
          if new_ids = [] then body else
          Lambda.subst no_env_update (subst env body 0 new_ids_meths) body in
        begin try
          (* Doesn't seem to improve size for bytecode *)
          (* if not !Clflags.native_code then raise Not_found; *)
          if not arr || !Clflags.debug then raise Not_found;
          builtin_meths [self.name] env env2 (lfunction return args body')
        with Not_found ->
          [lfunction ~kind ~ret_mode return (self :: args)
             (if not (Ident.Set.mem env (free_variables body')) then body' else
              Llet(Alias, layout_block, env, env_duid,
                   Lprim(Pfield_computed Reads_vary,
                         [Lvar self.name; Lvar env2],
                         Loc_unknown),
                   body'))]
        end
      | _ -> assert false
  in
  let new_ids_init = ref [] in
  let env1 = Ident.create_local "env"
  and env1_duid = Lambda.debug_uid_none
  and env1' = Ident.create_local "env'"
  and env1'_duid = Lambda.debug_uid_none in
  let copy_env self =
    if top then lambda_unit else
    Lifused(env2, Lprim(Psetfield_computed (Pointer, Assignment modify_heap),
                        [Lvar self; Lvar env2; Lvar env1'],
                        Loc_unknown))
  and subst_env envs l lam =
    if top then lam else
    (* must be called only once! *)
    let lam = Lambda.subst no_env_update (subst env1 lam 1 new_ids_init) lam in
    Llet(Alias, layout_block, env1, env1_duid,
         (if l = [] then Lvar envs else lfield envs 0),
    Llet(Alias, layout_block, env1', env1'_duid,
         (if !new_ids_init = [] then Lvar env1 else lfield env1 0),
         lam))
  in

  (* Now we start compiling the class *)
  let cla = Ident.create_local "class" in
  let cla_duid = Lambda.debug_uid_none in
  let (inh_init, obj_init) =
    build_object_init_0 ~scopes cla [] cl copy_env subst_env top ids in
  let inh_init' = List.rev inh_init in
  let (inh_init', cl_init) =
    build_class_init ~scopes cla true ([],[]) inh_init' obj_init msubst top cl
  in
  assert (inh_init' = []);
  let table = Ident.create_local "table"
  and table_duid = Lambda.debug_uid_none
  and class_init = Ident.create_local (Ident.name cl_id ^ "_init")
  and class_init_duid = Lambda.debug_uid_none
  and env_init = Ident.create_local "env_init"
  and env_init_duid = Lambda.debug_uid_none
  and obj_init = Ident.create_local "obj_init"
  and obj_init_duid = Lambda.debug_uid_none in
  let pub_meths =
    List.sort
      (fun s s' -> compare (Btype.hash_variant s) (Btype.hash_variant s'))
      pub_meths in
  let tags = List.map Btype.hash_variant pub_meths in
  let rev_map = List.combine tags pub_meths in
  List.iter2
    (fun tag name ->
      let name' = List.assoc tag rev_map in
      if name' <> name then raise(Error(cl.cl_loc, Tags(name, name'))))
    tags pub_meths;
  let ltable table lam =
    Llet(Strict, layout_table, table, table_duid,
         mkappl (oo_prim "create_table", [transl_meth_list pub_meths],
                layout_table), lam)
  and ldirect obj_init =
    Llet(Strict, layout_function, obj_init, obj_init_duid, cl_init,
         Lsequence(mkappl (oo_prim "init_class", [Lvar cla], layout_unit),
                   mkappl (Lvar obj_init, [lambda_unit], layout_function)))
  in
  (* Simplest case: an object defined at toplevel (ids=[]) *)
  if top && ids = [] then llets layout_table (ltable cla (ldirect obj_init), Dynamic) else

  let concrete = (vflag = Concrete)
  and lclass mk_lam_and_kind =
    let cl_init, _ =
      llets layout_function
        (Lambda.lfunction
           ~kind:(Curried {nlocal=0})
           ~attr:default_function_attribute
           ~loc:Loc_unknown
           ~return:layout_function
           ~mode:alloc_heap
           ~ret_mode:alloc_heap
           ~params:[lparam cla cla_duid layout_table]
           ~body:cl_init,
         Dynamic (* Placeholder, real kind is computed in [lbody] below *))
    in
    let lam, rkind = mk_lam_and_kind (free_variables cl_init) in
    Llet(Strict, layout_function, class_init, class_init_duid, cl_init, lam),
    rkind
  and lbody fv =
    if List.for_all (fun id -> not (Ident.Set.mem id fv)) ids then
      mkappl (oo_prim "make_class",[transl_meth_list pub_meths;
                                    Lvar class_init], layout_block),
      Dynamic
    else
      ltable table (
      Llet(
      Strict, layout_function, env_init, env_init_duid,
      mkappl (Lvar class_init, [Lvar table], layout_function),
      Lsequence(
      mkappl (oo_prim "init_class", [Lvar table], layout_unit),
      Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
            [mkappl (Lvar env_init, [lambda_unit], layout_obj);
             Lvar class_init; Lvar env_init; lambda_unit],
            Loc_unknown)))),
      Static
  and lbody_virt lenvs =
    Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
          [lambda_unit; Lambda.lfunction
                          ~kind:(Curried {nlocal=0})
                          ~attr:default_function_attribute
                          ~loc:Loc_unknown
                          ~return:layout_function
                          ~mode:alloc_heap
                          ~ret_mode:alloc_heap
                          ~params:[lparam cla cla_duid layout_table]
                          ~body:cl_init;
           lambda_unit; lenvs],
         Loc_unknown),
    Static
  in
  (* Still easy: a class defined at toplevel *)
  if top && concrete then lclass lbody else
  if top then llets layout_block (lbody_virt lambda_unit) else

  (* Now for the hard stuff: prepare for table caching *)
  let envs = Ident.create_local "envs"
  and envs_duid = Lambda.debug_uid_none
  and cached = Ident.create_local "cached"
  and cached_duid = Lambda.debug_uid_none in
  let lenvs =
    if !new_ids_meths = [] && !new_ids_init = [] && inh_init = []
    then lambda_unit
    else Lvar envs in
  let lenv =
    let menv =
      if !new_ids_meths = [] then lambda_unit else
      Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
            List.map (fun id -> Lvar id) !new_ids_meths,
            Loc_unknown) in
    if !new_ids_init = [] then menv else
    Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
          menv :: List.map (fun id -> Lvar id) !new_ids_init,
          Loc_unknown)
  and linh_envs =
    List.map
      (fun (_, path_lam, _) -> Lprim(class_field 3, [path_lam], Loc_unknown))
      (List.rev inh_init)
  in
  let make_envs (lam, rkind) =
    Llet(StrictOpt, layout_block, envs, envs_duid,
         (if linh_envs = [] then lenv else
         Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
               lenv :: linh_envs, Loc_unknown)),
         lam),
    rkind
  and def_ids cla lam =
    Llet(StrictOpt, layout_int, env2, env2_duid,
         mkappl (oo_prim "new_variable", [Lvar cla; transl_label ""], layout_int),
         lam)
  in
  let inh_paths =
    List.filter
      (fun (path, _, _) -> List.mem (Path.head path) new_ids) inh_init
  in
  let inh_keys =
    List.map
      (fun (_, path_lam, _) ->
        Lprim(class_field 1, [path_lam], Loc_unknown))
      inh_paths
  in
  let lclass lam =
    Llet(Strict, layout_function, class_init, class_init_duid,
         Lambda.lfunction
                   ~kind:(Curried {nlocal=0})
                   ~params:[lparam cla cla_duid layout_table]
                   ~return:layout_function
                   ~attr:default_function_attribute
                   ~loc:Loc_unknown
                   ~mode:alloc_heap
                   ~ret_mode:alloc_heap
                   ~body:(def_ids cla cl_init), lam)
  and lset cached i lam =
    Lprim(Psetfield(i, Pointer, Assignment modify_heap),
          [Lvar cached; lam], Loc_unknown)
  in
  let ldirect () =
    ltable cla
      (Llet(Strict, layout_function, env_init, env_init_duid,
            def_ids cla cl_init,
            Lsequence(mkappl (oo_prim "init_class", [Lvar cla], layout_unit),
                      lset cached 0 (Lvar env_init))))
  and lclass_virt () =
    lset cached 0
      (Lambda.lfunction
         ~kind:(Curried {nlocal=0})
         ~attr:default_function_attribute
         ~loc:Loc_unknown
         ~mode:alloc_heap
         ~ret_mode:alloc_heap
         ~return:layout_function
         ~params:[lparam cla cla_duid layout_table]
         ~body:(def_ids cla cl_init))
  in
  let lupdate_cache =
    if ids = [] then ldirect () else
      if not concrete then lclass_virt () else
        lclass (
            mkappl (oo_prim "make_class_store",
                    [transl_meth_list pub_meths;
                     Lvar class_init; Lvar cached], layout_unit)) in
  let lcheck_cache =
    if !Clflags.native_code && !Clflags.afl_instrument then
      (* When afl-fuzz instrumentation is enabled, ignore the cache
         so that the program's behaviour does not change between runs *)
      lupdate_cache
    else
      Lifthenelse(lfield cached 0, lambda_unit, lupdate_cache, layout_unit) in
  let lcache (lam, rkind) =
    let lam = Lsequence (lcheck_cache, lam) in
    let lam =
      if inh_keys = []
      then Llet(Alias, layout_tables, cached, cached_duid, Lvar tables, lam)
      else
        Llet(Strict, layout_tables, cached, cached_duid,
             mkappl (oo_prim "lookup_tables",
                     [Lvar tables; Lprim(Pmakearray(Paddrarray, Immutable, alloc_heap),
                                         inh_keys, Loc_unknown)], layout_tables),
             lam)
    in
    lam, rkind
  in
  llets layout_block (
  lcache (
  make_envs (
  if ids = []
  then mkappl (lfield cached 0, [lenvs], layout_obj), Dynamic
  else
    Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
        (if concrete then
          [mkappl (lfield cached 0, [lenvs], layout_obj);
           lfield cached 1;
           lfield cached 0;
           lenvs]
        else [lambda_unit; lfield cached 0; lambda_unit; lenvs]),
        Loc_unknown
       ),
    Static)))

(* Wrapper for class compilation *)
(*
    let cl_id = ci.ci_id_class in
(* TODO: cl_id is used somewhere else as typesharp ? *)
  let _arity = List.length ci.ci_params in
  let pub_meths = m in
  let cl = ci.ci_expr in
  let vflag = vf in
*)

let transl_class ~scopes ids id pub_meths cl vf =
  oo_wrap_gen cl.cl_env false (transl_class ~scopes ids id pub_meths cl) vf

let () =
  transl_object := (fun ~scopes id meths cl ->
    let lam, _rkind = transl_class ~scopes [] id meths cl Concrete in
    lam)

(* Error report *)

open Format
module Style = Misc.Style

let report_error ppf = function
  | Tags (lab1, lab2) ->
      fprintf ppf "Method labels %a and %a are incompatible.@ %s"
        Style.inline_code lab1
        Style.inline_code lab2
        "Change one of them."

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
