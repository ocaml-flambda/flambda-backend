open Lambda
open Typedtree
open Asttypes

let int n = Lconst (Const_base (Const_int n))

type binding =
  { let_kind : let_kind;
    value_kind : value_kind;
    var : Ident.t;
    init : lambda }

let binding let_kind value_kind var init =
  {let_kind; value_kind; var; init}

let gen_binding {let_kind; value_kind; var; init} body =
  Llet(let_kind, value_kind, var, init, body)

let gen_bindings bindings body =
  List.fold_right gen_binding bindings body

let valuekind_of_arraykind = function
  | Pgenarray -> Pgenval
  | Paddrarray | Pintarray -> Pintval
  | Pfloatarray -> Pfloatval


(* Translate a clause into some initialising bindings, a variable
   that will be bound to the number of iterations in the clause by
   those bindings, and lambda code that performs the iterations. *)
let transl_arr_clause ~transl_exp ~scopes ~loc clause body =
  let len_var = Ident.create_local "len_var" in
  let bindings, for_ =
    match clause with
    | In (pat , e2) ->
        let in_var = Ident.create_local "in_var" in
        let in_kind = Typeopt.array_kind e2 in
        let in_binding =
          binding Strict Pgenval in_var (transl_exp ~scopes e2)
        in
        let len_binding =
          let init = Lprim( (Parraylength(in_kind)), [Lvar(in_var)], loc) in
          binding Alias Pintval len_var init
        in
        let index = Ident.create_local "index" in
        let for_ =
          Lfor(index, (int 0), Lprim(Psubint, [Lvar(len_var); int 1], loc) , Upto,
            Matching.for_let ~scopes pat.pat_loc
              (Lprim(Parrayrefu(in_kind),
                     [Lvar(in_var); Lvar(index)], loc)) pat
              (valuekind_of_arraykind in_kind)
              body)
        in
        [in_binding; len_binding], for_
    | From_to(id, _, e2, e3, dir) ->
        let from_var = Ident.create_local "from" in
        let from_binding =
          binding Strict Pintval from_var (transl_exp ~scopes e2)
        in
        let to_var = Ident.create_local "to" in
        let to_binding =
          binding Strict Pintval to_var (transl_exp ~scopes e3)
        in
        let low, high =
          match dir with
          | Upto -> Lvar from_var, Lvar to_var
          | Downto -> Lvar to_var, Lvar from_var
        in
        let len_binding =
          let init =
            Lprim(Psubint, [Lprim(Paddint, [high; int 1], loc); low], loc)
          in
          binding Alias Pintval len_var init
        in
        let for_ = Lfor(id, Lvar from_var, Lvar to_var, dir, body) in
        [from_binding; to_binding; len_binding], for_
  in
  bindings, len_var, for_

(* Generate code to iterate over a comprehension block, along with some
   initialising bindings.  The bindings will also bind the given
   [length_var] ident to the total number of iterations in the
   block. *)
let iterate_arr_block ~transl_exp ~loc ~scopes
      {clauses; guard} length_var body =
  let body =
    match guard with
    | None -> body
    | Some guard ->
      Lifthenelse(transl_exp ~scopes guard, body, lambda_unit, Pintval)
  in
  let body, length_opt, bindings =
    List.fold_left
      (fun (body, length, bindings) clause ->
         let new_bindings, new_length_var, body =
           transl_arr_clause ~transl_exp ~scopes ~loc clause body
         in
         let rev_bindings = new_bindings @ bindings in
         let length =
           match length with
           | None -> Lvar new_length_var
           | Some length ->
               Lprim(Pmulint, [Lvar new_length_var; length], loc)
         in
         body, Some length, rev_bindings)
      (body, None, []) clauses
  in
  let length = Option.value length_opt ~default:(int 0) in
  let length_binding = binding Alias Pintval length_var length in
  let bindings = List.append bindings [length_binding] in
  bindings, body

let make_array_prim ~loc size init =
  let prim =
    Primitive.simple ~name:"caml_make_vect" ~arity:2 ~alloc:true
  in
  Lprim (Pccall prim, [size; init], loc)

let make_floatarray_prim ~loc size =
  let prim =
    Primitive.simple ~name:"caml_make_float_vect" ~arity:1 ~alloc:true
  in
  Lprim (Pccall prim, [size], loc)

let blit_array_prim ~loc src src_pos dst dst_pos len =
  let prim_blit_arr =
    Primitive.simple ~name:"caml_array_blit" ~arity:5 ~alloc:true
  in
  Lprim (Pccall prim_blit_arr, [src; src_pos; dst; dst_pos; len], loc)

(* Generate binding to make an "uninitialized" array *)
let make_array ~loc ~kind ~size ~array =
  match kind with
  | Pgenarray ->
      (* This array can be Immutable since it is empty and will later be
         replaced when an example value (to create the array) is known.
         That is also why the biding is a Variable. *)
      let init = Lprim(Pmakearray(Pgenarray, Immutable, alloc_heap), [], loc) in
      binding Variable Pgenval array init
  | Pintarray | Paddrarray ->
      let init = make_array_prim ~loc size (int 0) in
      binding Strict Pgenval array init
  | Pfloatarray ->
      let init = make_floatarray_prim ~loc size in
      binding Strict Pgenval array init

(* Generate code to initialise an element of an "uninitialised" array *)
let init_array_elem ~loc ~kind ~size ~array ~index ~value =
  let set_elem =
    Lprim(Parraysetu kind, [Lvar array; Lvar index; Lvar value], loc)
  in
  match kind with
  | Pgenarray ->
      let is_first_iteration =
        Lprim(Pintcomp Ceq, [Lvar index; int 0], loc)
      in
      let make_array =
        Lassign(array, make_array_prim ~loc size (Lvar value))
      in
      Lifthenelse(is_first_iteration, make_array, set_elem, Pintval)
  | Pintarray | Paddrarray | Pfloatarray -> set_elem

(* Generate code to blit elements into an "uninitialised" array *)
let init_array_elems ~loc ~kind ~size ~array ~index ~src ~len =
  let blit =
    blit_array_prim ~loc (Lvar src) (int 0) (Lvar array) (Lvar index) (Lvar len)
  in
  match kind with
  | Pgenarray ->
      let is_first_iteration =
        Lprim(Pintcomp Ceq, [Lvar index; int 0], loc)
      in
      let is_not_empty =
        Lprim(Pintcomp(Cne), [Lvar len; int 0], loc)
      in
      let first_elem =
        Lprim(Parrayrefu kind, [Lvar src; int 0], loc)
      in
      let make_array =
        Lassign(array, make_array_prim ~loc size first_elem)
      in
      Lsequence(
        Lifthenelse(is_first_iteration,
          Lifthenelse(is_not_empty, make_array, lambda_unit, Pintval),
          lambda_unit, Pintval),
        blit)
  | Pintarray | Paddrarray | Pfloatarray -> blit

(* Binding for a counter *)
let make_counter counter =
  binding Variable Pintval counter (int 0)

(* Code to increment a counter *)
let increment_counter ~loc counter step =
  Lassign(counter, Lprim(Paddint, [Lvar counter; step], loc))

type block_lambda =
  | Without_size of { body : lambda }
  | With_size of { body : lambda; raise_count: int }

let transl_arr_block ~transl_exp ~loc ~scopes
          global_counter body array_kind value_kind block =
  let length_var = Ident.create_local "len" in
  let size =
    match body with
    | Without_size _ -> Lvar length_var
    | With_size _ -> Lprim(Pmulint, [Lvar length_var; int 2], loc)
  in
  let result_array_var = Ident.create_local "arr" in
  let result_array_binding =
    make_array ~loc ~kind:array_kind ~size ~array:result_array_var
  in
  let counter_var = Ident.create_local "counter" in
  let counter_binding = make_counter counter_var in
  let elem_var = Ident.create_local "elem" in
  let init_elem =
    init_array_elem ~loc ~kind:array_kind ~size
      ~array:result_array_var ~index:counter_var ~value:elem_var
  in
  let set_result =
    match body with
    | Without_size {body} ->
        Llet(Strict, value_kind, elem_var, body,
          Lsequence(init_elem, increment_counter ~loc counter_var (int 1)))
    | With_size {body; raise_count} ->
        let elem_len_var = Ident.create_local "len" in
        let set_len =
          Lprim(Parraysetu Paddrarray,
            [Lvar result_array_var;
             Lprim(Paddint, [Lvar counter_var; int 1], loc);
             Lvar elem_len_var], loc)
        in
        Lstaticcatch(body,
          (raise_count, [(elem_var, Pgenval); (elem_len_var, Pintval)]),
          Lsequence(init_elem, Lsequence(set_len,
             increment_counter ~loc counter_var (int 2))), Pintval)
  in
  let bindings, loops =
    iterate_arr_block ~transl_exp ~loc ~scopes block length_var set_result
  in
  let bindings =
    bindings @ [result_array_binding; counter_binding]
  in
  let body =
    match global_counter with
    | None -> loops
    | Some global_counter_var ->
      let len =
        match body with
        | Without_size _ -> Lvar counter_var
        | With_size _ -> Lprim(Pdivint Unsafe, [Lvar counter_var; int 2], loc)
      in
      Lsequence(loops, increment_counter ~loc global_counter_var len)
  in
  match block.guard with
  | None ->
      let body =
        gen_bindings bindings (Lsequence(body, Lvar result_array_var))
      in
      Without_size { body }
  | Some _ ->
      let raise_count = next_raise_count () in
      let return =
        Lstaticraise(raise_count, [Lvar result_array_var; Lvar counter_var])
      in
      let body =
        gen_bindings bindings (Lsequence(body, return))
      in
      With_size { body; raise_count }

let sub_array ~loc src src_pos len =
  let prim =
    Primitive.simple ~name:"caml_array_sub" ~arity:3 ~alloc:true
  in
  Lprim (Pccall prim, [src; src_pos; len], loc)

let transl_single_arr_block ~transl_exp ~loc ~scopes
      block body array_kind value_kind =
  let body =
    transl_arr_block ~transl_exp ~loc ~scopes None
      (Without_size {body}) array_kind value_kind block
  in
  match body with
  | Without_size { body } -> body
  | With_size { body; raise_count } ->
      let array_var = Ident.create_local "array" in
      let len_var = Ident.create_local "len" in
      Lstaticcatch(body,
          (raise_count, [(array_var, Pgenval); (len_var, Pintval)]),
          sub_array ~loc (Lvar array_var) (int 0) (Lvar len_var), Pintval)

type intermediate_array_shape =
  | Array_of_elements
  | Array_of_arrays of intermediate_array_shape
  | Array_of_filtered_arrays of intermediate_array_shape

let concat_arrays ~loc arr kind shape global_count_var =
  let res_var = Ident.create_local "res" in
  let res_binding =
    make_array ~loc ~kind ~size:(Lvar global_count_var) ~array:res_var
  in
  let counter_var = Ident.create_local "counter" in
  let counter_binding = make_counter counter_var in
  let rec loop shape arr_var len_var =
    let kind =
      match shape with
      | Array_of_elements -> kind
      | Array_of_arrays _ | Array_of_filtered_arrays _ -> Paddrarray
    in
    let len_var, bindings =
      match len_var with
      | Some var -> var, []
      | None ->
        let var = Ident.create_local "len" in
        let init = Lprim((Parraylength kind), [Lvar(arr_var)], loc) in
        let binding = binding Alias Pintval var init in
        var, [binding]
    in
    match shape with
    | Array_of_elements ->
        gen_bindings bindings
          (Lsequence(
             init_array_elems ~loc ~kind ~size:(Lvar global_count_var)
               ~array:res_var ~index:counter_var ~src:arr_var ~len:len_var,
             increment_counter ~loc counter_var (Lvar len_var)))
    | Array_of_arrays shape ->
        let index_var = Ident.create_local "index" in
        let sub_arr_var = Ident.create_local "arr" in
        let last_index = Lprim(Psubint, [Lvar len_var; int 1], loc) in
        let sub_arr =
          Lprim(Parrayrefu kind, [Lvar arr_var; Lvar index_var], loc)
        in
        gen_bindings bindings
          (Lfor(index_var, int 0, last_index, Upto,
             Llet(Strict, Pgenval, sub_arr_var, sub_arr,
               loop shape sub_arr_var None)))
    | Array_of_filtered_arrays shape ->
        let index_var = Ident.create_local "index" in
        let index_binding = make_counter index_var in
        let sub_arr_var = Ident.create_local "arr" in
        let sub_arr =
          Lprim(Parrayrefu kind, [Lvar arr_var; Lvar index_var], loc)
        in
        let sub_arr_len_var = Ident.create_local "len" in
        let sub_arr_len =
          Lprim(Parrayrefu kind,
                [Lvar arr_var; Lprim(Paddint, [Lvar index_var; int 1], loc)], loc)
        in
        gen_bindings bindings
          (gen_binding index_binding
            (Lwhile(Lprim(Pintcomp Clt, [Lvar index_var; Lvar len_var], loc),
               Lsequence(
                 Llet(Strict, Pgenval, sub_arr_var, sub_arr,
                   Llet(Strict, Pintval, sub_arr_len_var, sub_arr_len,
                     loop shape sub_arr_var (Some sub_arr_len_var))),
                 increment_counter ~loc index_var (int 2)))))
  in
  match arr with
  | Without_size { body } ->
      let array_var = Ident.create_local "array" in
      Llet(Strict, Pgenval, array_var, body,
           gen_binding res_binding
             (gen_binding counter_binding
                (Lsequence
                   (loop shape array_var None,
                    Lvar res_var))))
  | With_size { body; raise_count } ->
      let array_var = Ident.create_local "array" in
      let len_var = Ident.create_local "len" in
      Lstaticcatch(body,
          (raise_count, [(array_var, Pgenval); (len_var, Pintval)]),
           gen_binding res_binding
             (gen_binding counter_binding
                ((Lsequence
                    (loop shape array_var (Some len_var),
                     Lvar res_var)))), Pgenval)

let transl_arr_comprehension ~transl_exp ~loc ~scopes
      ~array_kind exp blocks =
  let body = transl_exp ~scopes exp in
  let value_kind = Typeopt.value_kind exp.exp_env exp.exp_type in
  match blocks with
  | [] -> assert false
  | [block] ->
      transl_single_arr_block ~transl_exp ~loc ~scopes
        block body array_kind value_kind
  | inner_block :: rest ->
      let counter_var = Ident.create_local "counter" in
      let counter_binding = make_counter counter_var in
      let body =
        transl_arr_block ~transl_exp ~loc ~scopes (Some counter_var)
          (Without_size {body}) array_kind value_kind inner_block
      in
      let shape, body =
        List.fold_left
          (fun (shape, body) block ->
             let shape =
               match body with
               | Without_size _ -> Array_of_arrays shape
               | With_size _ -> Array_of_filtered_arrays shape
             in
             let body =
               transl_arr_block ~transl_exp ~loc ~scopes None
                 body Paddrarray Pgenval block
             in
             shape, body)
          (Array_of_elements, body) rest
      in
      gen_binding counter_binding
        (concat_arrays ~loc body array_kind shape counter_var)

let from_to_comp_prim ~dir=
  let function_name = match dir with
    | Upto ->  "map_from_to_cons"
    | Downto -> "map_from_downto_cons"
  in
  Lambda.transl_prim "CamlinternalComprehension" function_name

let in_comp_prim () = Lambda.transl_prim "CamlinternalComprehension" "map_cons"

let comp_rev () = Lambda.transl_prim "CamlinternalComprehension" "rev"

let transl_list_comp type_comp body acc_var mats ~transl_exp ~scopes ~loc =
  let new_acc = Ident.create_local "acc" in
  let param, pval, args, func, body, mats =
    match type_comp with
    | From_to (param, _,e2,e3, dir) ->
      let pval = Pintval in
      let from_var = Ident.create_local "from" in
      let to_var = Ident.create_local "to_" in
      let args = [Lvar(from_var); Lvar(to_var); Lvar(new_acc)] in
      let func = from_to_comp_prim ~dir in
      let mats =
        (from_var, transl_exp ~scopes e2)::(to_var, transl_exp ~scopes e3)::mats
      in
      param, pval, args, func, body, mats
    | In (pat, in_) ->
      let pat_id = Ident.create_local "pat" in
      let pval = Typeopt.value_kind pat.pat_env pat.pat_type in
      let in_var = Ident.create_local "in_var" in
      let args = [Lvar(in_var); Lvar(new_acc)] in
      let func = in_comp_prim () in
      let body =
        Matching.for_let ~scopes pat.pat_loc (Lvar(pat_id)) pat pval body
      in
      let mats = (in_var, transl_exp ~scopes in_)::mats in
      pat_id , pval, args, func, body, mats
  in
  let fn =
    Lfunction {kind = Curried {nlocal=0};
              params= [param, pval; acc_var, Pgenval];
              return = Pgenval;
              attr = default_function_attribute;
              loc = loc;
              body = body;
              mode = alloc_heap;
              region = true}
  in
  Lapply{
    ap_loc=loc;
    ap_region_close=Rc_normal;
    ap_mode=alloc_heap;
    ap_func=func;
    ap_args= fn::args;
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inlined;
    ap_specialised=Default_specialise;
    ap_probe=None;
  }, new_acc, mats

let transl_list_comprehension ~transl_exp ~loc ~scopes body blocks =
  let acc_var = Ident.create_local "acc" in
  let value_kind = Typeopt.value_kind body.exp_env body.exp_type in
  let bdy =
    Lprim(
      Pmakeblock(0, Immutable, None, alloc_heap),
      [(transl_exp ~scopes  body); Lvar(acc_var)], loc)
  in
  let res_list, res_var = List.fold_left
    (fun (body, acc_var)  block ->
      let body =
        match block.guard with
        | None -> body
        | Some guard ->
          Lifthenelse((transl_exp ~scopes  guard), body, Lvar(acc_var), value_kind)
      in
      let body, acc_var, materialize =
        List.fold_left
          (fun (body, acc_var, mats) el ->
            transl_list_comp ~transl_exp ~scopes ~loc el body acc_var mats)
          (body, acc_var, []) block.clauses
        in
        let body = List.fold_right (fun (id, arr) body ->
          Llet(Strict, Pgenval, id, arr, body))
          materialize body
        in
        body, acc_var)
    (bdy, acc_var) blocks
  in
  Llet(Alias, Pintval, res_var, int 0, (*Empty list.*)
    Lapply{
        ap_loc=loc;
        ap_func=comp_rev ();
        ap_args=[res_list];
        ap_region_close=Rc_normal;
        ap_mode=alloc_heap;
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inlined;
        ap_specialised=Default_specialise;
        ap_probe=None;
      })
