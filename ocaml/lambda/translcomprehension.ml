open Lambda
open Typedtree
open Asttypes

type comp_block =
  | Unguarded of lambda * array_kind
  | Guarded of lambda * array_kind * int

type arrays =
  | Array_of_elements
  | Array_of_arrays of arrays
  | Array_of_filtered_arrays of arrays

let int n = Lconst (Const_base (Const_int n))

let empty_arr ~loc=
  Lprim( (Pmakearray(Pgenarray, Immutable)), [] ,loc)

let make_array size init ~loc=
  let prim_make_arr =
    Primitive.simple ~name:"caml_make_vect" ~arity:2 ~alloc:true
  in
  Lprim (Pccall prim_make_arr, [size; init], loc)

let make_array_of_kind kind size ~loc =
  match kind with
  | Pgenarray -> assert false (*An example is needed to create this array.*)
  | Pintarray -> make_array size (int 0) ~loc
  | Paddrarray -> make_array size (int 0) ~loc
  | Pfloatarray ->
    let prim_make_float_arr =
      Primitive.simple ~name:"caml_make_float_vect" ~arity:1 ~alloc:true
    in
    Lprim (Pccall prim_make_float_arr, [size], loc)

let blit_array src src_pos dst dst_pos len ~loc=
  let prim_blit_arr =
    Primitive.simple ~name:"caml_array_blit" ~arity:5 ~alloc:true
  in
  Lprim (Pccall prim_blit_arr, [src; src_pos; dst; dst_pos; len], loc)

let transl_loop ~type_comp ~body ~scopes ~loc  ~transl_exp ~mats  =
  let len_var = Ident.create_local "len_var" in
  match type_comp with
  | In (pat , e2) ->
    let in_ = transl_exp ~scopes e2 in
    let in_var = Ident.create_local "in_var" in
    let in_kind = Typeopt.array_kind e2 in
    let len = Lprim( (Parraylength(in_kind)), [Lvar(in_var)], loc) in
    let index = Ident.create_local "index" in
    let mats = (in_var, in_)::mats in
    Lfor(index, (int 0), Lprim(Psubint, [Lvar(len_var); int 1], loc) , Upto,
      Matching.for_let ~scopes pat.pat_loc
        (Lprim(Parrayrefs(in_kind),
          [Lvar(in_var); Lvar(index)], loc)) pat body), (len_var,len), mats
    
  | From_to(id, _, e2, e3, dir) ->
    let from = transl_exp ~scopes e2 in
    let to_ = transl_exp ~scopes e3 in
    let from_var = Ident.create_local "from" in 
    let to_var = Ident.create_local "to_" in 
    let mats = (from_var, from)::(to_var, to_)::mats in
    let low, high =
      match dir with
      | Upto -> Lvar(from_var), Lvar(to_var)
      | Downto -> Lvar(to_var), Lvar(from_var) in
    let len =
      Lprim(Psubint,
        [Lprim(Paddint, [high; int 1], loc);
        low], loc)
    in
    Lfor(id,Lvar(from_var), Lvar(to_var), dir, body), (len_var,len), mats

let transl_loops block base_body ~loc ~scopes  ~transl_exp  =
  List.fold_left (fun (body, lens, mats) type_comp ->
    let new_body, new_len, mats =
      transl_loop  ~transl_exp ~type_comp ~body ~scopes ~loc ~mats
    in
    new_body, new_len::lens, mats)
  (base_body, [], []) block

(*The block created here takes the result of the innerblock and writes it into
  the array of arrays (arr).*)
let transl_block global_counter  (comp_block, arrs) {clauses; guard;}
    ~loc ~scopes ~transl_exp =
  let arr = Ident.create_local "arr" in
  let counter = Ident.create_local "counter" in
  let body, res_len, array_kind =
    match comp_block with
    | Unguarded (body, Pgenarray) ->
      let res_len = Ident.create_local "res_len" in
      Lsequence(
      Lifthenelse(
        Lprim(Pintcomp(Ceq), [Lvar(counter); int 0], loc),
        Lassign(arr, make_array ~loc (Lvar(res_len)) body),
        Lprim(Parraysets(Pgenarray),
          [Lvar(arr); Lvar(counter); body] ,loc)),
      Lassign(counter, Lprim(Paddint, [Lvar(counter); int 1], loc))),
      Some res_len, Pgenarray
    | Unguarded (body, arr_kind) ->
      Lsequence(
        Lprim(Parraysets(arr_kind),[Lvar(arr); Lvar(counter); body], loc),
        Lassign(counter, Lprim(Paddint, [Lvar(counter); int 1], loc))),
      None, arr_kind
    | Guarded (bdy_and_len, arr_kind, id) ->
      let body = Ident.create_local "body" in
      let len = Ident.create_local "len" in
      Lstaticcatch(bdy_and_len, (id, [(body, Pgenval); (len, Pintval)]),
        Lsequence(
            Lsequence(
              Lprim(Parraysets(arr_kind),
                [Lvar(arr); Lvar(counter); Lvar(body)], loc),
              Lprim(Parraysets(arr_kind),
                [Lvar(arr); Lprim(Paddint, [Lvar(counter); (int 1)], loc);
                Lvar(len)], loc)),
          Lassign(counter, Lprim(Paddint, [Lvar(counter); (int 2)], loc)))),
      None, arr_kind
  in
  let body, arrs =
    match guard with
      | None ->
        body, Array_of_arrays(arrs)
      | Some guard ->
        Lifthenelse(
          (transl_exp ~scopes guard),
          body,
          lambda_unit
        ), Array_of_filtered_arrays(arrs)
  in
  let body, lengths, materialize =
    transl_loops  ~transl_exp  ~loc ~scopes clauses body in
  let block_len = Ident.create_local "block_len" in
  let body, len =
    match global_counter, comp_block with
    | Some gc, Unguarded _ ->
      Lsequence(
        body,
        Lassign(gc, Lprim(Paddint, [Lvar(counter); Lvar(gc)], loc))),
      Lvar(block_len)
    | Some gc, Guarded _ ->
      Lsequence(
        body,
        Lassign(gc, Lprim(Paddint,
          [Lvar(gc);
          Lprim(Pdivint(Unsafe), [Lvar(counter); int 2], loc)], loc))),
      Lprim(Pmulint, [Lvar(block_len); int 2], loc)
    | None, Unguarded _  -> body, Lvar(block_len)
    | None, Guarded _  -> body, Lprim(Pmulint, [Lvar(block_len); int 2], loc)
  in
  let body =
    match res_len with
    | None ->
      Llet(Strict, Pgenval, arr, make_array_of_kind ~loc array_kind len,
        Lsequence(body, Lvar(arr)))
    | Some res_len ->
      Llet(Alias, Pintval, res_len, len,
        Llet(Variable, Pgenval, arr, empty_arr ~loc,
        Lsequence(body, Lvar(arr))))
  in
  let block_len_val = List.fold_left (fun (tot_len) (len_id, _len) ->
      let new_tot_len =
        match tot_len with
        | None -> Lvar(len_id)
        | Some tot_len -> Lprim(Pmulint, [Lvar(len_id); tot_len], loc)
      in
      Some new_tot_len)
    (None) lengths
  in
  let body =
    Llet(Strict, Pintval, block_len,
      Option.value block_len_val ~default:(int 0), body)
  in
  let body = List.fold_left (fun body (len_id, len) ->
      Llet(Strict, Pintval, len_id, len, body))
    (body) lengths
  in
  let body = List.fold_right (fun (id, arr) body ->
    Llet(Strict, Pgenval, id, arr, body))
    materialize body
  in
  match guard with
  | None ->
    Unguarded(Llet(Variable, Pintval, counter, int 0, body), Paddrarray),
    arrs
  | Some _ ->
    let static_return_id = next_raise_count () in
    Guarded(
      Llet(Variable, Pintval, counter, int 0,
        Lstaticraise(static_return_id, [body; Lvar(counter)])),
      Paddrarray, static_return_id),
    arrs

let transl_concat_arrays arr arr_len arrs res_kind total_len ~loc  =
  let res = Ident.create_local "res" in
  let counter = Ident.create_local "counter" in
  let rec transl_for arr arr_len arrs=
    let i = Ident.create_local "i" in
    match arrs with
    | Array_of_elements ->
      let blit =
      Lsequence(
        blit_array arr ~loc (int 0) (Lvar(res)) (Lvar(counter)) (arr_len),
        Lassign(counter, Lprim(Paddint, [Lvar(counter); arr_len], loc)))
      in
      (*Only create the array in the first iteration if its Pgenarray.*)
      (match res_kind with
      | Pgenarray ->
        Lsequence(
        Lifthenelse(
          Lprim(Psequand, [
            Lprim(Pintcomp(Ceq), [Lvar(counter); int 0], loc);
            (*This check protects from trying to acces an empty array.*)
            Lprim(Pintcomp(Cne), [arr_len; int 0], loc)], loc),
          Lassign(res, make_array ~loc (Lvar(total_len))
            (Lprim(Parrayrefs(res_kind), [arr; int 0], loc))),
          lambda_unit),
        blit)
      | Pintarray | Pfloatarray | Paddrarray -> blit)
    | Array_of_arrays(arrs) ->
      let sub_arr = Ident.create_local "sub_arr" in
      let len =
        Lprim((Parraylength(Paddrarray)), [Lvar(sub_arr)], loc)
      in
      Lfor(i, int 0, Lprim(Psubint, [arr_len; int 1], loc), Upto,
        Llet(Strict, Pgenval, sub_arr,
          Lprim(Parrayrefs(Paddrarray), [arr; Lvar(i)], loc),
          transl_for (Lvar(sub_arr)) len arrs))
    | Array_of_filtered_arrays(arrs) ->
      let sub_arr = Ident.create_local "sub_arr" in
      let sub_arr_len = Ident.create_local "sub_arr_len" in
      Llet(Variable, Pintval, i, int 0,
      Lwhile(Lprim(Pintcomp(Clt), [Lvar(i); arr_len], loc),
        Lsequence(
            Llet(Strict, Pgenval,  sub_arr,
              Lprim(Parrayrefs(Paddrarray), [arr; Lvar(i)], loc),
              Llet(Strict, Pintval, sub_arr_len,
                Lprim(Parrayrefs(Paddrarray),
                  [arr; Lprim(Paddint, [Lvar(i); int 1], loc)],loc),
                transl_for (Lvar(sub_arr)) (Lvar(sub_arr_len)) arrs)),
          Lassign(i, Lprim(Paddint, [Lvar(i); int 2], loc))
        )))
  in
  (*Remove the outer most layer. The outermost array would always be of size
    one, thats why its always unwrapped (arr, arr_len).*)
  let arrs =
    match arrs with
    | Array_of_elements -> arrs
    | Array_of_arrays(arrs) -> arrs
    | Array_of_filtered_arrays(arrs) -> arrs
  in
  match res_kind with
  | Pgenarray ->
    Llet(Variable, Pgenval, res, empty_arr ~loc,
      Llet(Variable, Pintval, counter, (int 0),
        Lsequence(transl_for arr arr_len arrs, Lvar(res))))
  | Pintarray | Pfloatarray | Paddrarray as kind ->
    Llet(Strict, Pgenval, res, make_array_of_kind ~loc kind (Lvar(total_len)),
      Llet(Variable, Pintval, counter, (int 0),
        Lsequence(transl_for arr arr_len arrs, Lvar(res))))

let transl_arr_comprehension body blocks ~array_kind ~scopes ~loc ~transl_exp =
  (*The global counter is required when the size of the result array is not
    known in advance.*)
  let global_counter =
    match blocks with
    | [{ guard = None; _}] -> None
    | _ -> Some (Ident.create_local "global_counter")
  in
  let base_block =
    transl_block ~transl_exp ~loc ~scopes global_counter
      (Unguarded(transl_exp ~scopes body, array_kind),
        Array_of_elements)
      (List.hd blocks)
  in
  let translated_blocks, arrs =
    List.fold_left
      (fun acc el ->
        transl_block  ~transl_exp  ~loc ~scopes None acc el )
      base_block (List.tl blocks)
  in
  match translated_blocks, global_counter with
  | Unguarded(body, _kind), Some gc ->
    let arr = Ident.create_local "arr" in
    let len = Ident.create_local "len_var" in
    Llet(
      Variable, Pintval, gc, int 0,
      Llet(Strict, Pgenval, arr, body,
        Llet(Alias, Pintval, len,
          Lprim((Parraylength(Paddrarray)), [Lvar(arr)], loc),
          transl_concat_arrays ~loc
            (Lvar(arr)) (Lvar(len)) arrs array_kind gc)))
  | Unguarded(body, _kind), None -> body
  | Guarded(arr_and_len, _, id), Some gc ->
    let len = Ident.create_local "len" in
    let arr = Ident.create_local "arr" in
    Llet(
      Variable, Pintval, gc, int 0,
      Lstaticcatch(arr_and_len, (id, [(arr, Pgenval); (len, Pintval)]),
          transl_concat_arrays ~loc
            (Lvar(arr)) (Lvar(len)) arrs array_kind gc))
  | Guarded _, None -> assert false


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
    | In (pat, _in) ->
      let pat_id = Ident.create_local "pat" in
      let pval = Typeopt.value_kind pat.pat_env pat.pat_type in
      let in_var = Ident.create_local "in_var" in
      let args = [Lvar(in_var); Lvar(new_acc)] in
      let func = in_comp_prim () in
      let body =
        Matching.for_let ~scopes pat.pat_loc (Lvar(pat_id)) pat body
      in
      let mats = (in_var, transl_exp ~scopes _in)::mats in
      pat_id , pval, args, func, body, mats
  in
  let fn =
    Lfunction {kind = Curried;
              params= (param, pval)::[acc_var, Pgenval];
              return = Pgenval;
              attr = default_function_attribute;
              loc = loc;
              body = body}
  in
  Lapply{
    ap_loc=loc;
    ap_func=func;
    ap_args= fn::args;
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inlined;
    ap_specialised=Default_specialise;
    ap_probe=None;
  }, new_acc, mats

let transl_list_comprehension body blocks ~scopes ~loc ~transl_exp  =
  let acc_var = Ident.create_local "acc" in
  let bdy =
    Lprim(
      Pmakeblock(0, Immutable, None),
      [(transl_exp ~scopes  body); Lvar(acc_var)], loc)
  in
  let res_list, res_var = List.fold_left
    (fun (body, acc_var)  block ->
      let body =
        match block.guard with
        | None -> body
        | Some guard ->
          Lifthenelse((transl_exp ~scopes  guard), body, Lvar(acc_var))
      in
      let body, acc_var, materialize =
        List.fold_left
          (fun (body, acc_var, mats) el ->
            transl_list_comp ~transl_exp ~scopes ~loc el body acc_var mats)
          (body, acc_var, []) block.clauses
        in
        let body = List.fold_right (fun (id, arr) body ->
          Llet(Strict, Pgenval, id, arr, body))
          materialize (body) 
        in
        body, acc_var)
    (bdy, acc_var) blocks
  in
  Llet(Alias, Pgenval, res_var, int 0, (*Empty list.*)
    Lapply{
        ap_loc=loc;
        ap_func=comp_rev ();
        ap_args=[res_list];
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inlined;
        ap_specialised=Default_specialise;
        ap_probe=None;
      })
