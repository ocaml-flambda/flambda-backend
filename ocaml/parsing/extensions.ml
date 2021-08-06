open Parsetree

type extension_expr =
| Eexp_list_comprehension of expression * comprehension list
| Eexp_arr_comprehension of  expression * comprehension list

and comprehension =
{
  clauses: comprehension_clause list;
  guard : expression option
}

and comprehension_clause =
(*[ body for i = E2 to E3 ]      (flag = Upto)
 [ body for i = E2 downto E3 ]  (flag = downto)*)
| From_to of pattern * expression *
   expression * Asttypes.direction_flag
(*[ body for i in E2 ]      *)
| In of pattern * expression


type error =
  | Extension_not_existent
  | Illegal_comprehension_extension_construct

exception Error of Location.t * error


let structure_item_of_expr_desc ~loc expr_desc =
  {
    pstr_desc=Pstr_eval(
      {
        pexp_desc=expr_desc;
        pexp_loc=loc;
        pexp_loc_stack=[];
        pexp_attributes=[];
      }, []);
    pstr_loc=loc;
  }

let structure_item_of_expr ~loc expr =
  {
    pstr_desc=Pstr_eval(expr, []);
    pstr_loc=loc;
  }

let map_comprehension ~loc extension_name body comp_list : extension=
  let list =
    (List.map (fun {clauses; guard}  ->
      let clauses =
        List.map (fun comp_type ->
          let expr_desc =
          match comp_type with
          | From_to (p, e2, e3, dir) ->
            Pexp_for(p, e2, e3, dir, e2 (*This is arbitrary and unused.*))
          | In (p, e2) ->
            Pexp_let(Nonrecursive,
              [{
                pvb_pat=p;
                pvb_expr=e2;
                pvb_attributes=[];
                pvb_loc=loc;
              }], e2 (*This is arbitrary and unused.*))
          in
          structure_item_of_expr_desc ~loc expr_desc
        ) clauses
      in
      let extension : extension =
        match guard with
        | None ->
          let payload = PStr(clauses) in
          { txt="block"; loc; }, payload
        | Some guard ->
          let payload = PStr((structure_item_of_expr ~loc guard)::clauses) in
          { txt="guarded_block"; loc; }, payload
      in
      structure_item_of_expr_desc ~loc (Pexp_extension(extension))
    ) comp_list)
  in
  let payload = PStr((structure_item_of_expr ~loc body)::list) in
  { txt=extension_name; loc; }, payload

let unwrap_expression ~loc = function
| Pstr_eval(exp, _) -> exp
| _ -> raise(Error(loc, Illegal_comprehension_extension_construct))

let unwrap_extension ~loc = function
| Pexp_extension(extension) -> extension
| _ -> raise(Error(loc, Illegal_comprehension_extension_construct))

let unwrap_structure ~loc = function
| PStr(structure) -> structure
| _ -> raise(Error(loc, Illegal_comprehension_extension_construct))

let unmap_comprehension ~loc payload =
  let str = unwrap_structure ~loc payload in
  let body = unwrap_expression ~loc ((List.hd str).pstr_desc) in
  let str = List.tl str in
  let comp = List.map (fun {pstr_desc; pstr_loc=_;}  ->
    let name, payload =
      unwrap_extension ~loc (unwrap_expression ~loc pstr_desc).pexp_desc
    in
    let str = unwrap_structure ~loc payload in
    let str, guard =
      match name.txt with
      | "block" ->  str, None
      | "guarded_block" ->
        let guard = unwrap_expression ~loc ((List.hd str).pstr_desc) in
        let str = List.tl str in
        str, Some guard
      | _ -> raise(Error(loc, Illegal_comprehension_extension_construct))
    in
    let clauses =
      List.map (fun {pstr_desc; pstr_loc=_;}  ->
          match (unwrap_expression ~loc pstr_desc).pexp_desc with
          | Pexp_for(p, e2, e3, dir, _) -> From_to (p, e2, e3, dir)
          | Pexp_let(Nonrecursive,
          [{
            pvb_pat=p;
            pvb_expr=e2;
            pvb_attributes=_;
            pvb_loc=_;
          }], _) ->  In (p, e2)
          | _ -> raise(Error(loc, Illegal_comprehension_extension_construct))
        ) str
    in
    { clauses; guard; }

  ) str
  in
  body, comp

let payload_of_extension_expr ~loc = function
  | Eexp_list_comprehension(body, comp_list) ->
      map_comprehension ~loc "extension.list_comprehension" body comp_list
  | Eexp_arr_comprehension(body, comp_list) ->
      map_comprehension ~loc "extension.arr_comprehension" body comp_list

let extension_expr_of_payload ~loc ((name, payload) : extension) =
  match name.txt with
  | "extension.list_comprehension" ->
    let body, comp = unmap_comprehension ~loc payload in
    Eexp_list_comprehension(body, comp)
  | "extension.arr_comprehension" ->
    let body, comp = unmap_comprehension ~loc payload in
    Eexp_arr_comprehension(body, comp)
  | _ -> raise(Error(loc, Extension_not_existent))


let report_error ~loc = function
  | Extension_not_existent ->
    Location.errorf ~loc "Extension does not exsist."
  | Illegal_comprehension_extension_construct ->
    Location.errorf ~loc "Wrong extension sytax for comprehensions."

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (report_error ~loc err)
      | _ ->
        None
    )
