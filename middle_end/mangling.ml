(* The MIT License
 *
 * Copyright (c) 2021--2022 Jane Street Group, LLC opensource@janestreet.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

[@@@ocaml.warning "-69"]

module String = Misc.Stdlib.String

let escape_symbols part =
  let buf = Buffer.create 16 in
  let was_hex_last = ref false in
  let handle_char = function
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c ->
      if !was_hex_last then Buffer.add_string buf "__";
      Buffer.add_char buf c;
      was_hex_last := false
    | c ->
      Printf.bprintf buf "%sX%02x"
        (if !was_hex_last then "_" else "__")
        (Char.code c);
      was_hex_last := true
  in
  String.iter handle_char part;
  Buffer.contents buf

type expression =
  | String of string
  | Dot of expression * string

type cpp_name =
  | Simple of string
  | Scoped of cpp_name list
  | Templated of string * template_arg list

and template_arg =
  | Cpp_name of cpp_name
  | Expression of expression

let mangle_cpp name =
  let with_length s =
    let s = escape_symbols s in
    Printf.sprintf "%d%s" (String.length s) s
  in
  let rec mangle_expression = function
    | String s -> with_length s
    | Dot (e, name) ->
      Printf.sprintf "dt%s%s" (mangle_expression e) (with_length name)
  in
  let rec mangle_name = function
    | Simple s -> with_length s
    | Scoped names ->
      let s = List.map mangle_name names |> String.concat "" in
      Printf.sprintf "N%sE" s
    | Templated (str, parts) ->
      let s = List.map mangle_arg parts |> String.concat "" in
      Printf.sprintf "%sI%sE" (with_length str) s
  and mangle_arg = function
    | Cpp_name name -> mangle_name name
    | Expression expression ->
      Printf.sprintf "X%sE" (mangle_expression expression)
  in
  "_Z" ^ mangle_name name

let file_template_arg file =
  (* Take the base name only *)
  let filename = Filename.basename file in
  match String.split_on_char '.' filename with
  | [] -> Misc.fatal_error "Empty split"
  | hd :: tl ->
    let expr = List.fold_left (fun e x -> Dot (e, x)) (String hd) tl in
    Expression expr

let name_op = function
  | "+" -> "PLUS"
  | "++" -> "PLUSPLUS"
  | "+." -> "PLUSDOT"
  | "+=" -> "PLUSEQ"
  | "-" -> "MINUS"
  | "-." -> "MINUSDOT"
  | "*" -> "STAR"
  | "%" -> "PERCENT"
  | "=" -> "EQUAL"
  | "<" -> "LESS"
  | ">" -> "GREATER"
  | "<>" -> "NOTEQUAL"
  | "||" -> "BARBAR"
  | "&" -> "AMPERSAND"
  | "&&" -> "AMPERAMPER"
  | ":=" -> "COLONEQUAL"
  | "^" -> "CARET"
  | "^^" -> "CARETCARET"
  | "@" -> "AT"
  | "<<" -> "LSHIFT"
  | ">>" -> "RSHIFT"
  | op -> op

let build_location_info loc =
  let loc = Debuginfo.Scoped_location.to_location loc in
  let file, line, startchar = Location.get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in
  let line_str = Printf.sprintf "ln_%d" line in
  let info = [file_template_arg file; Cpp_name (Simple line_str)] in
  if startchar >= 0
  then
    let char_str = Printf.sprintf "ch_%d_to_%d" startchar endchar in
    info @ [Cpp_name (Simple char_str)]
  else info

(* OCaml names can contain single quotes but need to be escaped for C++
   identifiers. *)
let convert_identifier str =
  match String.split_on_char '\'' str with
  | [] -> Misc.fatal_error "empty split"
  | [s] -> Simple s
  | parts ->
    let s = String.concat "_Q" parts in
    Templated (s, [Cpp_name (Simple "quoted")])

let convert_closure_id id loc =
  if String.begins_with id ~prefix:"anon_fn["
  then
    (* Keep the unique integer stamp *)
    let _init, stamp = String.split_last_exn id ~split_on:'_' in
    (* Put the location inside C++ template args *)
    Templated ("anon_fn_" ^ stamp, build_location_info loc)
  else
    match id.[0] with
    (* A regular identifier *)
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> convert_identifier id
    (* An operator *)
    | _op ->
      let op, stamp = String.split_last_exn id ~split_on:'_' in
      Templated ("op_" ^ stamp, [Cpp_name (Simple (name_op op))])

let convert_scope scope =
  let n = String.length scope in
  (* anonymous function *)
  if String.equal scope "(fun)"
  then Templated ("anon_fn", []) (* operators *)
  else if n > 2 && String.get scope 0 = '(' && String.get scope (n - 1) = ')'
  then
    let op = String.sub scope 1 (n - 2) in
    Templated ("op", [Cpp_name (Simple (name_op op))])
    (* regular identifiers *)
  else convert_identifier scope

let list_of_scopes scopes =
  (* Works for now since the only separators are '.' and '#' *)
  let scope_str = Debuginfo.Scoped_location.string_of_scopes scopes in
  String.split_on_chars scope_str ~split_on:['.'; '#']

let scope_matches_closure_id scope closure_id =
  (* If the `id` is an anonymous function this corresponds to that, and, even if
     not, then the function has likely been given a name via some aliasing (e.g.
     `let f = fun x -> ...`) *)
  String.equal scope "(fun)"
  (* Normal case where closure id and scope match directly *)
  || String.begins_with closure_id ~prefix:scope
  || (* For operators, the scope is wrapped in parens *)
  String.length scope >= 3
  && String.begins_with closure_id
       ~prefix:(String.sub scope 1 (String.length scope - 2))

(* Returns a pair of the top-level module and the list of scopes that strictly
   contain the closure id *)
let module_and_scopes ~unitname loc id =
  match (loc : Debuginfo.Scoped_location.t) with
  | Loc_known { loc = _; scopes } -> (
    let scopes = list_of_scopes scopes in
    (* Remove last scope if it matches closure id *)
    let scopes =
      match List.rev scopes with
      | [] -> Misc.fatal_errorf "No location - %s %s" unitname id
      | last_scope :: rest when scope_matches_closure_id last_scope id ->
        List.rev rest
      | _ -> scopes
    in
    (* If the scope is now empty, use the unitname as the top-level module *)
    match scopes with
    | [] -> unitname, []
    | top_module :: sub_scopes -> top_module, sub_scopes)
  | Loc_unknown -> unitname, []

let remove_prefix ~prefix str =
  let n = String.length prefix in
  if String.begins_with str ~prefix
  then String.sub str n (String.length str - n)
  else str

let fun_symbol ~unitname ~loc ~id =
  let unitname = remove_prefix ~prefix:"caml" unitname in
  let top_level_module, sub_scopes = module_and_scopes ~unitname loc id in
  let namespace_parts name =
    String.split_on_string name ~split_on:"__"
    |> List.map (fun part -> Simple part)
  in
  let parts =
    List.concat
      [ namespace_parts top_level_module;
        List.map convert_scope sub_scopes;
        [convert_closure_id id loc];
        (if String.equal top_level_module unitname
        then []
        else
          [ Templated
              ("inlined_in", [Cpp_name (Scoped (namespace_parts unitname))]) ])
      ]
  in
  mangle_cpp (Scoped parts)
