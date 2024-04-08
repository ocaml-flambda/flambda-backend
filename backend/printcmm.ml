(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pretty-printing of C-- code *)

open Format
open Cmm

module V = Backend_var
module VP = Backend_var.With_provenance
open Location_tracker_formatter

let with_location_mapping ?label ~dbg ppf f =
  with_location_mapping ?label ~loc:(Debuginfo.to_location dbg) ppf f

let rec_flag ppf = function
  | Nonrecursive -> ()
  | Recursive -> fprintf ppf " rec"

let machtype_component ppf = function
  | Val -> fprintf ppf "val"
  | Addr -> fprintf ppf "addr"
  | Int -> fprintf ppf "int"
  | Float -> fprintf ppf "float"
  | Vec128 -> fprintf ppf "vec128"

let machtype ppf mty =
  match Array.length mty with
  | 0 -> fprintf ppf "unit"
  | n -> machtype_component ppf mty.(0);
         for i = 1 to n-1 do
           fprintf ppf "*%a" machtype_component mty.(i)
         done

let exttype ppf = function
  | XInt -> fprintf ppf "int"
  | XInt32 -> fprintf ppf "int32"
  | XInt64 -> fprintf ppf "int64"
  | XFloat -> fprintf ppf "float"
  | XFloat32 -> fprintf ppf "float32"
  | XVec128 -> fprintf ppf "vec128"

let extcall_signature ppf (ty_res, ty_args) =
  begin match ty_args with
  | [] -> ()
  | ty_arg1 :: ty_args ->
      exttype ppf ty_arg1;
      List.iter (fun ty -> fprintf ppf ",%a" exttype ty) ty_args
  end;
  begin match ty_res with
  | None ->
      fprintf ppf "->."
  | Some ty_res ->
      fprintf ppf "->%a" machtype ty_res
  end

let is_global ppf = function
  | Global -> fprintf ppf "G"
  | Local -> fprintf ppf "L"

let symbol ppf s =
  fprintf ppf "%a:\"%s\"" is_global s.sym_global s.sym_name

let integer_comparison = function
  | Ceq -> "=="
  | Cne -> "!="
  | Clt -> "<"
  | Cle -> "<="
  | Cgt -> ">"
  | Cge -> ">="

let float_comparison = function
  | CFeq -> "=="
  | CFneq -> "!="
  | CFlt -> "<"
  | CFnlt -> "!<"
  | CFle -> "<="
  | CFnle -> "!<="
  | CFgt -> ">"
  | CFngt -> "!>"
  | CFge -> ">="
  | CFnge -> "!>="

let chunk = function
  | Byte_unsigned -> "unsigned int8"
  | Byte_signed -> "signed int8"
  | Sixteen_unsigned -> "unsigned int16"
  | Sixteen_signed -> "signed int16"
  | Thirtytwo_unsigned -> "unsigned int32"
  | Thirtytwo_signed -> "signed int32"
  | Onetwentyeight_unaligned -> "unaligned vec128"
  | Onetwentyeight_aligned -> "aligned vec128"
  | Word_int -> "int"
  | Word_val -> "val"
  | Single -> "float32"
  | Double -> "float64"

let atomic_bitwidth : Cmm.atomic_bitwidth -> string = function
  | Word -> "int"
  | Thirtytwo -> "int32"
  | Sixtyfour -> "int64"

let temporal_locality = function
  | Nonlocal -> "nonlocal"
  | Low -> "low"
  | Moderate -> "moderate"
  | High -> "high"

let atomic_op = function
  | Fetch_and_add -> "fetch_and_add"
  | Compare_and_swap -> "compare_and_swap"

let phantom_defining_expr ppf defining_expr =
  match defining_expr with
  | Cphantom_const_int i -> Targetint.print ppf i
  | Cphantom_const_symbol sym -> Format.pp_print_string ppf sym
  | Cphantom_var var -> V.print ppf var
  | Cphantom_offset_var { var; offset_in_words; } ->
    Format.fprintf ppf "%a+(%d)" V.print var offset_in_words
  | Cphantom_read_field { var; field; } ->
    Format.fprintf ppf "%a[%d]" V.print var field
  | Cphantom_read_symbol_field { sym; field; } ->
    Format.fprintf ppf "%s[%d]" sym field
  | Cphantom_block { tag; fields; } ->
    Format.fprintf ppf "[%d: " tag;
    List.iter (fun field ->
        Format.fprintf ppf "%a; " V.print field)
      fields;
    Format.fprintf ppf "]"

let phantom_defining_expr_opt ppf defining_expr =
  match defining_expr with
  | None -> Format.pp_print_string ppf "()"
  | Some defining_expr -> phantom_defining_expr ppf defining_expr

let location d =
  if not !Clflags.locations then ""
  else Debuginfo.to_string d

let exit_label ppf = function
  | Return_lbl -> fprintf ppf "*return*"
  | Lbl lbl -> fprintf ppf "%d" lbl

let trap_action ppf ta =
  match ta with
  | Push i -> fprintf ppf "push(%d)" i
  | Pop i -> fprintf ppf "pop(%d)" i

let trap_action_list ppf traps =
  match traps with
  | [] -> ()
  | t :: rest ->
      fprintf ppf "<%a" trap_action t;
      List.iter (fun t -> fprintf ppf " %a" trap_action t) rest;
      fprintf ppf ">"

let to_string msg =
  let b = Buffer.create 17 in
  let ppf = Format.formatter_of_buffer b in
  Format.kfprintf (fun ppf ->
    Format.pp_print_flush ppf ();
    Buffer.contents b
  ) ppf msg

let operation d = function
  | Capply(_ty, _) -> "app" ^ location d
  | Cextcall { func = lbl; _ } ->
      Printf.sprintf "extcall \"%s\"%s" lbl (location d)
  | Cload {memory_chunk; mutability} -> (
      match mutability with
      | Asttypes.Immutable -> Printf.sprintf "load %s" (chunk memory_chunk)
      | Asttypes.Mutable   -> Printf.sprintf "load_mut %s" (chunk memory_chunk))
  | Calloc Alloc_heap -> "alloc" ^ location d
  | Calloc Alloc_local -> "alloc_local" ^ location d
  | Cstore (c, init) ->
    let init =
      match init with
      | Initialization -> "(init)"
      | Assignment -> ""
    in
    Printf.sprintf "store %s%s" (chunk c) init
  | Caddi -> "+"
  | Csubi -> "-"
  | Cmuli -> "*"
  | Cmulhi { signed } -> "*h"^(if signed then "" else "u")
  | Cdivi -> "/"
  | Cmodi -> "mod"
  | Cand -> "and"
  | Cor -> "or"
  | Cxor -> "xor"
  | Clsl -> "<<"
  | Clsr -> ">>u"
  | Casr -> ">>s"
  | Cbswap { bitwidth = Sixteen } -> "bswap_16"
  | Cbswap { bitwidth = Thirtytwo } -> "bswap_32"
  | Cbswap { bitwidth = Sixtyfour } -> "bswap_64"
  | Cclz { arg_is_non_zero; } -> Printf.sprintf "clz %B" arg_is_non_zero
  | Cctz { arg_is_non_zero; } -> Printf.sprintf "ctz %B" arg_is_non_zero
  | Cpopcnt -> "popcnt"
  | Ccmpi c -> integer_comparison c
  | Caddv -> "+v"
  | Cadda -> "+a"
  | Ccmpa c -> Printf.sprintf "%sa" (integer_comparison c)
  | Cnegf -> "~f"
  | Cabsf -> "absf"
  | Caddf -> "+f"
  | Csubf -> "-f"
  | Cmulf -> "*f"
  | Cdivf -> "/f"
  | Ccsel ret_typ ->
    to_string "csel %a" machtype ret_typ
  | Cvalueofint -> "valueofint"
  | Cintofvalue -> "intofvalue"
  | Cvectorcast Bits128 ->
    Printf.sprintf "vec128->vec128"
  | Cscalarcast Float_to_int -> "float->int"
  | Cscalarcast Float_of_int -> "int->float"
  | Cscalarcast (V128_to_scalar ty) ->
    Printf.sprintf "%s->scalar" (Primitive.vec128_name ty)
  | Cscalarcast (V128_of_scalar ty) ->
    Printf.sprintf "scalar->%s" (Primitive.vec128_name ty)
  | Ccmpf c -> Printf.sprintf "%sf" (float_comparison c)
  | Craise k -> Lambda.raise_kind k ^ location d
  | Cprobe { name; handler_code_sym; enabled_at_init; } ->
    Printf.sprintf "probe[%s %s%s]" name handler_code_sym
      (if enabled_at_init then " enabled_at_init" else "")
  | Cprobe_is_enabled {name} -> Printf.sprintf "probe_is_enabled[%s]" name
  | Cprefetch { is_write; locality; } ->
    Printf.sprintf "prefetch is_write=%b prefetch_temporal_locality_hint=%s"
      is_write (temporal_locality locality)
  | Catomic { op; size = _ } -> Printf.sprintf "atomic %s" (atomic_op op)
  | Copaque -> "opaque"
  | Cbeginregion -> "beginregion"
  | Cendregion -> "endregion"
  | Ctuple_field (field, _ty) ->
    to_string "tuple_field %i" field
  | Cdls_get -> "dls_get"

let rec expr ppf = function
  | Cconst_int (n, _dbg) -> fprintf ppf "%i" n
  | Cconst_natint (n, _dbg) ->
    fprintf ppf "%s" (Nativeint.to_string n)
  | Cconst_vec128 ({low; high}, _dbg) -> fprintf ppf "%016Lx:%016Lx" high low
  | Cconst_float (n, _dbg) -> fprintf ppf "%F" n
  | Cconst_symbol (s, _dbg) -> fprintf ppf "%a:\"%s\"" is_global s.sym_global s.sym_name
  | Cvar id -> V.print ppf id
  | Clet(id, def, (Clet(_, _, _) as body)) ->
      let print_binding id ppf def =
        fprintf ppf "@[<2>%a@ %a@]"
          VP.print id expr def in
      let rec in_part ppf = function
        | Clet(id, def, body) ->
            fprintf ppf "@ %a" (print_binding id) def;
            in_part ppf body
        | exp -> exp in
      fprintf ppf "@[<2>(let@ @[<1>(%a" (print_binding id) def;
      let exp = in_part ppf body in
      fprintf ppf ")@]@ %a)@]" sequence exp
  | Clet(id, def, body) ->
     fprintf ppf
      "@[<2>(let@ @[<2>%a@ %a@]@ %a)@]"
      VP.print id expr def sequence body
  | Clet_mut(id, kind, def, body) ->
    fprintf ppf
      "@[<2>(let_mut@ @[<2>%a: %a@ %a@]@ %a)@]"
      VP.print id machtype kind expr def sequence body
  | Cphantom_let(var, def, (Cphantom_let(_, _, _) as body)) ->
      let print_binding var ppf def =
        fprintf ppf "@[<2>%a@ %a@]" VP.print var
          phantom_defining_expr_opt def
      in
      let rec in_part ppf = function
        | Cphantom_let(var, def, body) ->
            fprintf ppf "@ %a" (print_binding var) def;
            in_part ppf body
        | exp -> exp in
      fprintf ppf "@[<2>(let?@ @[<1>(%a" (print_binding var) def;
      let exp = in_part ppf body in
      fprintf ppf ")@]@ %a)@]" sequence exp
  | Cphantom_let(var, def, body) ->
    fprintf ppf
      "@[<2>(let?@ @[<2>%a@ %a@]@ %a)@]"
      VP.print var
      phantom_defining_expr_opt def
      sequence body
  | Cassign(id, exp) ->
      fprintf ppf "@[<2>(assign @[<2>%a@ %a@])@]" V.print id expr exp
  | Ctuple el ->
      let tuple ppf el =
       let first = ref true in
       List.iter
        (fun e ->
          if !first then first := false else fprintf ppf "@ ";
          expr ppf e)
        el in
      fprintf ppf "@[<1>[%a]@]" tuple el
  | Cop(op, el, dbg) ->
      with_location_mapping ~label:"Cop" ~dbg ppf (fun () ->
      fprintf ppf "@[<2>(%s" (operation dbg op);
      List.iter (fun e -> fprintf ppf "@ %a" expr e) el;
      begin match op with
      | Capply(mty, _) -> fprintf ppf "@ %a" machtype mty
      | Cextcall { ty; ty_args; alloc = _; func = _; returns; } ->
        let ty = if returns then Some ty else None in
        fprintf ppf "@ %a" extcall_signature (ty, ty_args)
      | _ -> ()
      end;
      fprintf ppf ")@]")
  | Csequence(e1, e2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" sequence e1 sequence e2
  | Cifthenelse(e1, e2_dbg, e2, e3_dbg, e3, dbg, _kind) ->
      with_location_mapping ~label:"Cifthenelse-e1" ~dbg ppf (fun () ->
      fprintf ppf "@[<2>(if@ %a@ " expr e1;
      with_location_mapping ~label:"Cifthenelse-e2" ~dbg:e2_dbg ppf (fun () ->
      fprintf ppf "%a@ " expr e2);
      with_location_mapping ~label:"Cifthenelse-e3" ~dbg:e3_dbg ppf (fun () ->
      fprintf ppf "%a" expr e3);
      fprintf ppf ")@]")
  | Cswitch(e1, index, cases, dbg, _kind) ->
      with_location_mapping ~label:"Cswitch" ~dbg ppf (fun () ->
      let print_case i ppf =
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %i:" j
        done in
      let print_cases ppf =
       for i = 0 to Array.length cases - 1 do
        fprintf ppf "@ @[<2>%t@ %a@]" (print_case i) sequence (fst cases.(i))
       done in
      fprintf ppf "@[<v 0>@[<2>(switch@ %a@ @]%t)@]" expr e1 print_cases)
  | Ccatch(flag, handlers, e1, _kind) ->
      let print_handler ppf (i, ids, e2, dbg, is_cold) =
        with_location_mapping ~label:"Ccatch-handler" ~dbg ppf (fun () ->
        fprintf ppf "(%d%a)%s@ %a"
          i
          (fun ppf ids ->
             List.iter
               (fun (id, ty) ->
                 fprintf ppf "@ %a: %a"
                   VP.print id machtype ty)
               ids) ids
          (if is_cold then "(cold)" else "")
          sequence e2)
      in
      let print_handlers ppf l =
        List.iter (print_handler ppf) l
      in
      fprintf ppf
        "@[<2>(catch%a@ %a@;<1 -2>with%a)@]"
        rec_flag flag
        sequence e1
        print_handlers handlers
  | Cexit (i, el, traps) ->
      fprintf ppf "@[<2>(exit%a %a" trap_action_list traps exit_label i;
      List.iter (fun e -> fprintf ppf "@ %a" expr e) el;
      fprintf ppf ")@]"
  | Ctrywith(e1, exn_cont, id, e2, dbg, _value_kind) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -2>with(%d)@ %a@ "
            sequence e1 exn_cont VP.print id;
      with_location_mapping ~label:"Ctrywith" ~dbg ppf (fun () ->
            fprintf ppf "%a)@]" sequence e2);

and sequence ppf = function
  | Csequence(e1, e2) -> fprintf ppf "%a@ %a" sequence e1 sequence e2
  | e -> expression ppf e

and expression ppf e = fprintf ppf "%a" expr e

let property_to_string : Cmm.property -> string = function
  | Zero_alloc -> "zero_alloc"

let codegen_option = function
  | Reduce_code_size -> "reduce_code_size"
  | No_CSE -> "no_cse"
  | Use_linscan_regalloc -> "linscan"
  | Ignore_assert_all property ->
    Printf.sprintf "ignore %s" (property_to_string property)
  | Assume { property; strict; never_returns_normally = _; loc = _ } ->
    Printf.sprintf "assume_%s%s%s"
      (property_to_string property)
      (if strict then "_strict" else "")
      (if strict then "_never_returns_normally" else "")
  | Check { property; strict; loc = _ } ->
    Printf.sprintf "assert_%s%s"
      (property_to_string property)
      (if strict then "_strict" else "")

let print_codegen_options ppf l =
  List.iter (fun c -> fprintf ppf " %s" (codegen_option c)) l

let fundecl ppf f =
  let print_cases ppf cases =
    let first = ref true in
    List.iter
     (fun (id, ty) ->
       if !first then first := false else fprintf ppf "@ ";
       fprintf ppf "%a: %a" VP.print id machtype ty)
     cases in
  with_location_mapping ~label:"Function" ~dbg:f.fun_dbg ppf (fun () ->
  fprintf ppf "@[<1>(function%s%a@ %s@;<1 4>@[<1>(%a)@]@ @[%a@])@]@."
         (location f.fun_dbg) print_codegen_options f.fun_codegen_options f.fun_name.sym_name
         print_cases f.fun_args sequence f.fun_body)

let data_item ppf = function
  | Cdefine_symbol {sym_name; sym_global = Local} -> fprintf ppf "\"%s\":" sym_name
  | Cdefine_symbol {sym_name; sym_global = Global} -> fprintf ppf "global \"%s\":" sym_name
  | Cint8 n -> fprintf ppf "byte %i" n
  | Cint16 n -> fprintf ppf "int16 %i" n
  | Cint32 n -> fprintf ppf "int32 %s" (Nativeint.to_string n)
  | Cint n -> fprintf ppf "int %s" (Nativeint.to_string n)
  | Csingle f -> fprintf ppf "single %F" f
  | Cdouble f -> fprintf ppf "double %F" f
  | Cvec128 {high; low} ->
    fprintf ppf "vec128 %s:%s" (Int64.to_string high) (Int64.to_string low)
  | Csymbol_address s -> fprintf ppf "addr %a:\"%s\"" is_global s.sym_global s.sym_name
  | Csymbol_offset (s, o) -> fprintf ppf "addr %a:\"%s+%d\"" is_global s.sym_global s.sym_name o
  | Cstring s -> fprintf ppf "string \"%s\"" s
  | Cskip n -> fprintf ppf "skip %i" n
  | Calign n -> fprintf ppf "align %i" n

let data ppf dl =
  let items ppf = List.iter (fun d -> fprintf ppf "@ %a" data_item d) dl in
  fprintf ppf "@[<hv 1>(data%t)@]" items

let phrase ppf = function
  | Cfunction f -> fundecl ppf f
  | Cdata dl -> data ppf dl
