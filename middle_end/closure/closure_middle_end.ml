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

[@@@ocaml.warning "+a-4-30-40-41-42"]

let raw_clambda_dump_if ppf
      ((ulambda, _, structured_constants) : Clambda.with_constants) =
  if !Clflags.dump_rawclambda || !Clflags.dump_clambda then
    begin
      Format.fprintf ppf "@.clambda:@.";
      Printclambda.clambda ppf ulambda;
      List.iter (fun { Clambda. symbol; definition; _ } ->
          Format.fprintf ppf "%s:@ %a@."
            symbol
            Printclambda.structured_constant definition)
        structured_constants
    end;
  if !Clflags.dump_cmm then Format.fprintf ppf "@.cmm:@."

let lambda_to_clambda ~backend ~filename:_ ~prefixname:_ ~ppf_dump
      (lambda : Lambda.program) =
  let size =
    match lambda.module_block_format with
    | Mb_record { mb_size; _ } -> mb_size
    | Mb_wrapped_function _ ->
      Misc.fatal_error "Parameterised modules not supported by Closure"
  in
  let clambda = Closure.intro ~backend ~size lambda.code in
  let provenance : Clambda.usymbol_provenance =
    let current_unit_ident =
      Compilation_unit.get_current_exn ()
      |> Compilation_unit.name
      |> Compilation_unit.Name.to_string
      |> Ident.create_persistent
    in
    { original_idents = [];
      (* CR-someday lmaurer: Properly construct a [Path.t] from the module name
         with its pack prefix. *)
      module_path = Path.Pident current_unit_ident;
    }
  in
  let symbol =
    Symbol.for_current_unit ()
    |> Symbol.linkage_name
    |> Linkage_name.to_string
  in
  let preallocated_block =
    Clambda.{
      symbol;
      exported = true;
      tag = 0;
      fields = List.init size (fun _ -> None);
      provenance = Some provenance;
    }
  in
  let constants = Compilenv.structured_constants () in
  Compilenv.clear_structured_constants ();
  let clambda_and_constants =
    clambda, [preallocated_block], constants
  in
  Compiler_hooks.execute Compiler_hooks.Raw_clambda clambda;
  Compiler_hooks.execute Compiler_hooks.Clambda clambda;
  raw_clambda_dump_if ppf_dump clambda_and_constants;
  clambda_and_constants
