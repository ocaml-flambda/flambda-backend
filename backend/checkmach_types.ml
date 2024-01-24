(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2022-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

module Witness = struct
  type kind =
    | Alloc of
        { bytes : int;
          dbginfo : Debuginfo.alloc_dbginfo
        }
    | Indirect_call
    | Indirect_tailcall
    | Direct_call of { callee : string }
    | Direct_tailcall of { callee : string }
    | Missing_summary of { callee : string }
    | Forward_call of { callee : string }
    | Extcall of { callee : string }
    | Arch_specific
    | Probe of
        { name : string;
          handler_code_sym : string
        }

  type t =
    { dbg : Debuginfo.t;
      kind : kind
    }

  let create dbg kind = { dbg; kind }

  let compare { dbg = dbg1; kind = kind1 } { dbg = dbg2; kind = kind2 } =
    (* compare by [dbg] first to print the errors in the order they appear in
       the source file. *)
    let c = Debuginfo.compare dbg1 dbg2 in
    if c <> 0 then c else Stdlib.compare kind1 kind2

  let print_kind ppf kind =
    let open Format in
    match kind with
    | Alloc { bytes; dbginfo = _ } -> fprintf ppf "allocation of %d bytes" bytes
    | Indirect_call -> fprintf ppf "indirect call"
    | Indirect_tailcall -> fprintf ppf "indirect tailcall"
    | Direct_call { callee } -> fprintf ppf "direct call %s" callee
    | Direct_tailcall { callee : string } ->
      fprintf ppf "direct tailcall %s" callee
    | Missing_summary { callee } -> fprintf ppf "missing summary for %s" callee
    | Forward_call { callee } ->
      fprintf ppf "foward call or tailcall (conservatively handled) %s" callee
    | Extcall { callee } -> fprintf ppf "external call to %s" callee
    | Arch_specific -> fprintf ppf "arch specific operation"
    | Probe { name; handler_code_sym } ->
      fprintf ppf "probe %s handler %s" name handler_code_sym

  let get_alloc_dbginfo kind =
    match kind with
    | Alloc { bytes = _; dbginfo } -> Some dbginfo
    | Indirect_call | Indirect_tailcall | Direct_call _ | Direct_tailcall _
    | Missing_summary _ | Forward_call _ | Extcall _ | Arch_specific | Probe _
      ->
      None

  let print ppf { kind; dbg } =
    Format.fprintf ppf "%a %a" print_kind kind Debuginfo.print_compact dbg
end

module Witnesses : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val iter : t -> f:(Witness.t -> unit) -> unit

  val join : t -> t -> t

  val create : Witness.kind -> Debuginfo.t -> t

  val print : Format.formatter -> t -> unit

  val elements : t -> Witness.t list

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  val simplify : components -> components
end = struct
  include Set.Make (Witness)

  (* CR gyorsh: consider using Flambda_backend_flags.checkmach_details_cutoff to
     limit the size of this set. The downside is that it won't get tested as
     much. Only keep witnesses for functions that need checking. *)
  let join = union

  let create kind dbg = singleton (Witness.create dbg kind)

  let iter t ~f = iter f t

  let print ppf t = Format.pp_print_seq Witness.print ppf (to_seq t)

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  let simplify { nor; exn; div } =
    { div =
        (* don't print diverge witnesses unless they are the only ones. *)
        (if is_empty nor && is_empty exn then div else empty);
      nor;
      (* only print the exn witnesses that are not also nor witnesses. *)
      exn = diff exn nor
    }
end

type iter_witnesses = (string -> Witnesses.components -> unit) -> unit
