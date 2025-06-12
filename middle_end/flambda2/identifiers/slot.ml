(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  include Container_types.S

  module Lmap : Lmap.S with type key = t

  val create :
    Compilation_unit.t ->
    name:string ->
    is_always_immediate:bool ->
    Flambda_kind.t ->
    t

  val get_compilation_unit : t -> Compilation_unit.t

  val in_compilation_unit : t -> Compilation_unit.t -> bool

  val is_imported : t -> bool

  val to_string : t -> string

  val name : t -> string

  val kind : t -> Flambda_kind.t

  val is_always_immediate : t -> bool

  val rename : t -> t
end

module Make (P : sig
  val colour : Format.formatter -> unit
end) : S = struct
  type t =
    { compilation_unit : Compilation_unit.t;
      name : string;
      name_stamp : int;
          (** [name_stamp]s are unique within any given compilation unit. *)
      kind : Flambda_kind.t;
      is_always_immediate : bool
    }

  module Self = Container_types.Make (struct
    type nonrec t = t

    let compare
        ({ compilation_unit = compilation_unit1;
           name = _;
           name_stamp = name_stamp1;
           kind = _;
           is_always_immediate = _
         } as t1)
        ({ compilation_unit = compilation_unit2;
           name = _;
           name_stamp = name_stamp2;
           kind = _;
           is_always_immediate = _
         } as t2) =
      if t1 == t2
      then 0
      else
        let c = name_stamp1 - name_stamp2 in
        if c <> 0
        then c
        else Compilation_unit.compare compilation_unit1 compilation_unit2

    let equal t1 t2 = compare t1 t2 = 0

    let hash t =
      Hashtbl.hash (t.name_stamp, Compilation_unit.hash t.compilation_unit)

    let print ppf t =
      Format.fprintf ppf "@[%t(" P.colour;
      if Compilation_unit.equal t.compilation_unit
           (Compilation_unit.get_current_exn ())
      then Format.fprintf ppf "%s/%d" t.name t.name_stamp
      else
        Format.fprintf ppf "%a.%s/%d" Compilation_unit.print t.compilation_unit
          t.name t.name_stamp;
      Format.fprintf ppf " @<1>\u{2237} %a%s" Flambda_kind.print t.kind
        (if t.is_always_immediate then "(immediate)" else "");
      Format.fprintf ppf ")%t@]" Flambda_colours.pop
  end)

  include Self

  module Lmap = Lmap.Make (struct
    type nonrec t = t

    include Self
  end)

  let next_stamp = ref 0

  let get_next_stamp () =
    let stamp = !next_stamp in
    incr next_stamp;
    stamp

  let create compilation_unit ~name ~is_always_immediate kind =
    { compilation_unit;
      name;
      name_stamp = get_next_stamp ();
      kind;
      is_always_immediate
    }

  let get_compilation_unit t = t.compilation_unit

  let in_compilation_unit t compilation_unit =
    Compilation_unit.equal compilation_unit t.compilation_unit

  let is_imported t = not (Compilation_unit.is_current t.compilation_unit)

  let to_string t = t.name ^ "_" ^ string_of_int t.name_stamp

  let name t = t.name

  let kind t = t.kind

  let is_always_immediate t = t.is_always_immediate

  let rename t = { t with name_stamp = get_next_stamp () }
end
