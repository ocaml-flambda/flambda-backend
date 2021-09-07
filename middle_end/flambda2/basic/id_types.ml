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

module type BaseId = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val name : t -> string option
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val rename : t -> t
end

module type Id = sig
  include BaseId
  val create : ?name:string -> unit -> t
end

module type UnitId = sig
  module Compilation_unit : Container_types.Thing
  include BaseId
  val create : ?name:string -> Compilation_unit.t -> t
  val unit : t -> Compilation_unit.t
  val unique_name : t -> string
end

module Id(_:sig end) : Id = struct
  type t = int * string
  let empty_string = ""
  let r = ref 0
  let create ?(name=empty_string) () = incr r; !r, name
  let rename (_stamp, name) = create ~name ()
  let equal (t1,_) (t2,_) = (t1:int) = t2
  let compare (t1,_) (t2,_) = t1 - t2
  let hash (t,_) = t
  let name (_,name) =
    if name == empty_string
    then None
    else Some name
  let to_string (t,name) =
    if name == empty_string
    then Int.to_string t
    else Printf.sprintf "%s_%i" name t
  let output fd t = output_string fd (to_string t)
  let [@ocamlformat "disable"] print ppf v = Format.pp_print_string ppf (to_string v)
end

module UnitId(Innerid:Id)(Compilation_unit:Container_types.Thing) :
  UnitId with module Compilation_unit := Compilation_unit = struct
  type t = {
    id : Innerid.t;
    unit : Compilation_unit.t;
  }
  let compare x y =
    let c = Innerid.compare x.id y.id in
    if c <> 0
    then c
    else Compilation_unit.compare x.unit y.unit
  let output oc x =
    Printf.fprintf oc "%a.%a"
      Compilation_unit.output x.unit
      Innerid.output x.id
  let [@ocamlformat "disable"] print ppf x =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print x.unit
      Innerid.print x.id
  let hash off = Hashtbl.hash off
  let equal o1 o2 = compare o1 o2 = 0
  let name o = Innerid.name o.id
  let unique_name o = Innerid.to_string o.id

  let rename t =
    { id = Innerid.rename t.id;
      unit = t.unit;
    }

  let to_string x =
    Format.asprintf "%a.%a"
      Compilation_unit.print x.unit
      Innerid.print x.id
  let create ?name unit =
    let id = Innerid.create ?name () in
    { id; unit }
  let unit x = x.unit
end
