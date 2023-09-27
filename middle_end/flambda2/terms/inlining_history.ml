(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Absolute = struct
  type t = Compilation_unit.t * path

  and path =
    | Empty
    | Unknown of { prev : path }
    | Function of
        { dbg : Debuginfo.t;
          name : string;
          prev : path
        }
    | Module of
        { name : string;
          prev : path
        }
    | Class of
        { name : string;
          prev : path
        }
    | Call of
        { dbg : Debuginfo.t;
          callee : t;
          prev : path
        }
    | Inline of { prev : path }

  let empty compilation_unit = compilation_unit, Empty

  let compilation_unit (f, _) = f

  let path (_, p) = p

  let uid_path h = Marshal.to_bytes h [] |> Digest.bytes |> Digest.to_hex

  let rec print_path ppf (t : path) =
    let rec aux ppf = function
      | Empty -> Format.fprintf ppf ""
      | Unknown { prev } -> Format.fprintf ppf "%a(???)" aux prev
      | Function { name; prev; _ } -> Format.fprintf ppf "%a%s" aux prev name
      | Class { name; prev } -> Format.fprintf ppf "%a%s#" aux prev name
      | Module { name; prev } -> Format.fprintf ppf "%a%s." aux prev name
      | Call { prev; callee; dbg } ->
        Format.fprintf ppf "%a(calling %a[%a])" aux prev print callee
          Debuginfo.print_compact dbg
      | Inline { prev } -> Format.fprintf ppf "(%a inlined)" aux prev
    in
    aux ppf t

  and print ppf (compilation_unit, t) =
    Format.fprintf ppf "%a::%a" Compilation_unit.print_name compilation_unit
      print_path t

  let tag_path (path : path) =
    match path with
    | Empty -> 0
    | Unknown _ -> 1
    | Function _ -> 2
    | Module _ -> 3
    | Class _ -> 4
    | Call _ -> 5
    | Inline _ -> 6

  let[@ocaml.warning "-4"] rec compare_path (a : path) (b : path) =
    match a, b with
    | Empty, Empty -> 0
    | Unknown p1, Unknown p2 -> compare_path p1.prev p2.prev
    | Function f1, Function f2 ->
      let c = Debuginfo.compare f1.dbg f2.dbg in
      if c <> 0
      then c
      else
        let c = String.compare f1.name f2.name in
        if c <> 0 then c else compare_path f1.prev f2.prev
    | Module f1, Module f2 ->
      let c = String.compare f1.name f2.name in
      if c <> 0 then c else compare_path f1.prev f2.prev
    | Class f1, Class f2 ->
      let c = String.compare f1.name f2.name in
      if c <> 0 then c else compare_path f1.prev f2.prev
    | Call c1, Call c2 ->
      let c = Debuginfo.compare c1.dbg c2.dbg in
      if c <> 0
      then c
      else
        let c = compare c1.callee c2.callee in
        if c <> 0 then c else compare_path c1.prev c2.prev
    | Inline p1, Inline p2 -> compare_path p1.prev p2.prev
    | p1, p2 -> Int.compare (tag_path p1) (tag_path p2)

  and compare (compilation_unit1, t1) (compilation_unit2, t2) =
    let c = Compilation_unit.compare compilation_unit1 compilation_unit2 in
    if c <> 0 then c else compare_path t1 t2

  let to_string t =
    print Format.str_formatter t;
    Format.flush_str_formatter ()

  let rec shorten_to_definition (compilation_unit, absolute) =
    let rec aux t =
      match t with
      | Empty -> compilation_unit, Empty
      | Unknown { prev } ->
        let f, prev = aux prev in
        f, Unknown { prev }
      | Function { name; prev; dbg } ->
        let f, prev = aux prev in
        f, Function { name; prev; dbg }
      | Class { name; prev } ->
        let f, prev = aux prev in
        f, Class { name; prev }
      | Module { name; prev } ->
        let f, prev = aux prev in
        f, Module { name; prev }
      | Call { callee; _ } -> shorten_to_definition callee
      | Inline { prev } -> aux prev
    in
    aux absolute
end

module Relative = struct
  type t = Absolute.path

  let print = Absolute.print_path

  let empty = Absolute.Empty

  let compare = Absolute.compare_path

  let rec concat ~(earlier : t) ~(later : t) : t =
    match later with
    | Absolute.Empty -> earlier
    | Unknown { prev } -> Unknown { prev = concat ~earlier ~later:prev }
    | Class { name; prev } -> Class { name; prev = concat ~earlier ~later:prev }
    | Module { name; prev } ->
      Module { name; prev = concat ~earlier ~later:prev }
    | Function { name; prev; dbg } ->
      Function { name; prev = concat ~earlier ~later:prev; dbg }
    | Call { callee; prev; dbg } ->
      Call { callee; prev = concat ~earlier ~later:prev; dbg }
    | Inline { prev } -> Inline { prev = concat ~earlier ~later:prev }

  let fundecl ~dbg ~name prev = Absolute.Function { name; prev; dbg }

  let call ~dbg ~callee prev = Absolute.Call { callee; prev; dbg }

  let inline prev = Absolute.Inline { prev }

  let unknown prev = Absolute.Unknown { prev }

  let between_scoped_locations ~(parent : Debuginfo.Scoped_location.t)
      ~(child : Debuginfo.Scoped_location.t) =
    let scopes_are_equal (a : Debuginfo.Scoped_location.scopes)
        (b : Debuginfo.Scoped_location.scopes) =
      match a, b with
      | Cons a, Cons b ->
        a.item = b.item && a.str = b.str && a.str_fun = b.str_fun
        && a.name = b.name
      | Empty, _ | _, Empty -> false
    in
    let rec aux ~parent ~child =
      if scopes_are_equal parent child
      then Absolute.Empty
      else
        match child with
        | Debuginfo.Scoped_location.Empty -> Absolute.Empty
        | Debuginfo.Scoped_location.Cons { item; name; prev; _ } -> (
          let prev = aux ~parent ~child:prev in
          match item with
          | Sc_module_definition -> Absolute.Module { name; prev }
          | Sc_class_definition -> Absolute.Class { name; prev }
          | Sc_anonymous_function | Sc_method_definition | Sc_value_definition
          | Sc_lazy | Sc_partial_or_eta_wrapper ->
            prev)
    in
    let parent =
      match parent with
      | Loc_unknown -> Debuginfo.Scoped_location.empty_scopes
      | Loc_known { scopes; _ } -> scopes
    in
    let child =
      match child with
      | Loc_unknown -> parent
      | Loc_known { scopes; _ } -> scopes
    in
    aux ~parent ~child
end

let extend_absolute (compilation_unit, absolute) relative =
  let rec aux (r : Relative.t) : Absolute.path =
    match r with
    | Absolute.Empty -> absolute
    | Unknown { prev } -> Unknown { prev = aux prev }
    | Class { name; prev } -> Class { name; prev = aux prev }
    | Module { name; prev } -> Module { name; prev = aux prev }
    | Function { name; prev; dbg } -> Function { name; prev = aux prev; dbg }
    | Call { callee; prev; dbg } -> Call { callee; prev = aux prev; dbg }
    | Inline { prev } -> Inline { prev = aux prev }
  in
  compilation_unit, aux relative

module Tracker = struct
  type t =
    { absolute : Absolute.t;
      relative : Relative.t
    }

  let empty compilation_unit =
    { absolute = Absolute.empty compilation_unit; relative = Relative.empty }

  let inside_function absolute = { absolute; relative = Relative.empty }

  let absolute { absolute; _ } = absolute

  let relative { relative; _ } = relative

  let fundecl ~dbg ~function_relative_history ~name t =
    let relative' =
      Relative.concat ~earlier:t.relative ~later:function_relative_history
    in
    let relative = Relative.fundecl ~dbg ~name relative' in
    let absolute = extend_absolute t.absolute relative in
    absolute, relative'

  let unknown { absolute; relative } =
    let relative = Relative.unknown relative in
    let absolute = extend_absolute absolute relative in
    absolute, relative

  let enter_inlined_apply ~dbg ~callee ~apply_relative_history t =
    (* Discard the relative history from the tracker as it should already be
       present on the apply node. *)
    let relative =
      Relative.inline (Relative.call ~dbg ~callee apply_relative_history)
    in
    { t with relative }

  let unknown_call ~dbg ~relative { absolute; _ } =
    extend_absolute absolute
      (Relative.call ~dbg
         ~callee:(Absolute.empty (Compilation_unit.get_current_exn ()))
         relative)

  let call ~dbg ~callee ~relative { absolute; _ } =
    extend_absolute absolute (Relative.call ~dbg ~callee relative)

  let fundecl_of_scoped_location ~name
      ~(path_to_root : Debuginfo.Scoped_location.t)
      (loc : Debuginfo.Scoped_location.t) t =
    match loc with
    | Loc_unknown -> unknown t
    | Loc_known _ ->
      let relative =
        Relative.between_scoped_locations ~parent:path_to_root ~child:loc
      in
      fundecl ~function_relative_history:relative
        ~dbg:(Debuginfo.from_location loc)
        ~name t
end
