(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let hash_seed =
  let seed = Random.bits () in
  if seed mod 2 = 0 then seed + 1 else seed

(* Fast integer hashing algorithm for sdolan. With the stdlib Hashtbl
   implementation it's ok that this returns > 30 bits. *)
let hash2 a b =
  let a = Hashtbl.hash a in
  let b = Hashtbl.hash b in
  let r = (a * hash_seed) + b in
  r lxor (r lsr 17)

module Id = Table_by_int_id.Id

let var_flags = 0

let symbol_flags = 1

let const_flags = 2

let simple_flags = 3

let code_id_flags = 4

module Const_data = struct
  type t =
    | Naked_immediate of Targetint_31_63.t
    | Tagged_immediate of Targetint_31_63.t
    | Naked_float32 of Numeric_types.Float32_by_bit_pattern.t
    | Naked_float of Numeric_types.Float_by_bit_pattern.t
    | Naked_int32 of Int32.t
    | Naked_int64 of Int64.t
    | Naked_nativeint of Targetint_32_64.t
    | Naked_vec128 of Vector_types.Vec128.Bit_pattern.t

  let flags = const_flags

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf (t : t) =
      match t with
      | Naked_immediate i ->
        Format.fprintf ppf "%t#%a%t"
          Flambda_colours.naked_number
          Targetint_31_63.print i
          Flambda_colours.pop
      | Tagged_immediate i ->
        Format.fprintf ppf "%t%a%t"
          Flambda_colours.tagged_immediate
          Targetint_31_63.print i
          Flambda_colours.pop
      | Naked_float32 f ->
        Format.fprintf ppf "%t#%as%t"
          Flambda_colours.naked_number
          Numeric_types.Float32_by_bit_pattern.print f
          Flambda_colours.pop
      | Naked_float f ->
        Format.fprintf ppf "%t#%a%t"
          Flambda_colours.naked_number
          Numeric_types.Float_by_bit_pattern.print f
          Flambda_colours.pop
      | Naked_int32 n ->
        Format.fprintf ppf "%t#%ldl%t"
          Flambda_colours.naked_number
          n
          Flambda_colours.pop
      | Naked_int64 n ->
        Format.fprintf ppf "%t#%LdL%t"
          Flambda_colours.naked_number
          n
          Flambda_colours.pop
      | Naked_nativeint n ->
        Format.fprintf ppf "%t#%an%t"
          Flambda_colours.naked_number
          Targetint_32_64.print n
          Flambda_colours.pop
      | Naked_vec128 (v) ->
        Format.fprintf ppf "%t#vec128[%a]%t"
          Flambda_colours.naked_number
          Vector_types.Vec128.Bit_pattern.print v
          Flambda_colours.pop

    let compare t1 t2 =
      match t1, t2 with
      | Naked_immediate i1, Naked_immediate i2 -> Targetint_31_63.compare i1 i2
      | Tagged_immediate i1, Tagged_immediate i2 ->
        Targetint_31_63.compare i1 i2
      | Naked_float32 f1, Naked_float32 f2 ->
        Numeric_types.Float32_by_bit_pattern.compare f1 f2
      | Naked_float f1, Naked_float f2 ->
        Numeric_types.Float_by_bit_pattern.compare f1 f2
      | Naked_int32 n1, Naked_int32 n2 -> Int32.compare n1 n2
      | Naked_int64 n1, Naked_int64 n2 -> Int64.compare n1 n2
      | Naked_nativeint n1, Naked_nativeint n2 -> Targetint_32_64.compare n1 n2
      | Naked_vec128 v1, Naked_vec128 v2 ->
        Vector_types.Vec128.Bit_pattern.compare v1 v2
      | Naked_immediate _, _ -> -1
      | _, Naked_immediate _ -> 1
      | Tagged_immediate _, _ -> -1
      | _, Tagged_immediate _ -> 1
      | Naked_float _, _ -> -1
      | _, Naked_float _ -> 1
      | Naked_float32 _, _ -> -1
      | _, Naked_float32 _ -> 1
      | Naked_int32 _, _ -> -1
      | _, Naked_int32 _ -> 1
      | Naked_int64 _, _ -> -1
      | _, Naked_int64 _ -> 1
      | Naked_vec128 _, _ -> -1
      | _, Naked_vec128 _ -> 1

    let equal t1 t2 =
      if t1 == t2
      then true
      else
        match t1, t2 with
        | Naked_immediate i1, Naked_immediate i2 -> Targetint_31_63.equal i1 i2
        | Tagged_immediate i1, Tagged_immediate i2 ->
          Targetint_31_63.equal i1 i2
        | Naked_float32 f1, Naked_float32 f2 ->
          Numeric_types.Float32_by_bit_pattern.equal f1 f2
        | Naked_float f1, Naked_float f2 ->
          Numeric_types.Float_by_bit_pattern.equal f1 f2
        | Naked_int32 n1, Naked_int32 n2 -> Int32.equal n1 n2
        | Naked_int64 n1, Naked_int64 n2 -> Int64.equal n1 n2
        | Naked_nativeint n1, Naked_nativeint n2 -> Targetint_32_64.equal n1 n2
        | Naked_vec128 v1, Naked_vec128 v2 ->
          Vector_types.Vec128.Bit_pattern.equal v1 v2
        | ( ( Naked_immediate _ | Tagged_immediate _ | Naked_float _
            | Naked_float32 _ | Naked_vec128 _ | Naked_int32 _ | Naked_int64 _
            | Naked_nativeint _ ),
            _ ) ->
          false

    let hash t =
      match t with
      | Naked_immediate n -> Targetint_31_63.hash n
      | Tagged_immediate n -> Targetint_31_63.hash n
      | Naked_float32 n -> Numeric_types.Float32_by_bit_pattern.hash n
      | Naked_float n -> Numeric_types.Float_by_bit_pattern.hash n
      | Naked_int32 n -> Hashtbl.hash n
      | Naked_int64 n -> Hashtbl.hash n
      | Naked_nativeint n -> Targetint_32_64.hash n
      | Naked_vec128 v -> Vector_types.Vec128.Bit_pattern.hash v
  end)
end

module Variable_data = struct
  type t =
    { compilation_unit : Compilation_unit.t;
      name : string;
      name_stamp : int;
      user_visible : bool
    }

  let flags = var_flags

  let [@ocamlformat "disable"] print ppf { compilation_unit; name; name_stamp;
                                           user_visible; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(name_stamp@ %d)@]@ \
        @[<hov 1>(user_visible@ %b)@]\
        )@]"
      Compilation_unit.print_debug compilation_unit
      name
      name_stamp
      user_visible

  let hash { compilation_unit; name = _; name_stamp; user_visible = _ } =
    hash2 (Compilation_unit.hash compilation_unit) (Hashtbl.hash name_stamp)

  let equal t1 t2 =
    if t1 == t2
    then true
    else
      let { compilation_unit = compilation_unit1;
            name = _;
            name_stamp = name_stamp1;
            user_visible = _
          } =
        t1
      in
      let { compilation_unit = compilation_unit2;
            name = _;
            name_stamp = name_stamp2;
            user_visible = _
          } =
        t2
      in
      Int.equal name_stamp1 name_stamp2
      && Compilation_unit.equal compilation_unit1 compilation_unit2
end

module Symbol0 = Flambda2_import.Symbol

module Symbol_data = struct
  include Symbol0

  let unsafe_create compilation_unit linkage_name =
    Symbol0.unsafe_create compilation_unit linkage_name

  let flags = symbol_flags

  let [@ocamlformat "disable"] print ppf symbol =
    let compilation_unit = Symbol0.compilation_unit symbol in
    let linkage_name = Symbol0.linkage_name symbol in
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(linkage_name@ %a)@]\
        )@]"
      Compilation_unit.print_debug compilation_unit
      Linkage_name.print linkage_name
end

module Code_id_data = struct
  type t =
    { compilation_unit : Compilation_unit.t;
      name : string;
      linkage_name : Linkage_name.t
    }

  let flags = code_id_flags

  let [@ocamlformat "disable"] print ppf { compilation_unit; name; linkage_name; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(linkage_name@ %a)@]@ \
        )@]"
      Compilation_unit.print_debug compilation_unit
      name
      Linkage_name.print linkage_name

  let hash { compilation_unit = _; name = _; linkage_name } =
    (* As per comment above, just looking at the linkage name suffices (same
       below), rather than the compilation unit as well. *)
    Linkage_name.hash linkage_name

  let equal { compilation_unit = _; name = _; linkage_name = linkage_name1 }
      { compilation_unit = _; name = _; linkage_name = linkage_name2 } =
    Linkage_name.equal linkage_name1 linkage_name2
end

module Const = struct
  type t = Id.t

  type exported = Const_data.t

  module Table = Table_by_int_id.Make (Const_data)

  let grand_table_of_constants = ref (Table.create ())

  let initialise () = grand_table_of_constants := Table.create ()

  let find_data t = Table.find !grand_table_of_constants t

  module Descr = Const_data

  let create (data : Const_data.t) = Table.add !grand_table_of_constants data

  let naked_immediate imm = create (Naked_immediate imm)

  let tagged_immediate imm = create (Tagged_immediate imm)

  let naked_float32 f = create (Naked_float32 f)

  let naked_float f = create (Naked_float f)

  let naked_int32 i = create (Naked_int32 i)

  let naked_int64 i = create (Naked_int64 i)

  let naked_nativeint i = create (Naked_nativeint i)

  let naked_vec128 i = create (Naked_vec128 i)

  let const_true = tagged_immediate Targetint_31_63.bool_true

  let const_false = tagged_immediate Targetint_31_63.bool_false

  let untagged_const_true = naked_immediate Targetint_31_63.bool_true

  let untagged_const_false = naked_immediate Targetint_31_63.bool_false

  let untagged_const_zero = naked_immediate Targetint_31_63.zero

  let untagged_const_int i = naked_immediate i

  let const_int i = tagged_immediate i

  let const_zero = tagged_immediate Targetint_31_63.zero

  let const_one = tagged_immediate Targetint_31_63.one

  let const_unit = const_zero

  let descr t = find_data t

  module T0 = struct
    let compare = Id.compare

    let equal = Id.equal

    let hash = Id.hash

    let [@ocamlformat "disable"] print ppf t = Const_data.print ppf (descr t)
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map

  let export t = find_data t

  let import (data : exported) = create data
end

module Variable = struct
  type t = Id.t

  type exported = Variable_data.t

  module Table = Table_by_int_id.Make (Variable_data)

  let grand_table_of_variables = ref (Table.create ())

  let initialise () = grand_table_of_variables := Table.create ()

  let find_data t = Table.find !grand_table_of_variables t

  let compilation_unit t = (find_data t).compilation_unit

  let name t = (find_data t).name

  let name_stamp t = (find_data t).name_stamp

  let user_visible t = (find_data t).user_visible

  let previous_name_stamp = ref (-1)

  let create ?user_visible name =
    let name_stamp =
      (* CR mshinwell: check for overflow on 32 bit *)
      incr previous_name_stamp;
      !previous_name_stamp
    in
    let data : Variable_data.t =
      { compilation_unit = Compilation_unit.get_current_exn ();
        name;
        name_stamp;
        user_visible = Option.is_some user_visible
      }
    in
    Table.add !grand_table_of_variables data

  module T0 = struct
    let compare = Id.compare

    let equal = Id.equal

    let hash = Id.hash

    let print ppf t =
      let cu = compilation_unit t in
      if Compilation_unit.equal cu (Compilation_unit.get_current_exn ())
      then
        Format.fprintf ppf "%s/%d%s" (name t) (name_stamp t)
          (if user_visible t then "UV" else "N")
      else
        Format.fprintf ppf "%a.%s/%d%s" Compilation_unit.print cu (name t)
          (name_stamp t)
          (if user_visible t then "UV" else "N")
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map

  let export t = find_data t

  let import (data : exported) = Table.add !grand_table_of_variables data
end

module Symbol = struct
  type t = Id.t

  type exported = Symbol_data.t

  module Table = Table_by_int_id.Make (Symbol_data)

  let grand_table_of_symbols = ref (Table.create ())

  let initialise () = grand_table_of_symbols := Table.create ()

  let find_data t = Table.find !grand_table_of_symbols t

  let create_wrapped data = Table.add !grand_table_of_symbols data

  let unsafe_create compilation_unit linkage_name =
    Symbol_data.unsafe_create compilation_unit linkage_name |> create_wrapped

  let extern_syms = "*extern*" |> Compilation_unit.of_string

  let external_symbols_compilation_unit () = extern_syms

  let create compilation_unit linkage_name =
    let data =
      if Compilation_unit.equal compilation_unit extern_syms
      then
        (* Use linkage name without prefixing the compilation unit *)
        Symbol0.unsafe_create compilation_unit linkage_name
      else
        Symbol0.for_name compilation_unit
          (linkage_name |> Linkage_name.to_string)
    in
    create_wrapped data

  let compilation_unit t = Symbol0.compilation_unit (find_data t)

  let linkage_name t = Symbol0.linkage_name (find_data t)

  let linkage_name_as_string t = Linkage_name.to_string (linkage_name t)

  module T0 = struct
    let compare = Id.compare

    let equal = Id.equal

    let hash = Id.hash

    let print ppf t =
      Format.fprintf ppf "%t" Flambda_colours.symbol;
      Compilation_unit.print ppf (compilation_unit t);
      Format.pp_print_string ppf ".";
      Linkage_name.print ppf (linkage_name t);
      Format.fprintf ppf "%t" Flambda_colours.pop
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map

  let export t = find_data t

  let import (data : exported) = Table.add !grand_table_of_symbols data
end

module Name = struct
  type t = Id.t

  let var v = v

  let symbol s = s

  let[@inline always] pattern_match t ~var ~symbol =
    let flags = Id.flags t in
    if flags = var_flags
    then var t
    else if flags = symbol_flags
    then symbol t
    else assert false

  module T0 = struct
    let compare = Id.compare

    let equal = Id.equal

    let hash = Id.hash

    let print ppf t =
      Format.fprintf ppf "%t" Flambda_colours.name;
      pattern_match t
        ~var:(fun var -> Variable.print ppf var)
        ~symbol:(fun symbol -> Symbol.print ppf symbol);
      Format.fprintf ppf "%t" Flambda_colours.pop
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map
end

module Rec_info_expr = Rec_info_expr0.Make (Variable)
module Coercion = Coercion0.Make (Rec_info_expr)

module Simple_data = struct
  type t =
    { simple : Id.t;
      (* always without [Coercion] *)
      coercion : Coercion.t
    }

  let flags = simple_flags

  let [@ocamlformat "disable"] print ppf { simple = _; coercion; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(coercion@ %a)@]\
        @]"
      Coercion.print coercion

  let hash { simple; coercion } =
    Hashtbl.hash (Id.hash simple, Coercion.hash coercion)

  let equal t1 t2 =
    if t1 == t2
    then true
    else
      let { simple = simple1; coercion = coercion1 } = t1 in
      let { simple = simple2; coercion = coercion2 } = t2 in
      Id.equal simple1 simple2 && Coercion.equal coercion1 coercion2
end

module Simple = struct
  type t = Id.t

  type exported = Simple_data.t

  module Table = Table_by_int_id.Make (Simple_data)

  (* This table only holds [Simple]s that have auxiliary data associated with
     them, as indicated by the setting of the [simple_flags]. *)
  let grand_table_of_simples = ref (Table.create ())

  let initialise () = grand_table_of_simples := Table.create ()

  let find_data t = Table.find !grand_table_of_simples t

  let has_coercion t = Id.flags t = simple_flags

  let name n = n

  let var v = v

  let vars vars = vars

  let symbol s = s

  let const cst = cst

  let[@inline always] pattern_match t ~name ~const =
    let flags = Id.flags t in
    if flags = var_flags
    then (name [@inlined hint]) (Name.var t) ~coercion:Coercion.id
    else if flags = symbol_flags
    then (name [@inlined hint]) (Name.symbol t) ~coercion:Coercion.id
    else if flags = const_flags
    then (const [@inlined hint]) t
    else if flags = simple_flags
    then
      let { Simple_data.simple = t; coercion } = find_data t in
      let flags = Id.flags t in
      if flags = var_flags
      then (name [@inlined hint]) (Name.var t) ~coercion
      else if flags = symbol_flags
      then (name [@inlined hint]) (Name.symbol t) ~coercion
      else if flags = const_flags
      then (const [@inlined hint]) t
      else assert false
    else assert false

  let same t1 t2 =
    let name n1 ~coercion:co1 =
      pattern_match t2
        ~name:(fun n2 ~coercion:co2 ->
          Name.equal n1 n2 && Coercion.equal co1 co2)
        ~const:(fun _ -> false)
    in
    let const c1 =
      pattern_match t2
        ~name:(fun _ ~coercion:_ -> false)
        ~const:(fun c2 -> Const.equal c1 c2)
    in
    pattern_match t1 ~name ~const

  let[@inline always] coercion t =
    let flags = Id.flags t in
    if flags = simple_flags then (find_data t).coercion else Coercion.id

  module T0 = struct
    let compare = Id.compare

    let equal = Id.equal

    let hash = Id.hash

    let print ppf t =
      let print ppf t =
        pattern_match t
          ~name:(fun name ~coercion:_ -> Name.print ppf name)
          ~const:(fun cst -> Const.print ppf cst)
      in
      let coercion = coercion t in
      if Coercion.is_id coercion
      then print ppf t
      else
        Format.fprintf ppf "@[<hov 1>(coerce@ %a@ %a)@]" print t Coercion.print
          coercion
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  let with_coercion t new_coercion =
    if Coercion.is_id new_coercion || Id.flags t = const_flags
    then t
    else if Coercion.is_id (coercion t)
    then
      let data : Simple_data.t = { simple = t; coercion = new_coercion } in
      Table.add !grand_table_of_simples data
    else
      Misc.fatal_errorf
        "Cannot add [Coercion] to [Simple] %a that already has non-identity \
         [Coercion]"
        print t

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map

  let export t = find_data t

  let import (data : exported) =
    (* Note: We do not import the underlying name or const. This is done on
       purpose, to make the import process simpler and well-defined, but means
       that the real import functions (in Renaming) are responsible for
       importing the underlying name/const. *)
    Table.add !grand_table_of_simples data
end

module Code_id = struct
  type t = Id.t

  type exported = Code_id_data.t

  module Table = Table_by_int_id.Make (Code_id_data)

  let grand_table_of_code_ids = ref (Table.create ())

  let initialise () = grand_table_of_code_ids := Table.create ()

  let find_data t = Table.find !grand_table_of_code_ids t

  let get_compilation_unit t = (find_data t).compilation_unit

  let linkage_name t = (find_data t).linkage_name

  let name t = (find_data t).name

  let previous_name_stamp = ref (-1)

  let create ~name compilation_unit =
    let name_stamp =
      if !previous_name_stamp = max_int
      then Misc.fatal_error "Have run out of name stamps";
      incr previous_name_stamp;
      !previous_name_stamp
    in
    let linkage_name =
      let name =
        if Flambda_features.Expert.shorten_symbol_names ()
        then Printf.sprintf "%s_%d" name name_stamp
        else Printf.sprintf "%s_%d_code" name name_stamp
      in
      Symbol0.for_name compilation_unit name |> Symbol0.linkage_name
    in
    let data : Code_id_data.t = { compilation_unit; name; linkage_name } in
    Table.add !grand_table_of_code_ids data

  let rename t = create ~name:(name t) (Compilation_unit.get_current_exn ())

  let in_compilation_unit t comp_unit =
    Compilation_unit.equal (get_compilation_unit t) comp_unit

  let is_imported t = not (Compilation_unit.is_current (get_compilation_unit t))

  module T0 = struct
    let compare = Id.compare

    let equal = Id.equal

    let hash = Id.hash

    let print ppf t =
      Format.fprintf ppf "%t%a%t" Flambda_colours.code_id Linkage_name.print
        (linkage_name t) Flambda_colours.pop
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map
  module Lmap = Lmap.Make (T)

  let invert_map map =
    Map.fold
      (fun older newer invert_map -> Map.add newer older invert_map)
      map Map.empty

  let export t = find_data t

  let import (data : exported) = Table.add !grand_table_of_code_ids data
end

module Code_id_or_symbol = struct
  type t = Table_by_int_id.Id.t

  let create_code_id code_id = code_id

  let create_symbol symbol = symbol

  let pattern_match t ~code_id ~symbol =
    let flags = Table_by_int_id.Id.flags t in
    if flags = Code_id_data.flags
    then code_id t
    else if flags = Symbol_data.flags
    then symbol t
    else
      Misc.fatal_errorf "Code_id_or_symbol 0x%x with wrong flags 0x%x" t flags

  let compilation_unit t =
    pattern_match t ~code_id:Code_id.get_compilation_unit
      ~symbol:Symbol.compilation_unit

  module T0 = struct
    let compare = Id.compare

    let equal = Id.equal

    let hash = Id.hash

    let print ppf t =
      pattern_match t
        ~code_id:(fun code_id ->
          Format.fprintf ppf "@[<hov 1>(code_id@ %a)@]" Code_id.print code_id)
        ~symbol:(fun symbol ->
          Format.fprintf ppf "@[<hov 1>(symbol@ %a)@]" Symbol.print symbol)
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map
  module Lmap = Lmap.Make (T)

  let set_of_code_id_set code_ids =
    (* CR-someday lmaurer: This is just an expensive identity. Should add
       something to [Patricia_tree] to let us translate. *)
    Code_id.Set.fold
      (fun code_id free_code_ids ->
        Set.add (create_code_id code_id) free_code_ids)
      code_ids Set.empty

  let set_of_symbol_set symbols =
    Symbol.Set.fold
      (fun sym free_syms -> Set.add (create_symbol sym) free_syms)
      symbols Set.empty
end

let initialise () =
  Const.initialise ();
  Variable.initialise ();
  Symbol.initialise ();
  Simple.initialise ();
  Code_id.initialise ()

let reset () = initialise ()
