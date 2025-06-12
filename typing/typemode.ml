open Location
open Mode
open Jkind_axis

(* CR zqian: kind modifier can be either a modaity or externality/nullability.
   I.e., mode-like modifiers are just modalities and should be represented as
   such. Therefore, [transl_modalities] (not dealing with
   externality/nullability) will stay in this file, while [transl_modifiers]
   should go into [typekind.ml] and calls [transl_modalities]. *)

type modal = private |

type maybe_nonmodal = private |

type 'm annot_type =
  | Modifier : maybe_nonmodal annot_type
  | Mode : modal annot_type
  | Modality : modal annot_type

type error =
  | Duplicated_axis : _ Axis.t -> error
  | Unrecognized_modifier : _ annot_type * string -> error

exception Error of Location.t * error

module Axis_pair = struct
  type 'm t =
    | Modal_axis_pair : ('a, _, _) Mode.Alloc.Axis.t * 'a -> modal t
    | Any_axis_pair : 'a Axis.t * 'a -> maybe_nonmodal t
    | Everything_but_nullability : maybe_nonmodal t

  let of_string s =
    let open Mode in
    match s with
    | "local" -> Any_axis_pair (Modal (Comonadic Areality), Locality.Const.Local)
    | "global" ->
      Any_axis_pair (Modal (Comonadic Areality), Locality.Const.Global)
    | "unique" ->
      Any_axis_pair (Modal (Monadic Uniqueness), Uniqueness.Const.Unique)
    | "aliased" ->
      Any_axis_pair (Modal (Monadic Uniqueness), Uniqueness.Const.Aliased)
    | "once" -> Any_axis_pair (Modal (Comonadic Linearity), Linearity.Const.Once)
    | "many" -> Any_axis_pair (Modal (Comonadic Linearity), Linearity.Const.Many)
    | "nonportable" ->
      Any_axis_pair
        (Modal (Comonadic Portability), Portability.Const.Nonportable)
    | "portable" ->
      Any_axis_pair (Modal (Comonadic Portability), Portability.Const.Portable)
    | "contended" ->
      Any_axis_pair (Modal (Monadic Contention), Contention.Const.Contended)
    | "shared" ->
      Any_axis_pair (Modal (Monadic Contention), Contention.Const.Shared)
    | "uncontended" ->
      Any_axis_pair (Modal (Monadic Contention), Contention.Const.Uncontended)
    | "maybe_null" ->
      Any_axis_pair (Nonmodal Nullability, Nullability.Maybe_null)
    | "non_null" -> Any_axis_pair (Nonmodal Nullability, Nullability.Non_null)
    | "internal" -> Any_axis_pair (Nonmodal Externality, Externality.Internal)
    | "external64" ->
      Any_axis_pair (Nonmodal Externality, Externality.External64)
    | "external_" -> Any_axis_pair (Nonmodal Externality, Externality.External)
    | "yielding" ->
      Any_axis_pair (Modal (Comonadic Yielding), Yielding.Const.Yielding)
    | "unyielding" ->
      Any_axis_pair (Modal (Comonadic Yielding), Yielding.Const.Unyielding)
    | "stateless" ->
      Any_axis_pair
        (Modal (Comonadic Statefulness), Statefulness.Const.Stateless)
    | "observing" ->
      Any_axis_pair
        (Modal (Comonadic Statefulness), Statefulness.Const.Observing)
    | "stateful" ->
      Any_axis_pair (Modal (Comonadic Statefulness), Statefulness.Const.Stateful)
    | "immutable" ->
      Any_axis_pair (Modal (Monadic Visibility), Visibility.Const.Immutable)
    | "read" -> Any_axis_pair (Modal (Monadic Visibility), Visibility.Const.Read)
    | "read_write" ->
      Any_axis_pair (Modal (Monadic Visibility), Visibility.Const.Read_write)
    | "maybe_separable" ->
      Any_axis_pair (Nonmodal Separability, Separability.Maybe_separable)
    | "separable" ->
      Any_axis_pair (Nonmodal Separability, Separability.Separable)
    | "non_float" ->
      Any_axis_pair (Nonmodal Separability, Separability.Non_float)
    | "everything" -> Everything_but_nullability
    | _ -> raise Not_found
end

let transl_annot (type m) ~(annot_type : m annot_type) ~required_mode_maturity
    annot : m Axis_pair.t Location.loc =
  Option.iter
    (fun maturity ->
      Language_extension.assert_enabled ~loc:annot.loc Mode maturity)
    required_mode_maturity;
  let pair : m Axis_pair.t =
    match Axis_pair.of_string annot.txt, annot_type with
    | Any_axis_pair (Nonmodal _, _), (Mode | Modality)
    | Everything_but_nullability, (Mode | Modality)
    | (exception Not_found) ->
      raise (Error (annot.loc, Unrecognized_modifier (annot_type, annot.txt)))
    | Any_axis_pair (Modal axis, mode), Mode -> Modal_axis_pair (axis, mode)
    | Any_axis_pair (Modal axis, mode), Modality -> Modal_axis_pair (axis, mode)
    | pair, Modifier -> pair
  in
  { txt = pair; loc = annot.loc }

let unpack_mode_annot { txt = Parsetree.Mode s; loc } = { txt = s; loc }

module Transled_modifiers = struct
  type t =
    { locality : Mode.Locality.Const.t Location.loc option;
      linearity : Mode.Linearity.Const.t Location.loc option;
      uniqueness : Mode.Uniqueness.Const.t Location.loc option;
      portability : Mode.Portability.Const.t Location.loc option;
      contention : Mode.Contention.Const.t Location.loc option;
      yielding : Mode.Yielding.Const.t Location.loc option;
      statefulness : Mode.Statefulness.Const.t Location.loc option;
      visibility : Mode.Visibility.Const.t Location.loc option;
      externality : Jkind_axis.Externality.t Location.loc option;
      nullability : Jkind_axis.Nullability.t Location.loc option;
      separability : Jkind_axis.Separability.t Location.loc option
    }

  let empty =
    { locality = None;
      linearity = None;
      uniqueness = None;
      portability = None;
      contention = None;
      yielding = None;
      statefulness = None;
      visibility = None;
      externality = None;
      nullability = None;
      separability = None
    }

  let get (type a) ~(axis : a Axis.t) (t : t) : a Location.loc option =
    match axis with
    | Modal (Comonadic Areality) -> t.locality
    | Modal (Comonadic Linearity) -> t.linearity
    | Modal (Monadic Uniqueness) -> t.uniqueness
    | Modal (Comonadic Portability) -> t.portability
    | Modal (Monadic Contention) -> t.contention
    | Modal (Comonadic Yielding) -> t.yielding
    | Modal (Comonadic Statefulness) -> t.statefulness
    | Modal (Monadic Visibility) -> t.visibility
    | Nonmodal Externality -> t.externality
    | Nonmodal Nullability -> t.nullability
    | Nonmodal Separability -> t.separability

  let set (type a) ~(axis : a Axis.t) (t : t) (value : a Location.loc option) :
      t =
    match axis with
    | Modal (Comonadic Areality) -> { t with locality = value }
    | Modal (Comonadic Linearity) -> { t with linearity = value }
    | Modal (Monadic Uniqueness) -> { t with uniqueness = value }
    | Modal (Comonadic Portability) -> { t with portability = value }
    | Modal (Monadic Contention) -> { t with contention = value }
    | Modal (Comonadic Yielding) -> { t with yielding = value }
    | Modal (Comonadic Statefulness) -> { t with statefulness = value }
    | Modal (Monadic Visibility) -> { t with visibility = value }
    | Nonmodal Externality -> { t with externality = value }
    | Nonmodal Nullability -> { t with nullability = value }
    | Nonmodal Separability -> { t with separability = value }
end

let transl_mod_bounds annots =
  let step bounds_so_far annot =
    match
      transl_annot ~annot_type:Modifier ~required_mode_maturity:None
      @@ unpack_mode_annot annot
    with
    | { txt = Any_axis_pair (type a) ((axis, mode) : a Axis.t * a); loc } ->
      let (module A) = Axis.get axis in
      let is_top = A.le A.max mode in
      if is_top
      then
        (* CR layouts v2.8: This warning is disabled for now because transl_type_decl
           results in 3 calls to transl_annots per user-written annotation. This results
           in the warning being reported 3 times. *)
        (* Location.prerr_warning new_raw.loc (Warnings.Mod_by_top new_raw.txt) *)
        ();
      let is_dup =
        Option.is_some (Transled_modifiers.get ~axis bounds_so_far)
      in
      if is_dup then raise (Error (annot.loc, Duplicated_axis axis));
      Transled_modifiers.set ~axis bounds_so_far (Some { txt = mode; loc })
    | { txt = Everything_but_nullability; loc } ->
      Transled_modifiers.
        { locality = Some { txt = Locality.Const.min; loc };
          linearity = Some { txt = Linearity.Const.min; loc };
          uniqueness = Some { txt = Uniqueness.Const_op.min; loc };
          portability = Some { txt = Portability.Const.min; loc };
          contention = Some { txt = Contention.Const_op.min; loc };
          yielding = Some { txt = Yielding.Const.min; loc };
          externality = Some { txt = Externality.min; loc };
          statefulness = Some { txt = Statefulness.Const.min; loc };
          visibility = Some { txt = Visibility.Const_op.min; loc };
          nullability =
            Transled_modifiers.get ~axis:(Nonmodal Nullability) bounds_so_far;
          separability =
            Transled_modifiers.get ~axis:(Nonmodal Separability) bounds_so_far
        }
  in
  let empty_modifiers = Transled_modifiers.empty in
  let modifiers = List.fold_left step empty_modifiers annots in
  (* Since [yielding] is the default mode in presence of [local],
     the [global] modifier must also apply [unyielding] unless specified. *)
  let modifiers =
    match
      ( Transled_modifiers.get ~axis:(Modal (Comonadic Yielding)) modifiers,
        Transled_modifiers.get ~axis:(Modal (Comonadic Areality)) modifiers )
    with
    | None, Some { txt = Locality.Const.Global; _ } ->
      Transled_modifiers.set ~axis:(Modal (Comonadic Yielding)) modifiers
        (Some { txt = Yielding.Const.Unyielding; loc = Location.none })
    | _, _ -> modifiers
  in
  (* Likewise, [immutable] => [contended], [read] => [shared]. *)
  let modifiers =
    match
      ( Transled_modifiers.get ~axis:(Modal (Monadic Contention)) modifiers,
        Transled_modifiers.get ~axis:(Modal (Monadic Visibility)) modifiers )
    with
    | None, Some { txt = Visibility.Const.Immutable; _ } ->
      Transled_modifiers.set ~axis:(Modal (Monadic Contention)) modifiers
        (Some { txt = Contention.Const.Contended; loc = Location.none })
    | None, Some { txt = Visibility.Const.Read; _ } ->
      Transled_modifiers.set ~axis:(Modal (Monadic Contention)) modifiers
        (Some { txt = Contention.Const.Shared; loc = Location.none })
    | _, _ -> modifiers
  in
  (* Likewise, [stateless] => [portable]. *)
  let modifiers =
    match
      ( Transled_modifiers.get ~axis:(Modal (Comonadic Portability)) modifiers,
        Transled_modifiers.get ~axis:(Modal (Comonadic Statefulness)) modifiers
      )
    with
    | None, Some { txt = Statefulness.Const.Stateless; _ } ->
      Transled_modifiers.set ~axis:(Modal (Comonadic Portability)) modifiers
        (Some { txt = Portability.Const.Portable; loc = Location.none })
    | _, _ -> modifiers
  in
  let open Types.Jkind_mod_bounds in
  let locality =
    Option.fold ~some:Location.get_txt ~none:Locality.max modifiers.locality
  in
  let linearity =
    Option.fold ~some:Location.get_txt ~none:Linearity.max modifiers.linearity
  in
  let uniqueness =
    Option.fold ~some:Location.get_txt ~none:Uniqueness.max modifiers.uniqueness
  in
  let portability =
    Option.fold ~some:Location.get_txt ~none:Portability.max
      modifiers.portability
  in
  let contention =
    Option.fold ~some:Location.get_txt ~none:Contention.max modifiers.contention
  in
  let yielding =
    Option.fold ~some:Location.get_txt ~none:Yielding.max modifiers.yielding
  in
  let statefulness =
    Option.fold ~some:Location.get_txt ~none:Statefulness.max
      modifiers.statefulness
  in
  let visibility =
    Option.fold ~some:Location.get_txt ~none:Visibility.max modifiers.visibility
  in
  let externality =
    Option.fold ~some:Location.get_txt ~none:Externality.max
      modifiers.externality
  in
  let nullability =
    Option.fold ~some:Location.get_txt ~none:Nullability.max
      modifiers.nullability
  in
  let separability =
    Option.fold ~some:Location.get_txt ~none:Separability.max
      modifiers.separability
  in
  create ~locality ~linearity ~uniqueness ~portability ~contention ~yielding
    ~statefulness ~visibility ~externality ~nullability ~separability

let default_mode_annots (annots : Alloc.Const.Option.t) =
  (* [yielding] has a different default depending on whether [areality]
     is [global] or [local]. *)
  let yielding =
    match annots.yielding, annots.areality with
    | (Some _ as y), _ | y, None -> y
    | None, Some Locality.Const.Global -> Some Yielding.Const.Unyielding
    | None, Some Locality.Const.Local -> Some Yielding.Const.Yielding
  in
  (* Likewise for [contention]. *)
  let contention =
    match annots.contention, annots.visibility with
    | (Some _ as c), _ | c, None -> c
    | None, Some Visibility.Const.Immutable -> Some Contention.Const.Contended
    | None, Some Visibility.Const.Read -> Some Contention.Const.Shared
    | None, Some Visibility.Const.Read_write ->
      Some Contention.Const.Uncontended
  in
  (* Likewise for [portability]. *)
  let portability =
    match annots.portability, annots.statefulness with
    | (Some _ as p), _ | p, None -> p
    | None, Some Statefulness.Const.Stateless -> Some Portability.Const.Portable
    | None, Some Statefulness.Const.(Observing | Stateful) ->
      Some Portability.Const.Nonportable
  in
  { annots with yielding; contention; portability }

let transl_mode_annots annots : Alloc.Const.Option.t =
  let step modifiers_so_far annot =
    let { txt =
            Modal_axis_pair (type a d0 d1)
              ((axis, mode) : (a, d0, d1) Mode.Alloc.Axis.t * a);
          loc
        } =
      transl_annot ~annot_type:Mode ~required_mode_maturity:(Some Stable)
      @@ unpack_mode_annot annot
    in
    let axis = Axis.Modal axis in
    if Option.is_some (Transled_modifiers.get ~axis modifiers_so_far)
    then raise (Error (annot.loc, Duplicated_axis axis));
    Transled_modifiers.set ~axis modifiers_so_far (Some { txt = mode; loc })
  in
  let empty_modifiers = Transled_modifiers.empty in
  let modes = List.fold_left step empty_modifiers annots in
  default_mode_annots
    { areality = Option.map get_txt modes.locality;
      linearity = Option.map get_txt modes.linearity;
      uniqueness = Option.map get_txt modes.uniqueness;
      portability = Option.map get_txt modes.portability;
      contention = Option.map get_txt modes.contention;
      yielding = Option.map get_txt modes.yielding;
      statefulness = Option.map get_txt modes.statefulness;
      visibility = Option.map get_txt modes.visibility
    }

let untransl_mode_annots (modes : Mode.Alloc.Const.Option.t) =
  let print_to_string_opt print a = Option.map (Format.asprintf "%a" print) a in
  (* Untranslate [areality] and [yielding]. *)
  let areality = print_to_string_opt Mode.Locality.Const.print modes.areality in
  let yielding =
    (* Since [yielding] has non-standard defaults, we special-case
       whether we want to print it here. *)
    match modes.yielding, modes.areality with
    | Some Yielding.Const.Yielding, Some Locality.Const.Local
    | Some Yielding.Const.Unyielding, Some Locality.Const.Global ->
      None
    | _, _ -> print_to_string_opt Mode.Yielding.Const.print modes.yielding
  in
  (* Untranslate [visibility] and [contention]. *)
  let visibility =
    print_to_string_opt Mode.Visibility.Const.print modes.visibility
  in
  let contention =
    match modes.visibility, modes.contention with
    | Some Visibility.Const.Immutable, Some Contention.Const.Contended
    | Some Visibility.Const.Read, Some Contention.Const.Shared
    | Some Visibility.Const.Read_write, Some Contention.Const.Uncontended ->
      None
    | _, _ -> print_to_string_opt Mode.Contention.Const.print modes.contention
  in
  (* Untranslate [statefulness] and [portability]. *)
  let statefulness =
    print_to_string_opt Mode.Statefulness.Const.print modes.statefulness
  in
  let portability =
    match modes.statefulness, modes.portability with
    | Some Statefulness.Const.Stateless, Some Portability.Const.Portable
    | ( Some Statefulness.Const.(Observing | Stateful),
        Some Portability.Const.Nonportable ) ->
      None
    | _, _ -> print_to_string_opt Mode.Portability.Const.print modes.portability
  in
  (* Untranslate remaining modes. *)
  let uniqueness =
    print_to_string_opt Mode.Uniqueness.Const.print modes.uniqueness
  in
  let linearity =
    print_to_string_opt Mode.Linearity.Const.print modes.linearity
  in
  List.filter_map
    (fun x ->
      Option.map (fun s -> { txt = Parsetree.Mode s; loc = Location.none }) x)
    [ areality;
      uniqueness;
      linearity;
      portability;
      contention;
      yielding;
      statefulness;
      visibility ]

let transl_modality ~maturity { txt = Parsetree.Modality modality; loc } =
  let axis_pair =
    transl_annot ~annot_type:Modality ~required_mode_maturity:(Some maturity)
      { txt = modality; loc }
  in
  let atom =
    match axis_pair.txt with
    | Modal_axis_pair (Comonadic Areality, mode) ->
      Modality.Atom
        (Comonadic Areality, Meet_with (Const.locality_as_regionality mode))
    | Modal_axis_pair (Comonadic Linearity, mode) ->
      Modality.Atom (Comonadic Linearity, Meet_with mode)
    | Modal_axis_pair (Comonadic Portability, mode) ->
      Modality.Atom (Comonadic Portability, Meet_with mode)
    | Modal_axis_pair (Monadic Uniqueness, mode) ->
      Modality.Atom (Monadic Uniqueness, Join_with mode)
    | Modal_axis_pair (Monadic Contention, mode) ->
      Modality.Atom (Monadic Contention, Join_with mode)
    | Modal_axis_pair (Comonadic Yielding, mode) ->
      Modality.Atom (Comonadic Yielding, Meet_with mode)
    | Modal_axis_pair (Comonadic Statefulness, mode) ->
      Modality.Atom (Comonadic Statefulness, Meet_with mode)
    | Modal_axis_pair (Monadic Visibility, mode) ->
      Modality.Atom (Monadic Visibility, Join_with mode)
  in
  atom, loc

let untransl_modality (a : Modality.t) : Parsetree.modality loc =
  let s =
    match a with
    | Atom (Comonadic Areality, Meet_with Regionality.Const.Global) -> "global"
    | Atom (Comonadic Areality, Meet_with Regionality.Const.Local) -> "local"
    | Atom (Comonadic Linearity, Meet_with Linearity.Const.Many) -> "many"
    | Atom (Comonadic Linearity, Meet_with Linearity.Const.Once) -> "once"
    | Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Aliased) -> "aliased"
    | Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Unique) -> "unique"
    | Atom (Comonadic Portability, Meet_with Portability.Const.Portable) ->
      "portable"
    | Atom (Comonadic Portability, Meet_with Portability.Const.Nonportable) ->
      "nonportable"
    | Atom (Monadic Contention, Join_with Contention.Const.Contended) ->
      "contended"
    | Atom (Monadic Contention, Join_with Contention.Const.Shared) -> "shared"
    | Atom (Monadic Contention, Join_with Contention.Const.Uncontended) ->
      "uncontended"
    | Atom (Comonadic Yielding, Meet_with Yielding.Const.Yielding) -> "yielding"
    | Atom (Comonadic Yielding, Meet_with Yielding.Const.Unyielding) ->
      "unyielding"
    | Atom (Comonadic Statefulness, Meet_with Statefulness.Const.Stateless) ->
      "stateless"
    | Atom (Comonadic Statefulness, Meet_with Statefulness.Const.Observing) ->
      "observing"
    | Atom (Comonadic Statefulness, Meet_with Statefulness.Const.Stateful) ->
      "stateful"
    | Atom (Monadic Visibility, Join_with Visibility.Const.Immutable) ->
      "immutable"
    | Atom (Monadic Visibility, Join_with Visibility.Const.Read) -> "read"
    | Atom (Monadic Visibility, Join_with Visibility.Const.Read_write) ->
      "read_write"
    | _ -> failwith "BUG: impossible modality atom"
  in
  { txt = Modality s; loc = Location.none }

(* For now, mutable implies legacy modalities for both comonadic axes and
   monadic axes. In the future, implications on the comonadic axes will be
   removed. The implications on the monadic axes will stay. Implied modalities
   can be overriden. *)
(* CR zqian: decouple mutable and comonadic modalities *)
let mutable_implied_modalities (mut : Types.mutability) =
  let comonadic : Modality.t list =
    [ Atom (Comonadic Areality, Meet_with Regionality.Const.legacy);
      Atom (Comonadic Linearity, Meet_with Linearity.Const.legacy);
      Atom (Comonadic Portability, Meet_with Portability.Const.legacy);
      Atom (Comonadic Yielding, Meet_with Yielding.Const.legacy);
      Atom (Comonadic Statefulness, Meet_with Statefulness.Const.legacy) ]
  in
  let monadic : Modality.t list =
    [ Atom (Monadic Uniqueness, Join_with Uniqueness.Const.legacy);
      Atom (Monadic Contention, Join_with Contention.Const.legacy);
      Atom (Monadic Visibility, Join_with Visibility.Const.legacy) ]
  in
  match mut with Immutable -> [] | Mutable _ -> monadic @ comonadic

let mutable_implied_modalities (mut : Types.mutability) =
  let l = mutable_implied_modalities mut in
  List.fold_left
    (fun t (Modality.Atom (ax, a)) -> Modality.Value.Const.set ax a t)
    Modality.Value.Const.id l

(* Since [yielding] is the default mode in presence of [local],
   the [global] modality must also apply [unyielding] unless specified.

   Similarly for [visibility]/[contention] and [statefulness]/[portability]. *)
let implied_modalities (Atom (ax, a) : Modality.t) : Modality.t list =
  match ax, a with
  | Comonadic Areality, Meet_with a ->
    let b : Yielding.Const.t =
      match a with
      | Global -> Unyielding
      | Local -> Yielding
      | Regional -> assert false
    in
    [Atom (Comonadic Yielding, Meet_with b)]
  | Monadic Visibility, Join_with a ->
    let b : Contention.Const.t =
      match a with
      | Immutable -> Contended
      | Read -> Shared
      | Read_write -> Uncontended
    in
    [Atom (Monadic Contention, Join_with b)]
  | Comonadic Statefulness, Meet_with a ->
    let b : Portability.Const.t =
      match a with Stateless -> Portable | Stateful | Observing -> Nonportable
    in
    [Atom (Comonadic Portability, Meet_with b)]
  | _ -> []

let least_modalities_implying mut (t : Modality.Value.Const.t) =
  let baseline = mutable_implied_modalities mut in
  let annotated = Modality.Value.Const.(diff baseline t) in
  let implied = List.concat_map implied_modalities annotated in
  let exclude_implied =
    List.filter (fun x -> not @@ List.mem x implied) annotated
  in
  let overridden =
    List.filter_map
      (fun (Modality.Atom (ax, m_implied)) ->
        let m_projected = Modality.Value.Const.proj ax t in
        if m_projected <> m_implied
        then Some (Modality.Atom (ax, m_projected))
        else None)
      implied
  in
  exclude_implied @ overridden

let sort_dedup_modalities ~warn l =
  let compare (Modality.Atom (ax0, _), _) (Modality.Atom (ax1, _), _) =
    Value.Axis.compare ax0 ax1
  in
  let dedup ~on_dup =
    let rec loop x = function
      | [] -> [x]
      | y :: xs ->
        if compare x y = 0
        then (
          on_dup x y;
          loop y xs)
        else x :: loop y xs
    in
    function [] -> [] | x :: xs -> loop x xs
  in
  let on_dup (Modality.Atom (ax0, _), loc0) (a1, _) =
    if warn
    then
      let axis = Format.asprintf "%a" Value.Axis.print ax0 in
      let { txt = Modality overriden_by; _ } = untransl_modality a1 in
      Location.prerr_warning loc0
        (Warnings.Modal_axis_specified_twice { axis; overriden_by })
  in
  l |> List.stable_sort compare |> dedup ~on_dup |> List.map fst

let transl_modalities ~maturity mut modalities =
  let mut_modalities = mutable_implied_modalities mut in
  let modalities = List.map (transl_modality ~maturity) modalities in
  (* axes listed in the order of implication. *)
  let modalities = sort_dedup_modalities ~warn:true modalities in
  let open Modality in
  (* - mut_modalities is applied before explicit modalities.
     - explicit modalities can override mut_modalities.
     - For the same axis, later modalities overrides earlier modalities. *)
  List.fold_left
    (fun m (Atom (ax, a) as t) ->
      let m = Value.Const.set ax a m in
      List.fold_left
        (fun m (Atom (ax, a)) -> Value.Const.set ax a m)
        m (implied_modalities t))
    mut_modalities modalities

let untransl_modalities mut t =
  t
  |> least_modalities_implying mut
  |> List.map (fun x -> x, Location.none)
  |> sort_dedup_modalities ~warn:false
  |> List.map untransl_modality

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

(* Error reporting *)

let report_error ppf =
  let open Format in
  function
  | Duplicated_axis axis ->
    fprintf ppf "The %s axis has already been specified." (Axis.name axis)
  | Unrecognized_modifier (annot_type, modifier) ->
    let annot_type_str =
      match annot_type with
      | Modifier -> "modifier"
      | Mode -> "mode"
      | Modality -> "modality"
    in
    fprintf ppf "Unrecognized %s %s." annot_type_str modifier

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
